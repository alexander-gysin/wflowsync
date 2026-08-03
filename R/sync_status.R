#' Check Project Status and Sync
#'
#' Opens an interactive Shiny Gadget allowing users to select files to publish
#' via Workflowr or commit/push via Git. Automatically checks for unpulled changes
#' and verifies final synchronization status. Features a 2-step review process.
#'
#' @export
sync_status <- function() {

  if (!requireNamespace("shiny", quietly = TRUE) || !requireNamespace("miniUI", quietly = TRUE)) {
    cli::cli_abort("Packages {.pkg shiny} and {.pkg miniUI} are required. Run {.code install.packages(c('shiny', 'miniUI'))}")
  }

  cli::cli_h1("Analyzing Project Status...")

  col_green <- "#28a745"
  col_yellow <- "#f39c12"
  col_red <- "#dc3545"

  # 1. EVALUATE WORKFLOWR ------------------------------------------------
  has_wflow <- file.exists("_workflowr.yml") || file.exists("analysis/_site.yml")
  wflow_files <- character(0)

  if (!has_wflow) {
    wf_color <- col_red
    wf_title <- "Workflowr: Not Configured"
    wf_state <- "missing"
  } else {
    ws <- tryCatch(workflowr::wflow_status(), error = function(e) NULL)
    if (!is.null(ws)) {
      wflow_files <- rownames(ws$status)[ws$status$unpublished | ws$status$modified | ws$status$scratch]
    }

    if (length(wflow_files) > 0) {
      wf_color <- col_yellow
      wf_title <- "Workflowr: Action Needed"
      wf_state <- "desync"
    } else {
      wf_color <- col_green
      wf_title <- "Workflowr: Synced"
      wf_state <- "synced"
    }
  }

  # 2. EVALUATE RENV ------------------------------------------------
  if (!file.exists("renv.lock")) {
    renv_color <- col_red
    renv_title <- "renv: Not Configured"
    renv_state <- "missing"
  } else {
    suppressMessages(suppressWarnings({
      rs <- tryCatch(renv::status(), error = function(e) list())
    }))
    if (!is.null(rs$synchronized) && isFALSE(rs$synchronized)) {
      renv_color <- col_yellow
      renv_title <- "renv: Out of Sync"
      renv_state <- "desync"
    } else {
      renv_color <- col_green
      renv_title <- "renv: Up to Date"
      renv_state <- "synced"
    }
  }

  # 3. EVALUATE GIT & PAT ------------------------------------------------
  pat_state <- "missing"
  pat_text <- "\U0000274C PAT not configured or missing!"
  pat_color <- col_red

  if (file.exists("~/.wflowsync_meta.rds")) {
    meta <- tryCatch(readRDS("~/.wflowsync_meta.rds"), error = function(e) NULL)
    if (!is.null(meta) && !is.null(meta$expiry)) {
      days_left <- as.numeric(as.Date(meta$expiry) - Sys.Date())
      if (days_left < 0) {
        pat_state <- "expired"
        pat_text <- sprintf("\U0000274C PAT Expired %d days ago!", abs(days_left))
        pat_color <- col_red
      } else if (days_left <= 7) {
        pat_state <- "warning"
        pat_text <- sprintf("\U000026A0\U0000FE0F PAT expires in %d days!", days_left)
        pat_color <- col_yellow
      } else {
        pat_state <- "valid"
        pat_text <- sprintf("\U00002705 PAT valid for %d days", days_left)
        pat_color <- col_green
      }
    }
  }

  gs <- tryCatch(gert::git_status(), error = function(e) NULL)
  git_files_only <- character(0)
  if (!is.null(gs)) {
    git_files_only <- setdiff(gs$file, wflow_files)
  }

  ab <- tryCatch(gert::git_ahead_behind(), error = function(e) list(ahead = 0, behind = 0))
  n_ahead <- if (!is.null(ab$ahead)) ab$ahead else 0
  n_behind <- if (!is.null(ab$behind)) ab$behind else 0

  if (pat_state %in% c("missing", "expired")) {
    git_color <- col_red
    git_title <- "Git & GitHub: Error"
  } else if (pat_state == "warning" || length(git_files_only) > 0 || n_behind > 0 || n_ahead > 0) {
    git_color <- col_yellow
    git_title <- "Git & GitHub: Action Needed"
  } else {
    git_color <- col_green
    git_title <- "Git & GitHub: Clean"
  }

  # UI HELPER (Restored borders, tweaked padding for pane) -----------
  create_card <- function(title, color, body_ui) {
    shiny::tags$div(
      style = sprintf("border: 2px solid %s; border-radius: 8px; overflow: hidden; margin-bottom: 15px; background: white;", color),
      shiny::tags$div(
        style = sprintf("background-color: %s; color: white; padding: 8px; font-weight: bold; text-align: center;", color),
        title
      ),
      shiny::tags$div(style = "padding: 10px; overflow-y: auto; max-height: 400px;", body_ui)
    )
  }

  # SHINY UI ------------------------------------------------
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("wflowsync: Project Dashboard", right = NULL),
    miniUI::miniContentPanel(
      shiny::uiOutput("dynamic_ui")
    ),
    shiny::tags$div(
      style = "padding: 10px 15px; border-top: 1px solid #ddd; background: #f8f9fa; text-align: right;",
      shiny::uiOutput("dynamic_buttons")
    )
  )

  # SHINY SERVER ------------------------------------------------
  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(step = 1)

    output$dynamic_ui <- shiny::renderUI({
      if (rv$step == 1) {
        # --- STEP 1: TABBED DASHBOARD ---
        shiny::tabsetPanel(
          type = "tabs",

          shiny::tabPanel("Workflowr", icon = shiny::icon("circle", style = sprintf("color: %s;", wf_color)),
                          shiny::tags$div(style = "margin-top: 15px;",
                                          create_card(wf_title, wf_color,
                                                      if (wf_state == "missing") {
                                                        shiny::p(style = "color: #dc3545; font-weight: bold;", "Run workflowr::wflow_start() to configure.")
                                                      } else if (length(wflow_files) == 0) {
                                                        shiny::p(style = "color: grey; text-align: center; margin-top: 20px;", "All .Rmd files are compiled and synced.")
                                                      } else {
                                                        shiny::tagList(
                                                          shiny::p("Files to knit & publish:"),
                                                          shiny::checkboxGroupInput("wflow_cb", label = NULL, choices = wflow_files, selected = wflow_files),
                                                          shiny::textInput("wflow_msg", "Publish Message:", placeholder = "Update analysis")
                                                        )
                                                      }
                                          )
                          )
          ),

          shiny::tabPanel("Git", icon = shiny::icon("circle", style = sprintf("color: %s;", git_color)),
                          shiny::tags$div(style = "margin-top: 15px;",
                                          create_card(git_title, git_color,
                                                      shiny::tagList(
                                                        shiny::tags$div(style = sprintf("font-weight: bold; margin-bottom: 10px; border-bottom: 1px solid #ddd; padding-bottom: 10px; color: %s;", pat_color), pat_text),
                                                        if (n_behind > 0) {
                                                          shiny::tagList(
                                                            shiny::p(style = "color: #dc3545; font-weight: bold;", sprintf("\U000026A0\U0000FE0F You are %d commits behind GitHub!", n_behind)),
                                                            shiny::checkboxInput("do_pull", shiny::tags$b("Pull latest changes"), value = TRUE),
                                                            shiny::hr()
                                                          )
                                                        },
                                                        if (n_ahead > 0) {
                                                          shiny::p(style = "color: #f39c12; font-weight: bold;", sprintf("\U00002191 %d local commits ready to push.", n_ahead))
                                                        },
                                                        if (length(git_files_only) == 0) {
                                                          shiny::p(style = "color: grey; text-align: center; margin-top: 10px;", "No other untracked or modified files.")
                                                        } else {
                                                          shiny::tagList(
                                                            shiny::p("Standard files to commit:"),
                                                            shiny::checkboxGroupInput("git_cb", label = NULL, choices = git_files_only, selected = git_files_only),
                                                            shiny::textInput("git_msg", "Commit Message:", placeholder = "Update project files")
                                                          )
                                                        }
                                                      )
                                          )
                          )
          ),

          shiny::tabPanel("renv", icon = shiny::icon("circle", style = sprintf("color: %s;", renv_color)),
                          shiny::tags$div(style = "margin-top: 15px;",
                                          create_card(renv_title, renv_color,
                                                      if (renv_state == "missing") {
                                                        shiny::p(style = "color: #dc3545; font-weight: bold;", "Run renv::init() to set up package tracking.")
                                                      } else if (renv_state == "desync") {
                                                        shiny::tagList(
                                                          shiny::p("Your local R library has unsaved package changes."),
                                                          shiny::checkboxInput("do_snapshot", shiny::tags$b("Run renv::snapshot()"), value = TRUE)
                                                        )
                                                      } else {
                                                        shiny::p(style = "color: grey; text-align: center; margin-top: 20px;", "Packages are perfectly tracked and synced.")
                                                      }
                                          )
                          )
          )
        )
      } else {
        # --- STEP 2: REVIEW & CONFIRM (Stacked vertically for narrow pane) ---
        shiny::tagList(
          shiny::p(style = "margin-bottom: 15px;", "Review your selections below:"),

          create_card("1. Publish Review", "#17a2b8",
                      if (length(input$wflow_cb) > 0) {
                        shiny::tagList(
                          shiny::p(shiny::tags$b("Message: "), if (is.null(input$wflow_msg) || input$wflow_msg == "") "Update analysis" else input$wflow_msg),
                          shiny::p(shiny::tags$b("Files to knit & publish:")),
                          shiny::tags$ul(lapply(input$wflow_cb, shiny::tags$li))
                        )
                      } else {
                        shiny::p(style = "color: grey;", "No .Rmd files selected for publishing.")
                      }
          ),

          create_card("2. Commit Review", "#17a2b8",
                      if (length(input$git_cb) > 0 || isTRUE(input$do_snapshot) || n_ahead > 0 || isTRUE(input$do_pull)) {
                        shiny::tagList(
                          if (isTRUE(input$do_pull)) shiny::p(style = "color: #28a745; font-weight: bold;", "\U00002193 Will pull changes from GitHub first."),
                          if (n_ahead > 0) shiny::p(style = "color: #f39c12; font-weight: bold;", sprintf("\U00002191 Will push %d existing commits.", n_ahead)),
                          if (length(input$git_cb) > 0 || isTRUE(input$do_snapshot)) {
                            shiny::tagList(
                              shiny::p(shiny::tags$b("Message: "), if (is.null(input$git_msg) || input$git_msg == "") "Update project files" else input$git_msg),
                              shiny::p(shiny::tags$b("Files to commit:")),
                              shiny::tags$ul(
                                if (isTRUE(input$do_snapshot)) shiny::tags$li(shiny::tags$b("renv.lock")),
                                lapply(input$git_cb, shiny::tags$li)
                              )
                            )
                          }
                        )
                      } else {
                        shiny::p(style = "color: grey;", "No standard files selected for commit.")
                      }
          ),

          create_card("3. Snapshot Review", "#17a2b8",
                      if (isTRUE(input$do_snapshot)) {
                        shiny::p("\U00002705 Will run ", shiny::tags$code("renv::snapshot()"), " and commit the updated lockfile.")
                      } else {
                        shiny::p(style = "color: grey;", "No renv updates selected.")
                      }
          )
        )
      }
    })

    output$dynamic_buttons <- shiny::renderUI({
      if (rv$step == 1) {
        shiny::actionButton("btn_review", "Review Sync", class = "btn-primary", style = "font-weight: bold; padding: 8px 20px;")
      } else {
        shiny::tagList(
          shiny::actionButton("btn_back", "Back to Edit", class = "btn-secondary", style = "margin-right: 10px;"),
          shiny::actionButton("btn_execute", "Confirm & Execute", class = "btn-success", style = "font-weight: bold; padding: 8px 20px;")
        )
      }
    })

    shiny::observeEvent(input$btn_review, { rv$step <- 2 })
    shiny::observeEvent(input$btn_back, { rv$step <- 1 })

    shiny::observeEvent(input$btn_execute, {
      shiny::stopApp(list(
        action = "execute",
        do_pull = isTRUE(input$do_pull),
        do_snapshot = isTRUE(input$do_snapshot),
        wflow_files = input$wflow_cb,
        wflow_msg = input$wflow_msg,
        git_files = input$git_cb,
        git_msg = input$git_msg,
        n_ahead = n_ahead
      ))
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  # Run the gadget in the Viewer pane (bulletproof on RStudio Server)
  res <- shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 600))

  # EXECUTE SYNC (Clean CLI Output) -------------------------------------
  if (!is.null(res) && res$action == "execute") {

    # 1. Give RStudio Server time to fully close the Gadget UI
    Sys.sleep(1)
    cli::cli_h2("Executing Sync Operations")
    Sys.setenv(GIT_TERMINAL_PROMPT = "0")

    # 0. PULL
    if (res$do_pull) {
      cli::cli_alert_info("Pulling latest changes...")
      tryCatch({
        gert::git_pull(verbose = FALSE)
        cli::cli_alert_success("Pull complete.")
      }, error = function(e) cli::cli_alert_danger("Pull failed: {e$message}"))
    }

    # 1. RENV
    if (res$do_snapshot) {
      cli::cli_alert_info("Running renv::snapshot()...")
      renv::snapshot(prompt = FALSE)
      cli::cli_alert_success("Lockfile updated.")
      if (!("renv.lock" %in% res$git_files)) res$git_files <- c(res$git_files, "renv.lock")
    }

    # 2. WORKFLOWR
    if (length(res$wflow_files) > 0) {
      cli::cli_alert_info("Publishing Workflowr files...")
      wf_msg <- if (!is.null(res$wflow_msg) && res$wflow_msg != "") res$wflow_msg else "Update analysis"
      workflowr::wflow_publish(res$wflow_files, message = wf_msg)
      cli::cli_alert_success("Publish complete.")
    }

    # 3. NATIVE GIT ADD & COMMIT
    if (length(res$git_files) > 0) {
      cli::cli_alert_info("Staging {length(res$git_files)} file(s)...")
      tryCatch({
        gert::git_add(res$git_files)
      }, error = function(e) cli::cli_alert_danger("Staging error: {e$message}"))

      cli::cli_alert_info("Committing files...")
      c_msg <- if (!is.null(res$git_msg) && res$git_msg != "") res$git_msg else "Update project files"
      tryCatch({
        gert::git_commit(c_msg)
        cli::cli_alert_success("Commit successful.")
      }, error = function(e) cli::cli_alert_danger("Commit error: {e$message}"))
    }

    # 4. NATIVE GIT PUSH
    cli::cli_alert_info("Pushing to GitHub...")
    tryCatch({
      gert::git_push(verbose = FALSE)
      cli::cli_alert_success("Push successful.")
    }, error = function(e) cli::cli_alert_danger("Push failed: {e$message}"))

    # 5. FINAL VERIFICATION
    final_ab <- tryCatch(gert::git_ahead_behind(), error = function(e) list(ahead = 1, behind = 1))
    if (final_ab$ahead == 0 && final_ab$behind == 0) {
      cli::cli_alert_success("Project perfectly synced!")
    }
  }
}

# Helper function
"%||%" <- function(a, b) if (!is.null(a) && a != "") a else b
