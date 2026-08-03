#' Check Project Status and Sync
#'
#' Opens an interactive Shiny Gadget allowing users to select files to publish
#' via Workflowr or commit/push via Git, while displaying PAT and renv status
#' in a visual 3-column dashboard. Features a 2-step review process.
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

  if (pat_state %in% c("missing", "expired")) {
    git_color <- col_red
    git_title <- "Git & GitHub: Error"
  } else if (pat_state == "warning" || length(git_files_only) > 0) {
    git_color <- col_yellow
    git_title <- "Git & GitHub: Action Needed"
  } else {
    git_color <- col_green
    git_title <- "Git & GitHub: Clean"
  }

  # UI HELPER ------------------------------------------------
  create_card <- function(title, color, body_ui) {
    shiny::tags$div(
      style = sprintf("border: 2px solid %s; border-radius: 8px; overflow: hidden; height: 100%%; margin-bottom: 15px; background: white;", color),
      shiny::tags$div(
        style = sprintf("background-color: %s; color: white; padding: 10px; font-weight: bold; text-align: center;", color),
        title
      ),
      shiny::tags$div(style = "padding: 15px; overflow-y: auto; max-height: 500px;", body_ui)
    )
  }

  # SHINY UI ------------------------------------------------
  ui <- miniUI::miniPage(
    # Setting right = NULL removes the confusing "Done" button, leaving only "Cancel" on the left
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

    # Track which screen the user is on (1 = Dashboard, 2 = Review)
    rv <- shiny::reactiveValues(step = 1)

    # Render Main View or Review View
    output$dynamic_ui <- shiny::renderUI({
      if (rv$step == 1) {
        # --- STEP 1: DASHBOARD ---
        shiny::fluidRow(
          # Column 1: Workflowr
          shiny::column(4, create_card(wf_title, wf_color,
                                       if (wf_state == "missing") {
                                         shiny::p(style = "color: #dc3545; font-weight: bold;", "Run workflowr::wflow_start() to configure.")
                                       } else if (length(wflow_files) == 0) {
                                         shiny::p(style = "color: grey; text-align: center; margin-top: 20px;", "All .Rmd files are compiled and synced.")
                                       } else {
                                         shiny::tagList(
                                           shiny::p("The following files need to be knitted and published:"),
                                           shiny::checkboxGroupInput("wflow_cb", label = NULL, choices = wflow_files, selected = wflow_files),
                                           shiny::textInput("wflow_msg", "Publish Message:", value = "Update analysis")
                                         )
                                       }
          )),

          # Column 2: Git & GitHub
          shiny::column(4, create_card(git_title, git_color,
                                       shiny::tagList(
                                         shiny::tags$div(style = sprintf("font-weight: bold; margin-bottom: 15px; border-bottom: 1px solid #ddd; padding-bottom: 10px; color: %s;", pat_color), pat_text),
                                         if (length(git_files_only) == 0) {
                                           shiny::p(style = "color: grey; text-align: center; margin-top: 10px;", "No other untracked or modified files.")
                                         } else {
                                           shiny::tagList(
                                             shiny::p("Standard files to commit & push:"),
                                             shiny::checkboxGroupInput("git_cb", label = NULL, choices = git_files_only, selected = git_files_only),
                                             shiny::textInput("git_msg", "Commit Message:", value = "Update project files")
                                           )
                                         }
                                       )
          )),

          # Column 3: renv
          shiny::column(4, create_card(renv_title, renv_color,
                                       if (renv_state == "missing") {
                                         shiny::p(style = "color: #dc3545; font-weight: bold;", "Run renv::init() to set up package tracking.")
                                       } else if (renv_state == "desync") {
                                         shiny::tagList(
                                           shiny::p("Your local R library does not match renv.lock. You have unsaved package changes."),
                                           shiny::checkboxInput("do_snapshot", shiny::tags$b("Include renv::snapshot() in this sync"), value = TRUE)
                                         )
                                       } else {
                                         shiny::p(style = "color: grey; text-align: center; margin-top: 20px;", "Packages are perfectly tracked and synced.")
                                       }
          ))
        )
      } else {
        # --- STEP 2: REVIEW & CONFIRM ---
        shiny::fluidRow(
          shiny::column(4, create_card("1. Snapshot Review", "#17a2b8",
                                       if (isTRUE(input$do_snapshot)) {
                                         shiny::p("\U00002705 Will run ", shiny::tags$code("renv::snapshot()"), " and commit the updated lockfile.")
                                       } else {
                                         shiny::p(style = "color: grey;", "No renv updates selected.")
                                       }
          )),
          shiny::column(4, create_card("2. Publish Review", "#17a2b8",
                                       if (length(input$wflow_cb) > 0) {
                                         shiny::tagList(
                                           shiny::p(shiny::tags$b("Message: "), input$wflow_msg),
                                           shiny::p(shiny::tags$b("Files to knit & publish:")),
                                           shiny::tags$ul(lapply(input$wflow_cb, shiny::tags$li))
                                         )
                                       } else {
                                         shiny::p(style = "color: grey;", "No .Rmd files selected for publishing.")
                                       }
          )),
          shiny::column(4, create_card("3. Commit Review", "#17a2b8",
                                       if (length(input$git_cb) > 0 || isTRUE(input$do_snapshot)) {
                                         shiny::tagList(
                                           shiny::p(shiny::tags$b("Message: "), input$git_msg),
                                           shiny::p(shiny::tags$b("Files to commit & push:")),
                                           shiny::tags$ul(
                                             if (isTRUE(input$do_snapshot)) shiny::tags$li(shiny::tags$b("renv.lock")),
                                             lapply(input$git_cb, shiny::tags$li)
                                           )
                                         )
                                       } else {
                                         shiny::p(style = "color: grey;", "No standard files selected for commit.")
                                       }
          ))
        )
      }
    })

    # Render Bottom Buttons
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

    # Navigation logic
    shiny::observeEvent(input$btn_review, { rv$step <- 2 })
    shiny::observeEvent(input$btn_back, { rv$step <- 1 })

    # Execute logic
    shiny::observeEvent(input$btn_execute, {
      shiny::stopApp(list(
        action = "execute",
        do_snapshot = isTRUE(input$do_snapshot),
        wflow_files = input$wflow_cb,
        wflow_msg = input$wflow_msg,
        git_files = input$git_cb,
        git_msg = input$git_msg
      ))
    })

    # Cancel handling
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  # Launch Gadget (Updated dimensions)
  res <- shiny::runGadget(ui, server, viewer = shiny::dialogViewer("wflowsync", width = 1200, height = 750))

  # EXECUTE SYNC ------------------------------------------------
  if (!is.null(res) && res$action == "execute") {
    cli::cli_h2("Executing Sync Operations...")

    # 1. RENV SNAPSHOT
    if (res$do_snapshot) {
      cli::cli_alert_info("Running renv::snapshot()...")
      tryCatch({
        renv::snapshot(prompt = FALSE)
        cli::cli_alert_success("renv.lock updated.")

        # Automatically add renv.lock to the Git payload if not already present
        if (!("renv.lock" %in% res$git_files)) {
          res$git_files <- c(res$git_files, "renv.lock")
        }
        # Provide a fallback commit message if git_msg is empty/missing
        if (is.null(res$git_msg) || res$git_msg == "") {
          res$git_msg <- "Update renv.lock"
        }
      }, error = function(e) cli::cli_alert_danger("renv snapshot failed: {e$message}"))
    }

    # 2. WORKFLOWR PUBLISH
    if (length(res$wflow_files) > 0) {
      cli::cli_alert_info("Publishing via Workflowr...")
      tryCatch({
        workflowr::wflow_publish(res$wflow_files, message = res$wflow_msg)
        cli::cli_alert_success("Successfully built and pushed .Rmd files!")
      }, error = function(e) cli::cli_alert_danger("Workflowr publish failed: {e$message}"))
    }

    # 3. GIT COMMIT & PUSH
    if (length(res$git_files) > 0) {
      cli::cli_alert_info("Staging, committing, and pushing standard files...")
      tryCatch({
        gert::git_add(res$git_files)
        gert::git_commit(message = res$git_msg)
        gert::git_push()
        cli::cli_alert_success("Successfully pushed standard files to GitHub!")
      }, error = function(e) cli::cli_alert_danger("Git action failed: {e$message}"))
    }

    # Final state if nothing was selected
    if (!res$do_snapshot && length(res$wflow_files) == 0 && length(res$git_files) == 0) {
      cli::cli_alert_info("No actions selected. Project remains unchanged.")
    }

  } else {
    cli::cli_alert_info("Sync cancelled by user. No changes made.")
  }

  invisible(res)
}
