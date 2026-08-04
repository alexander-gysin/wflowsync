# Evaluates the entire project state once to be used by the UI
get_project_state <- function() {
  state <- list(
    col_green = "#28a745",
    col_yellow = "#f39c12",
    col_red = "#dc3545"
  )

  # 1. EVALUATE WORKFLOWR
  state$has_wflow <- file.exists("_workflowr.yml") || file.exists("analysis/_site.yml")
  state$wf_scratch <- character(0)
  state$wf_modified <- character(0)
  state$wf_unpub <- character(0)

  if (!state$has_wflow) {
    state$wf_color <- state$col_red
    state$wf_title <- "Workflowr: Action required"
    state$wf_state <- "missing"
    state$wflow_files <- character(0)
  } else {
    ws <- tryCatch(workflowr::wflow_status(), error = function(e) NULL)
    if (!is.null(ws) && !is.null(ws$status)) {
      state$wf_scratch <- rownames(ws$status)[ws$status$scratch]
      state$wf_modified <- rownames(ws$status)[ws$status$modified]
      state$wf_unpub <- rownames(ws$status)[ws$status$unpublished]
    }
    state$wflow_files <- c(state$wf_scratch, state$wf_modified, state$wf_unpub)

    if (length(state$wflow_files) > 0) {
      state$wf_color <- state$col_yellow
      state$wf_title <- "Workflowr: Action required"
      state$wf_state <- "desync"
    } else {
      state$wf_color <- state$col_green
      state$wf_title <- "Workflowr: No action required"
      state$wf_state <- "synced"
    }
  }

  # 2. EVALUATE RENV
  state$renv_out <- character(0)
  if (!file.exists("renv.lock")) {
    state$renv_color <- state$col_red
    state$renv_title <- "renv: Action required"
    state$renv_state <- "missing"
  } else {
    suppressMessages(suppressWarnings({
      state$renv_out <- capture.output(rs <- tryCatch(renv::status(), error = function(e) list()))
    }))
    if (!is.null(rs$synchronized) && isFALSE(rs$synchronized)) {
      state$renv_color <- state$col_yellow
      state$renv_title <- "renv: Action required"
      state$renv_state <- "desync"
    } else {
      state$renv_color <- state$col_green
      state$renv_title <- "renv: No action required"
      state$renv_state <- "synced"
    }
  }

  # 3. EVALUATE GIT & PAT
  state$pat_state <- "missing"
  state$pat_text <- "\U0000274C PAT not configured or missing!"
  state$pat_color <- state$col_red

  cfg_path <- get_config_path()
  if (file.exists(cfg_path)) {
    meta <- tryCatch(readRDS(cfg_path), error = function(e) NULL)
    if (!is.null(meta) && !is.null(meta$expiry)) {
      days_left <- as.numeric(as.Date(meta$expiry) - Sys.Date())
      if (days_left < 0) {
        state$pat_state <- "expired"
        state$pat_text <- sprintf("\U0000274C PAT Expired %d days ago!", abs(days_left))
        state$pat_color <- state$col_red
      } else if (days_left <= 7) {
        state$pat_state <- "warning"
        state$pat_text <- sprintf("\U000026A0\U0000FE0F PAT expires in %d days!", days_left)
        state$pat_color <- state$col_yellow
      } else {
        state$pat_state <- "valid"
        state$pat_text <- sprintf("\U00002705 PAT valid for %d days", days_left)
        state$pat_color <- state$col_green
      }
    }
  }

  state$git_files_only <- character(0)
  state$has_remote <- FALSE
  state$n_ahead <- 0
  state$n_behind <- 0

  # SAFELY CHECK IF IT IS A GIT REPO
  is_git_repo <- !is.null(tryCatch(gert::git_info(), error = function(e) NULL))

  if (is_git_repo) {
    gs <- tryCatch(gert::git_status(), error = function(e) NULL)
    if (!is.null(gs)) {
      state$git_files_only <- setdiff(gs$file, state$wflow_files)
    }
    ab <- tryCatch(gert::git_ahead_behind(), error = function(e) list(ahead = 0, behind = 0))
    if (!is.null(ab$ahead)) state$n_ahead <- ab$ahead
    if (!is.null(ab$behind)) state$n_behind <- ab$behind

    # SAFELY CHECK FOR REMOTE
    remotes <- tryCatch(gert::git_remote_list(), error = function(e) NULL)
    state$has_remote <- !is.null(remotes) && nrow(remotes) > 0
  }

  if (state$pat_state %in% c("missing", "expired") || !state$has_remote) {
    state$git_color <- state$col_red
    state$git_title <- "Git & GitHub: Action required"
  } else if (state$pat_state == "warning" || length(state$git_files_only) > 0 || state$n_behind > 0 || state$n_ahead > 0) {
    state$git_color <- state$col_yellow
    state$git_title <- "Git & GitHub: Action required"
  } else {
    state$git_color <- state$col_green
    state$git_title <- "Git & GitHub: No action required"
  }

  return(state)
}

status_ui <- function() {
  miniUI::miniPage(
    # CSS to hide the default tabset headers since we control them with our custom top-bar buttons
    shiny::tags$head(shiny::tags$style(".nav-tabs { display: none; }")),

    # Custom Unified Header
    shiny::tags$div(
      style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 15px; border-bottom: 1px solid #ddd; background: #f8f9fa;",
      shiny::uiOutput("header_left"),
      shiny::uiOutput("header_center"),
      shiny::uiOutput("header_right")
    ),

    # Content Area
    miniUI::miniContentPanel(
      shiny::uiOutput("dynamic_ui")
    )
  )
}

status_server <- function(input, output, session, state) {
  rv <- shiny::reactiveValues(step = 1, view = "Workflowr")

  # Header Left: Cancel or Back
  output$header_left <- shiny::renderUI({
    if (rv$step == 1) {
      shiny::actionButton("cancel", "Cancel", class = "btn-default", style = "padding: 6px 15px;")
    } else {
      shiny::actionButton("btn_back", "Back to Edit", class = "btn-secondary", style = "padding: 6px 15px;")
    }
  })

  # Header Center: State-colored view toggles
  output$header_center <- shiny::renderUI({
    if (rv$step == 1) {
      btn_style <- function(name, color) {
        active <- rv$view == name
        base <- sprintf("padding: 6px 12px; margin: 0 4px; font-weight: bold; border: 2px solid %s; border-radius: 4px; transition: 0.2s;", color)
        if (active) paste0(base, sprintf(" background-color: %s; color: white;", color))
        else paste0(base, sprintf(" background-color: white; color: %s;", color))
      }

      shiny::tags$div(
        shiny::actionButton("view_wf", "Workflowr", style = btn_style("Workflowr", state$wf_color)),
        shiny::actionButton("view_git", "Git", style = btn_style("Git", state$git_color)),
        shiny::actionButton("view_renv", "renv", style = btn_style("renv", state$renv_color))
      )
    } else {
      shiny::tags$div(style = "font-weight: bold; font-size: 1.2em; color: #333;", "Review & Confirm")
    }
  })

  # Header Right: Review or Execute
  output$header_right <- shiny::renderUI({
    if (rv$step == 1) {
      shiny::actionButton("btn_review", "Review Sync", class = "btn-primary", style = "font-weight: bold; padding: 6px 15px;")
    } else {
      shiny::actionButton("btn_execute", "Confirm", class = "btn-success", style = "font-weight: bold; padding: 6px 15px;")
    }
  })

  # Handle Tab Toggles
  shiny::observeEvent(input$view_wf, { shiny::updateTabsetPanel(session, "hidden_tabs", selected = "Workflowr"); rv$view <- "Workflowr" })
  shiny::observeEvent(input$view_git, { shiny::updateTabsetPanel(session, "hidden_tabs", selected = "Git"); rv$view <- "Git" })
  shiny::observeEvent(input$view_renv, { shiny::updateTabsetPanel(session, "hidden_tabs", selected = "renv"); rv$view <- "renv" })

  get_wflow_selected <- shiny::reactive({
    c(input$wflow_cb_scratch, input$wflow_cb_modified, input$wflow_cb_unpub)
  })

  output$dynamic_ui <- shiny::renderUI({
    if (rv$step == 1) {
      shiny::tabsetPanel(
        id = "hidden_tabs",
        selected = rv$view,

        # WORKFLOWR TAB
        shiny::tabPanel("Workflowr",
                        shiny::tags$div(style = "margin-top: 10px;",
                                        create_card(state$wf_title, state$wf_color,
                                                    if (state$wf_state == "missing") {
                                                      shiny::p(style = "color: #dc3545; font-weight: bold;", "Run workflowr::wflow_start() to configure.")
                                                    } else if (length(state$wflow_files) == 0) {
                                                      shiny::p(style = "color: grey; text-align: center; margin-top: 20px;", "All .Rmd files are compiled and synced.")
                                                    } else {
                                                      wf_ui_list <- list(shiny::p("Files to knit & publish:"))

                                                      if (length(state$wf_scratch) > 0) {
                                                        wf_ui_list <- append(wf_ui_list, list(
                                                          shiny::tags$b("Scratch"),
                                                          shiny::tags$div(style = "color: grey; font-size: 0.9em; margin-bottom: 5px;", "Files in analysis directory not tracked by workflowr."),
                                                          shiny::checkboxGroupInput("wflow_cb_scratch", label = NULL, choices = state$wf_scratch, selected = isolate(input$wflow_cb_scratch) %||% state$wf_scratch)
                                                        ))
                                                      }
                                                      if (length(state$wf_modified) > 0) {
                                                        wf_ui_list <- append(wf_ui_list, list(
                                                          shiny::tags$b("Modified"),
                                                          shiny::tags$div(style = "color: grey; font-size: 0.9em; margin-bottom: 5px;", "Rmd files modified since last build."),
                                                          shiny::checkboxGroupInput("wflow_cb_modified", label = NULL, choices = state$wf_modified, selected = isolate(input$wflow_cb_modified) %||% state$wf_modified)
                                                        ))
                                                      }
                                                      if (length(state$wf_unpub) > 0) {
                                                        wf_ui_list <- append(wf_ui_list, list(
                                                          shiny::tags$b("Unpublished"),
                                                          shiny::tags$div(style = "color: grey; font-size: 0.9em; margin-bottom: 5px;", "Built HTML files not committed to Git."),
                                                          shiny::checkboxGroupInput("wflow_cb_unpub", label = NULL, choices = state$wf_unpub, selected = isolate(input$wflow_cb_unpub) %||% state$wf_unpub)
                                                        ))
                                                      }
                                                      wf_ui_list <- append(wf_ui_list, list(shiny::textInput("wflow_msg", "Publish Message:", value = isolate(input$wflow_msg), placeholder = "Update analysis")))
                                                      do.call(shiny::tagList, wf_ui_list)
                                                    }
                                        )
                        )
        ),

        # GIT TAB
        shiny::tabPanel("Git",
                        shiny::tags$div(style = "margin-top: 10px;",
                                        create_card(state$git_title, state$git_color,
                                                    shiny::tagList(
                                                      shiny::tags$div(style = sprintf("font-weight: bold; margin-bottom: 10px; border-bottom: 1px solid #ddd; padding-bottom: 10px; color: %s;", state$pat_color), state$pat_text),

                                                      if (!state$has_remote) {
                                                        shiny::p(style = "color: #dc3545; font-weight: bold;", "\U0000274C No GitHub remote! Run usethis::use_github() to link.")
                                                      },
                                                      if (state$n_behind > 0) {
                                                        shiny::tagList(
                                                          shiny::p(style = "color: #dc3545; font-weight: bold;", sprintf("\U000026A0\U0000FE0F You are %d commits behind GitHub!", state$n_behind)),
                                                          shiny::checkboxInput("do_pull", shiny::tags$b("Pull latest changes"), value = isolate(input$do_pull) %||% TRUE),
                                                          shiny::hr()
                                                        )
                                                      },
                                                      if (state$n_ahead > 0) {
                                                        shiny::p(style = "color: #f39c12; font-weight: bold;", sprintf("\U00002191 %d local commits ready to push.", state$n_ahead))
                                                      },
                                                      if (length(state$git_files_only) == 0) {
                                                        shiny::p(style = "color: grey; text-align: center; margin-top: 10px;", "No other untracked or modified files.")
                                                      } else {
                                                        shiny::tagList(
                                                          shiny::p("Standard files to commit:"),
                                                          shiny::checkboxGroupInput("git_cb", label = NULL, choices = state$git_files_only, selected = isolate(input$git_cb) %||% state$git_files_only),
                                                          shiny::textInput("git_msg", "Commit Message:", value = isolate(input$git_msg), placeholder = "Update project files")
                                                        )
                                                      }
                                                    )
                                        )
                        )
        ),

        # RENV TAB
        shiny::tabPanel("renv",
                        shiny::tags$div(style = "margin-top: 10px;",
                                        create_card(state$renv_title, state$renv_color,
                                                    if (state$renv_state == "missing") {
                                                      shiny::p(style = "color: #dc3545; font-weight: bold;", "Run renv::init() to set up package tracking.")
                                                    } else if (state$renv_state == "desync") {
                                                      shiny::tagList(
                                                        # Use the new robust aesthetic table parser
                                                        parse_renv_status(state$renv_out),
                                                        shiny::radioButtons("renv_action", shiny::tags$b("Action to sync library:"),
                                                                            choices = c("Snapshot (Update lockfile)" = "snapshot",
                                                                                        "Restore (Revert library)" = "restore",
                                                                                        "Skip" = "skip"),
                                                                            selected = isolate(input$renv_action) %||% "skip")
                                                      )
                                                    } else {
                                                      shiny::p(style = "color: grey; text-align: center; margin-top: 20px;", "Packages are perfectly tracked and synced.")
                                                    }
                                        )
                        )
        )
      )
    } else {
      # STEP 2: REVIEW & CONFIRM
      shiny::tagList(
        shiny::p(style = "margin-bottom: 15px;", "Review your selections below:"),

        create_card("1. Publish Review", "#007bff",
                    if (length(get_wflow_selected()) > 0) {
                      shiny::tagList(
                        shiny::p(shiny::tags$b("Message: "), if (is.null(input$wflow_msg) || input$wflow_msg == "") "Update analysis" else input$wflow_msg),
                        shiny::p(shiny::tags$b("Files to knit & publish:")),
                        shiny::tags$ul(lapply(get_wflow_selected(), shiny::tags$li))
                      )
                    } else {
                      shiny::p(style = "color: grey;", "No .Rmd files selected for publishing.")
                    }
        ),

        create_card("2. Commit Review", "#007bff",
                    if (length(input$git_cb) > 0 || (isTRUE(input$renv_action == "snapshot")) || state$n_ahead > 0 || isTRUE(input$do_pull)) {
                      shiny::tagList(
                        if (isTRUE(input$do_pull)) shiny::p(style = "color: #28a745; font-weight: bold;", "\U00002193 Will pull changes from GitHub first."),
                        if (state$n_ahead > 0) shiny::p(style = "color: #f39c12; font-weight: bold;", sprintf("\U00002191 Will push %d existing commits.", state$n_ahead)),
                        if (length(input$git_cb) > 0 || (isTRUE(input$renv_action == "snapshot"))) {
                          shiny::tagList(
                            shiny::p(shiny::tags$b("Message: "), if (is.null(input$git_msg) || input$git_msg == "") "Update project files" else input$git_msg),
                            shiny::p(shiny::tags$b("Files to commit:")),
                            shiny::tags$ul(
                              if (isTRUE(input$renv_action == "snapshot")) shiny::tags$li(shiny::tags$b("renv.lock")),
                              lapply(input$git_cb, shiny::tags$li)
                            )
                          )
                        }
                      )
                    } else {
                      shiny::p(style = "color: grey;", "No standard files selected for commit.")
                    }
        ),

        create_card("3. Environment Review", "#007bff",
                    if (is.null(input$renv_action) || input$renv_action == "skip") {
                      shiny::p(style = "color: grey;", "No renv updates selected.")
                    } else if (input$renv_action == "snapshot") {
                      shiny::p("\U00002705 Will run ", shiny::tags$code("renv::snapshot()"), " and commit the updated lockfile.")
                    } else if (input$renv_action == "restore") {
                      shiny::p("\U00002705 Will run ", shiny::tags$code("renv::restore()"), " to revert library state.")
                    }
        )
      )
    }
  })

  shiny::observeEvent(input$btn_review, { rv$step <- 2 })
  shiny::observeEvent(input$btn_back, { rv$step <- 1 })

  shiny::observeEvent(input$btn_execute, {
    shiny::stopApp(list(
      action = "execute",
      do_pull = isTRUE(input$do_pull),
      renv_action = if (is.null(input$renv_action)) "skip" else input$renv_action,
      wflow_files = get_wflow_selected(),
      wflow_msg = input$wflow_msg,
      git_files = input$git_cb,
      git_msg = input$git_msg
    ))
  })

  shiny::observeEvent(input$cancel, {
    shiny::stopApp(NULL)
  })
}


#' Check Project Status and Sync
#'
#' Opens an interactive Shiny Gadget allowing users to select files to publish
#' via Workflowr or commit/push via Git. Automatically checks for unpulled changes
#' and verifies final synchronization status. Features a 2-step review process.
#'
#' @export
sync_status <- function() {

  cli::cli_h1("Analyzing Project Status...")
  state <- get_project_state()

  res <- shiny::runGadget(status_ui(),
                          function(input, output, session) status_server(input, output, session, state),
                          viewer = shiny::paneViewer(minHeight = 600))

  # EXECUTE SYNC (Clean CLI Output) -------------------------------------
  if (!is.null(res) && res$action == "execute") {

    Sys.sleep(1) # RStudio visual sync delay
    cli::cli_h2("Executing Sync Operations")
    Sys.setenv(GIT_TERMINAL_PROMPT = "0")

    # 0. PULL
    if (res$do_pull) {
      cli::cli_alert_info("Pulling latest changes...")
      out <- system2("git", "pull", stdout = TRUE, stderr = TRUE)
      if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
        cli::cli_alert_danger("Pull failed:\n{paste(out, collapse='\n')}")
      } else {
        cli::cli_alert_success("Pull complete.")
      }
    }

    # 1. RENV
    if (res$renv_action == "snapshot") {
      cli::cli_alert_info("Running renv::snapshot()...")
      renv::snapshot(prompt = FALSE)
      cli::cli_alert_success("Lockfile updated.")
      if (!("renv.lock" %in% res$git_files)) res$git_files <- c(res$git_files, "renv.lock")
    } else if (res$renv_action == "restore") {
      cli::cli_alert_info("Running renv::restore()...")
      renv::restore(prompt = FALSE)
      cli::cli_alert_success("Environment restored.")
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
    out <- system2("git", "push", stdout = TRUE, stderr = TRUE)

    if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
      cli::cli_alert_danger("Push failed:\n{paste(out, collapse='\n')}")
    } else {
      cli::cli_alert_success("Push successful.")
    }

    # 5. FINAL VERIFICATION
    final_ab <- tryCatch(gert::git_ahead_behind(), error = function(e) list(ahead = 1, behind = 1))
    if (final_ab$ahead == 0 && final_ab$behind == 0) {
      cli::cli_alert_success("Project perfectly synced!")
    }
  }
}
