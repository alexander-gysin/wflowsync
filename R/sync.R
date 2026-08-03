#' Sync Project Files and Publish Workflowr Analyses
#'
#' A context-aware wrapper to commit, push, and optionally publish files.
#' Automatically detects the active file in RStudio.
#'
#' @param publish Either FALSE (default), TRUE (alias for "current"), or a string:
#'   "current", "modified", "scratch", or "all".
#' @param msg Commit message. If NULL, prompts the user. Defaults to "WIP update".
#'
#' @export
sync <- function(publish = FALSE, msg = NULL) {

  if (!requireNamespace("cli", quietly = TRUE)) stop("The 'cli' package is required.")

  # 1. EVALUATE STATUS (3-line summary) ---------------------------------
  cli::cli_h2("Project Status")

  # Workflowr Status
  has_wflow <- file.exists("_workflowr.yml") || file.exists("analysis/_site.yml")
  if (!has_wflow) {
    cli::cli_alert_warning("Workflowr: Action required (Not configured)")
  } else {
    ws <- tryCatch(workflowr::wflow_status(), error = function(e) NULL)
    if (!is.null(ws) && any(ws$status$unpublished | ws$status$modified | ws$status$scratch)) {
      cli::cli_alert_warning("Workflowr: Action required")
    } else {
      cli::cli_alert_success("Workflowr: Synced")
    }
  }

  # renv Status
  rs <- suppressMessages(suppressWarnings(tryCatch(renv::status(), error = function(e) list(synchronized = TRUE))))
  if (isFALSE(rs$synchronized)) {
    cli::cli_alert_warning("renv: Action required")
  } else {
    cli::cli_alert_success("renv: Synced")
  }

  # Git Status
  gs <- tryCatch(gert::git_status(), error = function(e) NULL)
  ab <- tryCatch(gert::git_ahead_behind(), error = function(e) list(ahead = 0, behind = 0))
  has_git_changes <- (!is.null(gs) && nrow(gs) > 0) || (ab$ahead > 0) || (ab$behind > 0)
  if (has_git_changes) {
    cli::cli_alert_warning("Git: Action required")
  } else {
    cli::cli_alert_success("Git: Synced")
  }

  cli::cli_text("") # Add an empty line for spacing

  # 2. DETECT ACTIVE RSTUDIO FILE ---------------------------------------
  active_file <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx) && nzchar(ctx$path)) {
      # Convert to relative path
      proj_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
      active_file <- normalizePath(ctx$path, winslash = "/", mustWork = FALSE)
      active_file <- sub(paste0("^", proj_root, "/"), "", active_file)
    }
  }

  # 3. HANDLE PUBLISH LOGIC ---------------------------------------------
  if (isTRUE(publish)) publish <- "current"

  publish_targets <- character(0)
  republish_flag <- FALSE

  if (is.character(publish)) {
    if (publish == "current") {
      if (is.null(active_file)) {
        cli::cli_abort("No active file detected in RStudio. Cannot publish 'current'.")
      }
      if (!grepl("\\.Rmd$", active_file, ignore.case = TRUE)) {
        cli::cli_abort(c(
          "x" = "Only .Rmd files can be published.",
          "i" = "Currently active file is: {.file {active_file}}",
          "!" = "Sync completely aborted. Nothing was committed or pushed."
        ))
      }
      publish_targets <- active_file

    } else if (publish == "modified") {
      if (!is.null(ws)) {
        publish_targets <- rownames(ws$status)[ws$status$modified]
      }
    } else if (publish == "scratch") {
      if (!is.null(ws)) {
        publish_targets <- rownames(ws$status)[ws$status$scratch]
      }
    } else if (publish == "all") {
      # Grab absolutely every .Rmd file in the analysis folder
      publish_targets <- list.files("analysis", pattern = "\\.Rmd$", full.names = TRUE)
      republish_flag <- TRUE
    } else if (publish == "republish") {
      publish_targets <- NULL # workflowr natively handles this when republish = TRUE
      republish_flag <- TRUE
    } else {
      cli::cli_abort("Invalid publish argument. Use TRUE, 'current', 'modified', 'scratch', 'republish', or 'all'.")
    }

  if (length(publish_targets) == 0 && !(publish %in% c("all", "republish"))) {
    cli::cli_alert_info("No files matched the publish target '{publish}'. Skipping publish step.")
  }
  }

  # 4. MESSAGE PROMPT ---------------------------------------------------
  if (is.null(msg)) {
    # If the console isn't interactive, provide a safe default immediately
    if (!interactive()) {
      msg <- "WIP update"
    } else {
      msg <- readline(prompt = "Enter commit message (default: 'WIP update'): ")
    }
  }
  if (msg == "") msg <- "WIP update"

  # 5. EXECUTION BLOCK --------------------------------------------------
  cli::cli_h2("Executing Sync")

  # A. Publish (if requested and targets exist)
  if (length(publish_targets) > 0 || publish == "republish") {
    cli::cli_alert_info("Publishing Workflowr files...")
    workflowr::wflow_publish(publish_targets, message = msg, republish = republish_flag)
    cli::cli_alert_success("Publish complete.")
  }

  # B. Git Add (Current file + all modified tracked files)
  cli::cli_alert_info("Staging standard files...")
  gs_current <- gert::git_status()

  # Identify tracked but modified/deleted files
  modified_tracked <- gs_current$file[gs_current$status %in% c("modified", "deleted")]

  # Ensure the active file is always included if it exists (even if untracked)
  files_to_add <- modified_tracked
  if (!is.null(active_file) && file.exists(active_file)) {
    files_to_add <- unique(c(files_to_add, active_file))
  }

  if (length(files_to_add) > 0) {
    gert::git_add(files_to_add)
  }

  # C. Git Commit
  # We check if there's actually anything staged before trying to commit to avoid errors
  staged_files <- gert::git_status()
  if (any(staged_files$staged)) {
    cli::cli_alert_info("Committing files...")
    gert::git_commit(message = msg)
    cli::cli_alert_success(sprintf("Committed with message: '%s'", msg))
  } else {
    cli::cli_alert_info("No new changes to commit.")
  }

  # D. Git Push
  cli::cli_alert_info("Pushing to GitHub...")

  # Suppress warnings so we can format the error cleanly with cli if it fails
  out <- suppressWarnings(system2("git", "push", stdout = TRUE, stderr = TRUE))

  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    cli::cli_alert_danger("Push failed:\n{paste(out, collapse='\n')}")
  } else {
    cli::cli_alert_success("Push successful.")
  }

  invisible(TRUE)
}
