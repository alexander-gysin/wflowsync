#' Sync Project Files and Publish Workflowr Analyses
#'
#' A context-aware wrapper to commit, push, and optionally publish files.
#' Automatically detects the active file in RStudio. Only previously tracked
#' modified files will be automatically staged.
#'
#' @param publish Either FALSE (default), TRUE (alias for "current"), or a string:
#'   "current", "modified", "scratch", or "all".
#' @param msg Commit message. If NULL, prompts the user. Defaults to "WIP update".
#'
#' @export
sync <- function(publish = FALSE, msg = NULL) {

  # 1. DETECT ACTIVE RSTUDIO FILE ---------------------------------------
  active_file <- NULL
  if (rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx) && nzchar(ctx$path)) {
      proj_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
      active_file <- normalizePath(ctx$path, winslash = "/", mustWork = FALSE)
      active_file <- sub(paste0("^", proj_root, "/"), "", active_file)
    }
  }

  # 2. HANDLE PUBLISH LOGIC ---------------------------------------------
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

    } else if (publish %in% c("modified", "scratch")) {
      ws <- tryCatch(workflowr::wflow_status(), error = function(e) NULL)
      if (!is.null(ws) && !is.null(ws$status)) {
        publish_targets <- rownames(ws$status)[ws$status[[publish]]]
      }
    } else if (publish == "all") {
      publish_targets <- list.files("analysis", pattern = "\\.Rmd$", full.names = TRUE)
      republish_flag <- TRUE
    } else if (publish == "republish") {
      publish_targets <- NULL
      republish_flag <- TRUE
    } else {
      cli::cli_abort("Invalid publish argument. Use TRUE, 'current', 'modified', 'scratch', 'republish', or 'all'.")
    }

    if (length(publish_targets) == 0 && !(publish %in% c("all", "republish"))) {
      cli::cli_alert_info("No files matched the publish target '{publish}'. Skipping publish step.")
    }
  }

  # 3. MESSAGE PROMPT ---------------------------------------------------
  if (is.null(msg)) {
    if (!interactive()) {
      msg <- "WIP update"
    } else {
      msg <- readline(prompt = "Enter commit message (default: 'WIP update'): ")
    }
  }
  if (msg == "") msg <- "WIP update"

  # 4. EXECUTION BLOCK --------------------------------------------------
  cli::cli_h2("Executing Sync")

  # A. Publish (if requested and targets exist)
  if (length(publish_targets) > 0 || publish == "republish") {
    cli::cli_alert_info("Publishing Workflowr files...")
    workflowr::wflow_publish(publish_targets, message = msg, republish = republish_flag)
    cli::cli_alert_success("Publish complete.")
  }

  # B. Git Add (Only tracked files that have been modified/deleted)
  cli::cli_alert_info("Staging tracked modifications...")
  gs_current <- gert::git_status()
  modified_tracked <- gs_current$file[gs_current$status %in% c("modified", "deleted")]

  if (length(modified_tracked) > 0) {
    gert::git_add(modified_tracked)
  }

  # C. Git Commit
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
  out <- suppressWarnings(system2("git", "push", stdout = TRUE, stderr = TRUE))

  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    cli::cli_alert_danger("Push failed:\n{paste(out, collapse='\n')}")
  } else {
    cli::cli_alert_success("Push successful.")
  }

  invisible(TRUE)
}
