#' Configure wflowsync
#'
#' Interactive wizard that sets up workflowr, initializes renv, and configures
#' GitHub credentials. Safely handles R session restarts.
#'
#' @export
sync_setup <- function() {

  if (!requireNamespace("cli", quietly = TRUE)) stop("The 'cli' package is required.")

  # Helper function for Yes/No prompts
  ask_yn <- function(prompt_text) {
    if (!interactive()) return(FALSE)
    ans <- tolower(trimws(readline(prompt = paste(prompt_text, "(y/n): "))))
    return(ans %in% c("y", "yes"))
  }

  cli::cli_h1("wflowsync Setup Wizard")


  # STEP 1: WORKFLOWR -------------------------------------------------------

  cli::cli_h2("1. Workflowr Configuration")

  has_wflow <- file.exists("_workflowr.yml") || file.exists("analysis/_site.yml")

  if (has_wflow) {
    cli::cli_alert_success("Workflowr is already configured in this directory.")
  } else {
    cli::cli_alert_warning("No workflowr project detected in the current directory.")

    # The New Prompt
    already_created <- ask_yn("Have you already created a workflowr project in which you would like to use wflowsync?")

    if (already_created) {
      cli::cli_abort(c(
        "x" = "Setup paused.",
        "i" = "Please open that existing project in RStudio (File -> Open Project...), then run {.code wflowsync::sync_setup()} again."
      ))
    }

    in_folder <- ask_yn("Are you currently inside the empty folder you want to turn into a workflowr project?")

    if (in_folder) {
      readline(prompt = "Press [ENTER] to initialize workflowr here...")
      workflowr::wflow_start(".", existing = TRUE)
      cli::cli_alert_success("Workflowr initialized!")
    } else {
      new_name <- readline(prompt = "Enter a name for your new project folder: ")
      if (trimws(new_name) == "") cli::cli_abort("Invalid name. Setup aborted.")

      cli::cli_alert_info("RStudio will now create and switch to the new project: {.file {new_name}}")
      cli::cli_alert_danger("IMPORTANT: This will restart your R session.")
      readline(prompt = "After the restart, please run wflowsync::sync_setup() again. Press [ENTER] to acknowledge and restart... ")

      workflowr::wflow_start(new_name, change_wd = TRUE)
      return(invisible(TRUE)) # Exit the function here so RStudio can restart
    }
  }


  # STEP 2: RENV ------------------------------------------------------------

  cli::cli_h2("2. Package Tracking (renv)")

  if (!file.exists("renv.lock")) {
    cli::cli_alert_warning("renv is not tracking packages in this project.")
    do_renv <- ask_yn("Would you like to initialize renv now?")

    if (do_renv) {
      cli::cli_alert_warning("IMPORTANT: Initializing renv may restart your R session.")
      readline(prompt = "If it restarts, just run wflowsync::sync_setup() again to finish Step 3. Press [ENTER] to continue... ")

      renv::init()

      # If renv restarts the session, the function ends here automatically.
      # If it doesn't restart, we just continue to Step 3.
    } else {
      cli::cli_alert_info("Skipping renv setup.")
    }
  } else {
    cli::cli_alert_success("renv is already initialized.")
  }


  # STEP 3: GITHUB PAT ------------------------------------------------------

  cli::cli_h2("3. GitHub Authentication")

  has_pat <- FALSE
  if (requireNamespace("gitcreds", quietly = TRUE)) {
    cred <- tryCatch(gitcreds::gitcreds_get(), error = function(e) NULL)
    if (!is.null(cred)) has_pat <- TRUE
  }

  if (has_pat) {
    cli::cli_alert_success("GitHub PAT is already configured on this machine.")

    # Optionally update the expiry metadata file so our UI knows it's active
    days_valid <- readline(prompt = "How many days is this PAT valid for? (Press ENTER to default to 30): ")
    days_valid <- if (trimws(days_valid) == "") 30 else as.numeric(days_valid)
    if (is.na(days_valid)) days_valid <- 30

    meta <- list(expiry = Sys.Date() + days_valid)
    saveRDS(meta, "~/.wflowsync_meta.rds")

  } else {
    cli::cli_alert_warning("No GitHub Personal Access Token (PAT) found.")
    cli::cli_text("You need a PAT to push your analysis to GitHub.")
    cli::cli_text("1. Run {.code usethis::create_github_token()} in your console to generate one.")
    cli::cli_text("2. Copy the token.")
    cli::cli_text("3. Run {.code gitcreds::gitcreds_set()} and paste the token.")
    cli::cli_text("4. Run {.code wflowsync::sync_setup()} one last time to save the expiry date.")
  }

  cli::cli_text("")
  cli::cli_alert_success("Setup check complete!")
  invisible(TRUE)
}
