#' Configure wflowsync
#'
#' Interactive wizard that sets up workflowr, initializes renv, and configures
#' GitHub credentials. Safely handles R session restarts.
#'
#' @description
#' `sync_setup()` is designed to be run at the beginning of your workflow and is
#' completely safe to run multiple times. It operates as a state-aware checklist
#' covering three main steps:
#'
#' **1. Workflowr Configuration:** The function checks if the current working directory
#' is a valid workflowr project. If not, it asks if you want to create a brand new
#' project. If you choose not to, it pauses and prompts you to open your existing
#' project folder first.
#'
#' **2. Package Tracking (renv):** It checks for an active `renv.lock` file. If
#' missing, it offers to initialize `renv` to ensure computational reproducibility.
#' It safely injects a temporary dependency script so `renv` automatically carries
#' `wflowsync` into your new isolated environment.
#'
#' **3. GitHub Authentication:** It verifies if a valid GitHub Personal Access
#' Token (PAT) is available via the `gitcreds` package. If not, it provides
#' step-by-step instructions to generate and safely store one.
#'
#' **Note on Restarts:** Creating a new project directory or initializing `renv`
#' may automatically restart your R session. This is normal RStudio behavior. The
#' wizard provides acknowledgment prompts before triggering these restarts. Once R
#' restarts, simply run `wflowsync::sync_setup()` again—it will remember your
#' progress, skip the completed steps, and pick up exactly where it left off.
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

    create_new <- ask_yn("Do you want to create a brand new workflowr project with wflowsync?")

    if (!create_new) {
      cli::cli_abort(c(
        "x" = "Setup paused.",
        "i" = "It looks like you want to use an existing project.",
        "i" = "Please open that project in RStudio (File -> Open Project...), then run {.code wflowsync::sync_setup()} again."
      ))
    }

    new_name <- readline(prompt = "Enter a name for your new project folder (or type '.' to use current directory): ")
    if (trimws(new_name) == "") cli::cli_abort("Invalid name. Setup aborted.")

    if (trimws(new_name) == ".") {
      readline(prompt = "Press [ENTER] to initialize workflowr here...")
      workflowr::wflow_start(".", existing = TRUE)
      cli::cli_alert_success("Workflowr initialized!")
    } else {
      # Build the project in the background without trying to switch the R directory yet
      workflowr::wflow_start(new_name, change_wd = FALSE)

      cli::cli_alert_info("RStudio will now force-switch to your new project: {.file {new_name}}")
      cli::cli_alert_danger("IMPORTANT: This will restart your R session.")
      readline(prompt = "After the restart, please run wflowsync::sync_setup() again. Press [ENTER] to switch... ")

      # Use the robust RStudio API to natively open the .Rproj file
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::openProject(new_name)
      } else {
        cli::cli_alert_warning("rstudioapi is unavailable. Please open the new folder manually.")
      }
      return(invisible(TRUE))
    }
  }


  # STEP 2: RENV ------------------------------------------------------------

  cli::cli_h2("2. Package Tracking (renv)")

  # Clean up the dummy script from a previous run if it exists
  if (file.exists("_wflowsync_deps.R")) unlink("_wflowsync_deps.R")

  if (!file.exists("renv.lock")) {
    cli::cli_alert_warning("renv is not tracking packages in this project.")
    do_renv <- ask_yn("Would you like to initialize renv now?")

    if (do_renv) {
      cli::cli_alert_warning("IMPORTANT: Initializing renv may restart your R session.")

      # Inject the dummy script so renv's scanner finds wflowsync and installs it
      writeLines("library(wflowsync)", "_wflowsync_deps.R")

      readline(prompt = "If it restarts, just run wflowsync::sync_setup() again to finish Step 3. Press [ENTER] to continue... ")

      renv::init()

      # If the session didn't restart, clean up immediately
      if (file.exists("_wflowsync_deps.R")) unlink("_wflowsync_deps.R")

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
