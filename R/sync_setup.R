#' Initialize and Configure the wflowsync Environment
#'
#' Guides the user through setting up a reproducible data analysis environment
#' on a headless Linux cluster. It sequentially verifies the presence of
#' Workflowr, renv, and a linked GitHub repository, and then securely configures
#' native Git credential storage for automated syncing.
#'
#' @param reset_credentials Logical. Set to \code{TRUE} to overwrite existing
#'   Git credentials and prompt for a new GitHub Personal Access Token (PAT).
#'   Default is \code{FALSE}.
#'
#' @return Invisible \code{TRUE} upon successful completion.
#' @export
sync_setup <- function(reset_credentials = FALSE) {

  cli::cli_h1("Initializing wflowsync Setup")

  # 1. TOOL CHECKS ------------------------------------------------
  cli::cli_h2("1. Environment Checks")

  # --- Checkpoint 1: Workflowr ---
  if (file.exists("_workflowr.yml") || file.exists("analysis/_site.yml")) {
    cli::cli_alert_success("Workflowr is initialized.")
  } else {
    cli::cli_alert_warning("Workflowr config missing.")
    cli::cli_text("Workflowr builds the reproducible project structure (analysis/, data/, docs/) and initializes Git.")
    cli::cli_text("Press {.kbd Esc} to exit setup. Then run:")
    cli::cli_bullets(c(
      "*" = "{.code workflowr::wflow_start(\".\", existing = TRUE)} (if you are already inside your project folder), or",
      "*" = "{.code workflowr::wflow_start(\"my_project_name\")} (to create a brand new directory)."
    ))
    cli::cli_text("Then restart {.code sync_setup()}.")
  }
  readline(prompt = "Press [ENTER] to continue to the renv check...")

  # --- Checkpoint 2: renv ---
  if (file.exists("renv.lock")) {
    cli::cli_alert_success("renv is initialized.")
  } else {
    cli::cli_alert_warning("renv is not initialized.")
    cli::cli_text("{.pkg renv} isolates your R packages and creates a {.file renv.lock} file to guarantee code reproducibility across different computers (like your cluster).")
    cli::cli_text("Press {.kbd Esc} to exit setup. Then run:")
    cli::cli_bullets(c(
      "*" = "{.code renv::init()} to scan your code, build the local library, and generate the lockfile."
    ))
    cli::cli_text("Then restart {.code sync_setup()}.")
  }
  readline(prompt = "Press [ENTER] to continue to the Git & GitHub check...")

  # --- Checkpoint 3: Git & GitHub ---
  is_git <- tryCatch(length(gert::git_info()) > 0, error = function(e) FALSE)

  if (is_git) {
    remotes <- tryCatch(gert::git_remote_list()$name, error = function(e) character(0))
    if ("origin" %in% remotes) {
      cli::cli_alert_success("Git is initialized and linked to a remote origin.")
      cli::cli_text(cli::col_grey("Note: If you just initialized renv, your first sync() will automatically commit the lockfile for you!"))
    } else {
      cli::cli_alert_warning("Git is initialized, but no remote GitHub repository is linked.")
      cli::cli_text("Your project needs a cloud destination (origin) to sync your files safely.")
      cli::cli_text("Press {.kbd Esc} to exit setup. Then run:")
      cli::cli_bullets(c(
        "*" = "{.code workflowr::wflow_use_github(\"your_github_username\")} to automatically create the repo on GitHub and link it to this folder."
      ))
      cli::cli_text("Then restart {.code sync_setup()}.")
    }
  } else {
    cli::cli_alert_warning("Git is not initialized in this directory.")
    cli::cli_text("Press {.kbd Esc} to exit setup. Running {.code workflowr::wflow_start()} (from Checkpoint 1) resolves this.")
  }
  readline(prompt = "Press [ENTER] to proceed to GitHub Authentication...")

  # 2. CREDENTIAL MANAGEMENT ------------------------------------------------
  cli::cli_h2("2. GitHub Authentication")

  if (reset_credentials) {
    cli::cli_alert_danger("Resetting credentials...")
    if (file.exists("~/.git-credentials")) file.remove("~/.git-credentials")
    if (file.exists("~/.wflowsync_meta.rds")) file.remove("~/.wflowsync_meta.rds")
    tryCatch(gitcreds::gitcreds_delete(), error = function(e) invisible())
  }

  if (!file.exists("~/.wflowsync_meta.rds")) {

    cli::cli_text("How are you accessing this R session?")
    env_choice <- utils::menu(
      choices = c(
        "Personal Computer (Local Mac or Windows)",
        "Remote Server/Cluster (SSH or RStudio Server in a web browser)"
      ),
      title = "Select your environment type:"
    )

    if (env_choice == 0) {
      cli::cli_alert_danger("Setup aborted.")
      stop("User cancelled setup.", call. = FALSE)
    }

    cli::cli_h3("Generating a GitHub Personal Access Token (PAT)")
    cli::cli_text("To push securely, you need a PAT.")
    cli::cli_ul(c(
      "Go to: {.url https://github.com/settings/tokens/new}",
      "Note: You can run {.code usethis::create_github_token()} in a new console to open the exact page.",
      "Set an expiration date (e.g., 60 days).",
      "Check the {.strong 'repo'} scope box (this is all we need).",
      "Click 'Generate token' at the bottom."
    ))

    readline(prompt = "Press [ENTER] once you have copied your PAT...")

    pat_days <- readline(prompt = "How many days until this PAT expires? (e.g. 60, 30): ")
    pat_days_num <- suppressWarnings(as.numeric(pat_days))

    while (is.na(pat_days_num) || pat_days_num <= 0) {
      cli::cli_alert_danger("Invalid input. Please enter a valid positive number.")
      pat_days <- readline(prompt = "How many days until this PAT expires? (e.g. 90, 30): ")
      pat_days_num <- suppressWarnings(as.numeric(pat_days))
    }

    expiry_date <- as.character(Sys.Date() + pat_days_num)

    if (env_choice == 1) {
      cli::cli_alert_info("Configuring credentials using your native OS secure credential manager.")
      cli::cli_text("When prompted below, paste your PAT as the password.")

      gitcreds::gitcreds_set()

      meta <- list(type = "desktop", expiry = expiry_date)
      saveRDS(meta, file = "~/.wflowsync_meta.rds")

      cli::cli_alert_success("Credentials successfully encrypted via your OS manager!")

    } else if (env_choice == 2) {
      cli::cli_alert_info("Configuring native Git credential storage for headless clusters.")
      cli::cli_text("Your token will be safely locked to your isolated Linux user profile.")
      cli::cli_alert_warning("System Administrators and other users who have access to your login can read the username and PAT!")

      tryCatch({
        system("git config --global credential.helper store", ignore.stdout = TRUE, ignore.stderr = TRUE)
      }, error = function(e) invisible())

      gh_user <- readline(prompt = "Enter your GitHub Username: ")

      if (rstudioapi::isAvailable()) {
        gh_pat <- rstudioapi::askForPassword("Paste your GitHub PAT:")
      } else {
        gh_pat <- readline(prompt = "Paste your GitHub PAT (text will be visible): ")
      }

      cred_string <- sprintf("https://%s:%s@github.com", gh_user, gh_pat)
      writeLines(cred_string, con = "~/.git-credentials")
      Sys.chmod("~/.git-credentials", mode = "0600")

      meta <- list(username = gh_user, type = "headless", expiry = expiry_date)
      saveRDS(meta, file = "~/.wflowsync_meta.rds")

      cli::cli_alert_success("Credentials successfully saved and locked to your user profile!")
    }

    cli::cli_alert_info("Your PAT expires on: {.emph {expiry_date}}")

  } else {
    meta <- tryCatch(readRDS("~/.wflowsync_meta.rds"), error = function(e) list(expiry = "Unknown"))
    cli::cli_alert_success("Git credentials already configured.")
    cli::cli_text("Your PAT expires on: {.emph {meta$expiry}}")
    cli::cli_text("To update your PAT, run {.code sync_setup(reset_credentials = TRUE)}")
  }
  cli::cli_alert_success("wflowsync setup complete! You are ready to use sync_status().")
  invisible(TRUE)
}
