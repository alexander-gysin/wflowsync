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
#' @param reset Logical. If TRUE, skips project checks and immediately prompts
#'   to reset the global Git username, Git email, and GitHub PAT.
#'
#' @export
sync_setup <- function(reset = FALSE) {

  if (!requireNamespace("cli", quietly = TRUE)) stop("The 'cli' package is required.")

  # RESET TRACK -------------------------------------------------------------
  if (reset) {
    cli::cli_h1("wflowsync Credential Reset")

    cli::cli_alert_info("Let's configure your global Git identity.")
    name <- readline(prompt = "Enter your Git User Name (e.g., Jane Doe): ")
    email <- readline(prompt = "Enter your Git User Email (e.g., jane@example.com): ")

    if (trimws(name) != "" && trimws(email) != "") {
      usethis::use_git_config(user.name = trimws(name), user.email = trimws(email), scope = "user")
      cli::cli_alert_success("Git identity updated globally.")
    } else {
      cli::cli_alert_warning("Skipped Git identity update (input was empty).")
    }

    cli::cli_alert_info("Calling gitcreds to reset your PAT. When prompted, select 'Replace these credentials' or provide the new PAT.")
    tryCatch(gitcreds::gitcreds_set(), error = function(e) cli::cli_alert_danger("Failed to set PAT: {e$message}"))

    days_valid <- readline(prompt = "How many days is this new PAT valid for? (Press ENTER to default to 30): ")
    days_valid <- if (trimws(days_valid) == "") 30 else as.numeric(days_valid)
    if (is.na(days_valid)) days_valid <- 30

    meta <- list(expiry = Sys.Date() + days_valid)
    saveRDS(meta, get_config_path())

    cli::cli_alert_success("Credentials successfully reset!")
    return(invisible(TRUE))
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
      workflowr::wflow_start(new_name, change_wd = FALSE)
      cli::cli_alert_info("RStudio will now force-switch to your new project: {.file {new_name}}")
      cli::cli_alert_danger("IMPORTANT: This will restart your R session.")
      readline(prompt = "After the restart, please run wflowsync::sync_setup() again. Press [ENTER] to switch... ")

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

  if (file.exists("_wflowsync_deps.R")) unlink("_wflowsync_deps.R")

  if (!file.exists("renv.lock")) {
    cli::cli_alert_warning("renv is not tracking packages in this project.")
    do_renv <- ask_yn("Would you like to initialize renv now?")

    if (do_renv) {
      cli::cli_alert_warning("IMPORTANT: Initializing renv may restart your R session.")
      writeLines("library(wflowsync)", "_wflowsync_deps.R")
      readline(prompt = "If it restarts, just run wflowsync::sync_setup() again to finish. Press [ENTER] to continue... ")

      renv::init()
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
  cred <- tryCatch(gitcreds::gitcreds_get(), error = function(e) NULL)
  if (!is.null(cred)) has_pat <- TRUE

  if (has_pat) {
    cli::cli_alert_success("GitHub PAT is already configured on this machine.")

    cfg_path <- get_config_path()
    if (!file.exists(cfg_path)) {
      days_valid <- readline(prompt = "How many days is this PAT valid for? (Press ENTER to default to 30): ")
      days_valid <- if (trimws(days_valid) == "") 30 else as.numeric(days_valid)
      if (is.na(days_valid)) days_valid <- 30

      meta <- list(expiry = Sys.Date() + days_valid)
      saveRDS(meta, cfg_path)
    }

  } else {
    cli::cli_alert_warning("No GitHub Personal Access Token (PAT) found.")
    cli::cli_text("You need a PAT to push your analysis to GitHub.")
    cli::cli_text("1. Run {.code usethis::create_github_token()} to generate one.")
    cli::cli_text("2. Copy the token.")
    cli::cli_text("3. Run {.code gitcreds::gitcreds_set()} and paste the token.")
    cli::cli_text("4. Run {.code wflowsync::sync_setup()} again.")
    return(invisible(TRUE))
  }

  # STEP 4: GITHUB REPOSITORY -----------------------------------------------
  cli::cli_h2("4. Remote GitHub Repository")

  is_git_repo <- !is.null(tryCatch(gert::git_info(), error = function(e) NULL))
  just_linked_remote <- FALSE

  if (is_git_repo) {
    remotes <- tryCatch(gert::git_remote_list(), error = function(e) NULL)
    if (is.null(remotes) || nrow(remotes) == 0) {
      cli::cli_alert_warning("This project is not yet linked to a GitHub repository.")
      link_repo <- ask_yn("Would you like to automatically create and link a GitHub repository now?")
      if (link_repo) {
        cli::cli_alert_info("Running usethis::use_github()...")
        usethis::use_github()
        just_linked_remote <- TRUE
      } else {
        cli::cli_alert_info("Skipping GitHub repository creation.")
      }
    } else {
      cli::cli_alert_success("Project is linked to a remote repository.")
    }
  } else {
    cli::cli_alert_warning("Current directory is not a Git repository. Cannot link to GitHub yet.")
  }

  # STEP 5: INITIAL PUBLISH AND PUSH ----------------------------------------
  if (just_linked_remote) {
    ws <- tryCatch(workflowr::wflow_status(), error = function(e) NULL)

    if (!is.null(ws) && !is.null(ws$status)) {
      core_files <- c("analysis/index.Rmd", "analysis/about.Rmd", "analysis/license.Rmd")
      unpub <- rownames(ws$status)[ws$status$unpublished]
      files_to_pub <- intersect(core_files, unpub)

      if (length(files_to_pub) > 0) {
        cli::cli_text("")
        do_initial <- ask_yn("Would you like to automatically publish the default files, snapshot renv, and push to GitHub now?")

        if (do_initial) {
          # A. renv snapshot and stage (including renv/ dir safely)
          cli::cli_alert_info("Taking initial renv snapshot...")
          old_state <- get_lockfile_state()
          renv::snapshot(prompt = FALSE)
          new_state <- get_lockfile_state()
          renv_msg <- generate_renv_commit_msg(old_state, new_state)

          # Stage the lockfile, root gitignore, and the entire renv/ directory
          renv_files <- c("renv.lock", ".gitignore", "renv/")
          existing_renv <- renv_files[file.exists(renv_files)]
          if (length(existing_renv) > 0) {
            gert::git_add(existing_renv)
            # Commit the environment files independently using the dynamic message
            c_msg <- if (renv_msg == "Update renv.lock") "initial wflowsync commit (environment tracking)" else renv_msg
            gert::git_commit(c_msg)
          }

          # B. workflowr publish (commits the Rmds and HTMLs)
          cli::cli_alert_info("Publishing core workflowr files...")
          workflowr::wflow_publish(files_to_pub, message = "initial wflowsync commit")

          # C. Push to remote
          cli::cli_alert_info("Pushing to GitHub...")
          out <- suppressWarnings(system2("git", "push", stdout = TRUE, stderr = TRUE))

          if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
            cli::cli_alert_danger("Push failed:\n{paste(out, collapse='\n')}")
          } else {
            cli::cli_alert_success("Initial push successful.")
          }
        }
      }
    }
  }

  cli::cli_text("")
  cli::cli_alert_success("Setup check complete!")
  invisible(TRUE)
}
