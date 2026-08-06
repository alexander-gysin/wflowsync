
# INTERNAL HELPER FUNCTIONS


"%||%" <- function(a, b) if (!is.null(a) && a != "") a else b

get_config_path <- function() {
  dir_path <- tools::R_user_dir("wflowsync", "config")
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  file.path(dir_path, "wflowsync_meta.rds")
}

ask_yn <- function(prompt_text) {
  if (!interactive()) return(FALSE)
  ans <- tolower(trimws(readline(prompt = paste(prompt_text, "(y/n): "))))
  return(ans %in% c("y", "yes"))
}

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

# Parses renv.lock to extract packages and versions
get_lockfile_state <- function() {
  if (!file.exists("renv.lock")) return(character(0))

  lines <- readLines("renv.lock", warn = FALSE)
  pkg_lines <- grep('"Package":', lines, value = TRUE)
  ver_lines <- grep('"Version":', lines, value = TRUE)

  pkgs <- sub('.*"Package":\\s*"([^"]+)".*', '\\1', pkg_lines)
  vers <- sub('.*"Version":\\s*"([^"]+)".*', '\\1', ver_lines)

  if (length(pkgs) == length(vers)) {
    return(stats::setNames(vers, pkgs))
  }
  return(stats::setNames(rep("unknown", length(pkgs)), pkgs))
}

# Compares old and new lockfile states to generate a dynamic commit message
generate_renv_commit_msg <- function(old_state, new_state) {
  added <- setdiff(names(new_state), names(old_state))
  removed <- setdiff(names(old_state), names(new_state))
  common <- intersect(names(old_state), names(new_state))
  updated <- common[old_state[common] != new_state[common]]

  parts <- c()
  if (length(added) > 0) parts <- c(parts, paste("added:", paste(added, collapse = ", ")))
  if (length(updated) > 0) parts <- c(parts, paste("updated:", paste(updated, collapse = ", ")))
  if (length(removed) > 0) parts <- c(parts, paste("removed:", paste(removed, collapse = ", ")))

  if (length(parts) == 0) return("Update renv.lock")

  msg <- paste("Update renv.lock -", paste(parts, collapse = ". "))
  if (nchar(msg) > 150) msg <- paste0(substr(msg, 1, 147), "...")
  return(msg)
}

# Parses raw renv::status() text into a clean HTML table
parse_renv_status <- function(raw_text) {
  lines <- raw_text[trimws(raw_text) != ""]

  # Remove known conversational lines from renv output
  fluff <- c("^The following", "^See \\?", "^It looks like", "out of sync", "package\\(s\\)", "are in an inconsistent", "^-")
  for (p in fluff) lines <- lines[!grepl(p, lines, ignore.case = TRUE)]

  if (length(lines) == 0) return(shiny::p(style = "color: grey;", "No specific package differences found."))

  # Split remaining rows by 2 or more spaces to isolate columns
  rows <- lapply(lines, function(x) unlist(strsplit(trimws(x), "\\s{2,}")))

  # Build aesthetic HTML table
  shiny::tags$table(style = "width: 100%; text-align: left; border-collapse: collapse; font-size: 0.85em; margin-bottom: 15px;",
                    shiny::tags$tbody(
                      lapply(seq_along(rows), function(i) {
                        is_header <- (i == 1 && any(grepl("package", tolower(rows[[i]]))))
                        bg <- if (is_header) "#f4f6f9" else if (i %% 2 == 0) "#fdfdfd" else "white"
                        fw <- if (is_header) "bold" else "normal"

                        shiny::tags$tr(style = sprintf("background-color: %s; border-bottom: 1px solid #eee;", bg),
                                       lapply(rows[[i]], function(cell) {
                                         shiny::tags$td(style = sprintf("padding: 6px; font-weight: %s;", fw), cell)
                                       })
                        )
                      })
                    )
  )
}

# Updated to remove nested scrollbars (max-height and overflow removed)
create_card <- function(title, color, body_ui) {
  shiny::tags$div(
    style = sprintf("border: 2px solid %s; border-radius: 8px; overflow: hidden; margin-bottom: 15px; background: white;", color),
    shiny::tags$div(
      style = sprintf("background-color: %s; color: white; padding: 8px; font-weight: bold; text-align: center;", color),
      title
    ),
    shiny::tags$div(style = "padding: 10px;", body_ui)
  )
}
