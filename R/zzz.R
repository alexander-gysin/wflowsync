.onAttach <- function(libname, pkgname) {
  # We use packageStartupMessage to comply with standard R package behavior,
  # but wrap it in cli::format_inline so we get those nice colors and formatting!
  msg <- ("Welcome to wflowsync! Run sync_setup() to configure GitHub credentials, initialize workflowr, and renv.")
  packageStartupMessage(msg)
}
