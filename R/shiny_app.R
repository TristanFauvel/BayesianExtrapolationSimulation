#' Launch the RBExT Shiny app
#'
#' @description Opens a browser-based app to configure a simulation study
#'   (case studies, methods, parameter grids), run it locally, and then
#'   analyze and visualize the results. Run this with your working directory
#'   set to the repository root (the same way `Rscript inst/scripts/main.R`
#'   is normally run) - the app reads and writes `./results/<env>/`,
#'   `./logs/<env>/` and `./user_configs/<env>/` relative to it.
#'
#' @param ... Passed on to `shiny::runApp()` (e.g. `port`, `launch.browser`).
#'
#' @return Does not return; runs the app until interrupted.
#' @export
run_rbext_app <- function(...) {
  app_dir <- system.file("shiny_app", package = "RBExT")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. If you are developing ",
         "the package, make sure devtools::load_all() has been run first.")
  }

  options(rbext.repo_root = getwd())
  shiny::runApp(app_dir, ...)
}
