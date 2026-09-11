#' Directory the app reads and writes simulation output in
#'
#' @description The app, like `inst/scripts/main.R`, addresses its output with
#'   paths relative to the working directory (`./results/<env>/`,
#'   `./logs/<env>/`, `./user_configs/<env>/`). In a source checkout that
#'   should stay the checkout, so results land next to the ones `main.R`
#'   produces. Installed from a tarball there is no checkout, and defaulting to
#'   `getwd()` scattered output into whatever directory the user was sitting in.
#'
#' @param workspace Directory to use, or `NULL` to choose one.
#' @param default Directory to fall back on outside a source checkout.
#'
#' @return Path of the workspace, with its output directories created.
#' @noRd
rbext_workspace <- function(workspace = NULL,
                            default = tools::R_user_dir("RBExT", "data")) {
  if (is.null(workspace)) {
    workspace <- if (is_rbext_checkout(getwd())) getwd() else default
  }

  for (output in c("results", "logs", "user_configs")) {
    dir.create(
      file.path(workspace, output),
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

  normalizePath(workspace, mustWork = FALSE)
}


#' Is this directory an RBExT source checkout?
#'
#' @param directory Directory to test.
#'
#' @return `TRUE` when the directory holds RBExT's own DESCRIPTION.
#' @noRd
is_rbext_checkout <- function(directory) {
  description <- file.path(directory, "DESCRIPTION")
  if (!file.exists(description)) {
    return(FALSE)
  }

  package <- read.dcf(description, fields = "Package")[1, 1]
  identical(unname(package), "RBExT")
}


#' Launch the RBExT Shiny app
#'
#' @description Opens a browser-based app to configure a simulation study
#'   (case studies, methods, parameter grids), run it locally, and then
#'   analyze and visualize the results.
#'
#'   The app reads and writes `results/<env>/`, `logs/<env>/` and
#'   `user_configs/<env>/` inside `workspace`. Launched from a source checkout
#'   it uses the checkout, so results land next to the ones
#'   `Rscript inst/scripts/main.R` produces; installed from a release tarball
#'   it uses a per-user directory under [tools::R_user_dir()]. Either way the
#'   directory it settled on is reported when the app starts.
#'
#' @param workspace Directory holding the simulation output. Defaults to the
#'   repository root in a source checkout and to
#'   `tools::R_user_dir("RBExT", "data")` otherwise.
#' @param ... Passed on to `shiny::runApp()` (e.g. `port`, `launch.browser`).
#'
#' @return Does not return; runs the app until interrupted.
#' @export
run_rbext_app <- function(workspace = NULL, ...) {
  app_dir <- system.file("shiny_app", package = "RBExT")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. If you are developing ",
         "the package, make sure devtools::load_all() has been run first.")
  }

  workspace <- rbext_workspace(workspace)
  message("RBExT workspace: ", workspace)

  options(rbext.workspace = workspace)
  shiny::runApp(app_dir, ...)
}
