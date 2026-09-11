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


#' Add a desktop shortcut that launches the app
#'
#' @description Writes an application-menu entry that starts the Shiny app, so
#'   the app can be launched without an R session. It shortens *launching*, not
#'   *installing*: R, RBExT and CmdStan all still have to be present, which is
#'   what `inst/scripts/install.R` is for.
#'
#'   Two files are written. A shell script holds the R call, because the
#'   `Exec` key of a desktop entry is split on whitespace rather than parsed by
#'   a shell, so quotes around an `-e` argument would reach R intact. The entry
#'   then points at that script.
#'
#'   The entry runs in a terminal on purpose. The app is a server that runs
#'   until it is stopped, and the terminal is what carries the workspace path,
#'   the progress of a run, and Ctrl+C as the way to stop it.
#'
#' @param workspace Workspace to launch with, or `NULL` to let
#'   [run_rbext_app()] choose one.
#' @param applications_dir Directory holding desktop entries. The default puts
#'   the entry in the application menu, which, unlike one placed on the
#'   desktop, needs no separate step to mark it trusted.
#' @param script_dir Directory to write the launcher script into.
#'
#' @return Paths of the launcher and the desktop entry, invisibly.
#' @export
create_rbext_shortcut <- function(workspace = NULL,
                                  applications_dir = path.expand("~/.local/share/applications"),
                                  script_dir = tools::R_user_dir("RBExT", "data")) {
  if (!is.null(workspace) && grepl("'", workspace, fixed = TRUE)) {
    stop(
      "The workspace path cannot contain a single quote, because the launcher ",
      "passes the R call as a single-quoted argument: ", workspace
    )
  }

  dir.create(applications_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(script_dir, showWarnings = FALSE, recursive = TRUE)

  arguments <- if (is.null(workspace)) {
    "launch.browser = TRUE"
  } else {
    sprintf('workspace = "%s", launch.browser = TRUE', workspace)
  }

  # Rscript searches only the default library, which is not where RBExT lives
  # when it was installed into renv or a custom R_LIBS_USER, so record the
  # library it was actually found in.
  library_path <- dirname(system.file(package = "RBExT"))

  launcher <- file.path(script_dir, "launch-rbext.sh")
  writeLines(
    c(
      "#!/bin/sh",
      "# Written by RBExT::create_rbext_shortcut(). Re-run that rather than",
      "# editing this file, which is overwritten.",
      sprintf(
        "exec \"%s\" -e '.libPaths(c(\"%s\", .libPaths())); RBExT::run_rbext_app(%s)'",
        file.path(R.home("bin"), "Rscript"),
        library_path,
        arguments
      )
    ),
    launcher
  )
  Sys.chmod(launcher, "0755")

  entry <- file.path(applications_dir, "rbext.desktop")
  writeLines(
    c(
      "[Desktop Entry]",
      "Type=Application",
      "Name=RBExT",
      "Comment=Configure, run and analyse Bayesian extrapolation simulation studies",
      paste0("Exec=", launcher),
      paste0("Icon=", system.file("shiny_app", "www", "favicon.svg", package = "RBExT")),
      "Terminal=true",
      "Categories=Science;"
    ),
    entry
  )

  # Without this the entry can take until the next login to show up in the menu.
  if (nzchar(Sys.which("update-desktop-database"))) {
    system2(
      "update-desktop-database",
      shQuote(applications_dir),
      stdout = FALSE,
      stderr = FALSE
    )
  }

  invisible(c(launcher = launcher, entry = entry))
}
