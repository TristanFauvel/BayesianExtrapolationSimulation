shortcut_in <- function(workspace = NULL) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  create_bexte_shortcut(
    workspace = workspace,
    applications_dir = file.path(root, "applications"),
    script_dir = file.path(root, "data")
  )
}


test_that("create_bexte_shortcut writes a valid desktop entry", {
  skip_if(!nzchar(Sys.which("desktop-file-validate")), "desktop-file-validate not installed")

  paths <- shortcut_in()

  # Validating beats matching strings: the spec has rules about required keys
  # and category names that a hand-written entry quietly gets wrong.
  expect_equal(
    system2("desktop-file-validate", shQuote(paths[["entry"]]),
            stdout = TRUE, stderr = TRUE),
    character(0)
  )
})


test_that("create_bexte_shortcut points the entry at an executable launcher", {
  paths <- shortcut_in()

  entry <- readLines(paths[["entry"]])
  exec <- sub("^Exec=", "", grep("^Exec=", entry, value = TRUE))

  expect_identical(exec, paths[["launcher"]])
  expect_true(file.exists(exec))
  expect_equal(file.access(exec, mode = 1)[[1]], 0L)
})


test_that("create_bexte_shortcut launches the app through Rscript", {
  paths <- shortcut_in()

  launcher <- readLines(paths[["launcher"]])

  expect_match(launcher[[1]], "^#!/bin/sh")
  # The .desktop Exec key is not run through a shell, so the R call has to live
  # in the script rather than in the entry itself.
  expect_true(any(grepl("Rscript", launcher, fixed = TRUE)))
  expect_true(any(grepl("run_bexte_app(launch.browser = TRUE)", launcher, fixed = TRUE)))
})


test_that("create_bexte_shortcut embeds an explicit workspace", {
  paths <- shortcut_in(workspace = "/srv/bexte-studies")

  launcher <- readLines(paths[["launcher"]])

  expect_true(any(grepl('workspace = "/srv/bexte-studies"', launcher, fixed = TRUE)))
})


test_that("create_bexte_shortcut refuses a workspace it cannot quote", {
  # The R call is passed as a single-quoted -e argument, so a quote in the path
  # would end the argument early and run something else entirely.
  expect_error(
    shortcut_in(workspace = "/srv/o'brien"),
    "single quote"
  )
})


test_that("create_bexte_shortcut uses the packaged icon", {
  paths <- shortcut_in()

  entry <- readLines(paths[["entry"]])
  icon <- sub("^Icon=", "", grep("^Icon=", entry, value = TRUE))

  expect_true(file.exists(icon))
  expect_identical(basename(icon), "favicon.svg")
})


test_that("create_bexte_shortcut pins the library BExTE was found in", {
  paths <- shortcut_in()

  launcher <- readLines(paths[["launcher"]])

  # Rscript otherwise searches only the default library, so the shortcut breaks
  # for anyone whose BExTE was installed into renv or a custom R_LIBS_USER.
  expect_true(any(grepl(
    dirname(system.file(package = "BExTE")),
    launcher,
    fixed = TRUE
  )))
})
