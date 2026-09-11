test_that("rbext_workspace creates the directories the app writes to", {
  root <- withr::local_tempdir()

  workspace <- rbext_workspace(file.path(root, "study"))

  # The app and every RBExT entry point address these with paths relative to
  # the working directory ("./results/<env>/"), so they have to exist before
  # the app switches into the workspace.
  expect_true(dir.exists(file.path(workspace, "results")))
  expect_true(dir.exists(file.path(workspace, "logs")))
  expect_true(dir.exists(file.path(workspace, "user_configs")))
})


test_that("rbext_workspace keeps a source checkout as the workspace", {
  checkout <- withr::local_tempdir()
  writeLines(c("Package: RBExT", "Version: 0.0.2"), file.path(checkout, "DESCRIPTION"))
  withr::local_dir(checkout)

  # Developers run the app from the repository root and expect results to land
  # in the checkout they are working in, next to the ones main.R produces.
  expect_identical(
    rbext_workspace(default = file.path(checkout, "unused")),
    normalizePath(checkout, mustWork = FALSE)
  )
})


test_that("rbext_workspace falls back to the user data directory elsewhere", {
  elsewhere <- withr::local_tempdir()
  fallback <- withr::local_tempdir()
  withr::local_dir(elsewhere)

  # Installed from a tarball there is no checkout, and the previous behaviour
  # scattered results/, logs/ and user_configs/ into whatever directory the
  # user happened to be sitting in.
  expect_identical(
    rbext_workspace(default = fallback),
    normalizePath(fallback, mustWork = FALSE)
  )
})


test_that("rbext_workspace does not treat another package's checkout as RBExT", {
  other <- withr::local_tempdir()
  writeLines(c("Package: ggplot2", "Version: 3.5.1"), file.path(other, "DESCRIPTION"))
  fallback <- withr::local_tempdir()
  withr::local_dir(other)

  expect_identical(
    rbext_workspace(default = fallback),
    normalizePath(fallback, mustWork = FALSE)
  )
})
