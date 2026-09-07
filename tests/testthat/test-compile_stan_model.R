test_that("write_stan_file_if_changed creates a file that does not exist yet", {
  path <- withr::local_tempfile(fileext = ".stan")

  changed <- write_stan_file_if_changed(path, "parameters { real x; }")

  expect_true(changed)
  expect_true(file.exists(path))
  expect_identical(readLines(path), "parameters { real x; }")
})


test_that("write_stan_file_if_changed leaves an unchanged file alone", {
  path <- withr::local_tempfile(fileext = ".stan")
  code <- "parameters { real x; }\nmodel { x ~ normal(0, 1); }"
  write_stan_file_if_changed(path, code)

  # cmdstanr decides whether to recompile by comparing timestamps, so rewriting
  # identical code would force a rebuild on every model construction.
  old_time <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(path, old_time)

  changed <- write_stan_file_if_changed(path, code)

  expect_false(changed)
  expect_equal(as.numeric(file.mtime(path)), as.numeric(old_time), tolerance = 1)
})


test_that("write_stan_file_if_changed rewrites when the model code changes", {
  path <- withr::local_tempfile(fileext = ".stan")
  write_stan_file_if_changed(path, "parameters { real x; }")
  Sys.setFileTime(path, as.POSIXct("2020-01-01 00:00:00", tz = "UTC"))

  changed <- write_stan_file_if_changed(path, "parameters { real y; }")

  expect_true(changed)
  expect_identical(readLines(path), "parameters { real y; }")
  expect_gt(file.mtime(path), as.POSIXct("2021-01-01 00:00:00", tz = "UTC"))
})


test_that("write_stan_file_if_changed handles code given as multiple lines", {
  path <- withr::local_tempfile(fileext = ".stan")
  code <- c("parameters {", "  real x;", "}")

  expect_true(write_stan_file_if_changed(path, code))
  expect_identical(readLines(path), code)
  expect_false(write_stan_file_if_changed(path, code))
})
