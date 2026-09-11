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


test_that("compile_stan_model rebuilds after changing the Stan source", {
  calls <- new.env(parent = emptyenv())

  result <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      compile_stan_model("changed_model", "parameters { real x; }"),
      cmdstan_model = function(...) {
        calls$args <- list(...)
        "compiled model"
      },
      .package = "cmdstanr"
    ),
    write_stan_file_if_changed = function(...) TRUE,
    .package = "RBExT"
  )

  expect_identical(result, "compiled model")
  expect_true(calls$args$force_recompile)
})


test_that("compile_stan_model caches the model outside the package library", {
  # inst/stan is excluded from the build (.Rbuildignore), so
  # system.file("stan", package = "RBExT") is "" for an installed copy and
  # pasting a model name onto it writes to the filesystem root. Even when the
  # directory does exist, a managed R installation keeps the library
  # read-only, so the compiled model cannot live there either.
  calls <- new.env(parent = emptyenv())

  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      compile_stan_model("cache_location_model", "parameters { real x; }"),
      cmdstan_model = function(...) {
        calls$args <- list(...)
        "compiled model"
      },
      .package = "cmdstanr"
    ),
    write_stan_file_if_changed = function(...) TRUE,
    .package = "RBExT"
  )

  cache_dir <- tools::R_user_dir("RBExT", "cache")

  expect_true(startsWith(calls$args[[1]], cache_dir))
  expect_true(startsWith(calls$args$exe_file, cache_dir))
})


test_that("clear_stan_model_cache empties the model cache", {
  cache <- withr::local_tempdir()
  file.create(file.path(cache, c("a_model.stan", "a_model.exe")))

  # inst/scripts/main.R clears the cache to force a recompile. It used to do
  # that by listing system.file("stan", package = "RBExT"), which stopped
  # being where the models live.
  removed <- clear_stan_model_cache(cache)

  expect_length(list.files(cache), 0)
  expect_length(removed, 2)
})


test_that("clear_stan_model_cache copes with a cache that does not exist yet", {
  absent <- file.path(withr::local_tempdir(), "never-created")

  expect_length(clear_stan_model_cache(absent), 0)
})
