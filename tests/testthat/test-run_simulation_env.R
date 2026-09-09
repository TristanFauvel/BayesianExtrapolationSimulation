test_that("run_simulation_env sets up log directories, skips both pipelines when disabled, and returns TRUE invisibly", {
  withr::local_dir(withr::local_tempdir())
  writeLines("parallelization: false", "scenarios_config.yml")

  # A stale checkpoint from a previous run: should be cleared on startup.
  dir.create("logs/test_env/checkpoints", recursive = TRUE)
  writeLines("stale", "logs/test_env/checkpoints/stale.RData")

  old_error_opt <- options("error")
  withr::defer(options(old_error_opt))
  old_appender <- futile.logger::flog.appender("ROOT")
  withr::defer(futile.logger::flog.appender(old_appender))

  result <- withVisible(run_simulation_env(
    env = "test_env",
    config_dir = "./",
    case_studies_config_dir = "./",
    simulation_config = list(
      compute_frequentist_ocs = FALSE,
      compute_bayesian_ocs_mc = FALSE
    ),
    analysis_config = list(),
    frequentist_metrics = list()
  ))

  expect_true(dir.exists("logs/test_env/checkpoints"))
  expect_true(dir.exists("logs/test_env/error_logs"))
  expect_false(file.exists("logs/test_env/checkpoints/stale.RData"))
  expect_true(result$value)
  expect_false(result$visible)
})


test_that("run_simulation_env dispatches to the frequentist pipeline when compute_frequentist_ocs is TRUE", {
  withr::local_dir(withr::local_tempdir())
  writeLines("parallelization: false", "scenarios_config.yml")
  dir.create("results/test_env", recursive = TRUE)
  writeLines("stale", "results/test_env/stale.csv")

  old_error_opt <- options("error")
  withr::defer(options(old_error_opt))
  old_appender <- futile.logger::flog.appender("ROOT")
  withr::defer(futile.logger::flog.appender(old_appender))

  calls <- new.env(parent = emptyenv())
  calls$freq <- 0L
  calls$concat_filenames <- character()
  calls$analysis <- 0L

  testthat::with_mocked_bindings(
    run_simulation_env(
      env = "test_env",
      config_dir = "./",
      case_studies_config_dir = "./",
      simulation_config = list(
        compute_frequentist_ocs = TRUE,
        compute_bayesian_ocs_mc = FALSE,
        delete_old_results = TRUE
      ),
      analysis_config = list(),
      frequentist_metrics = list()
    ),
    simulation_frequentist_ocs = function(...) {
      calls$freq <- calls$freq + 1L
      NULL
    },
    concatenate_simulation_results = function(results_dir, ocs_filename) {
      calls$concat_filenames <- c(calls$concat_filenames, ocs_filename)
      NULL
    },
    simulation_analysis = function(...) {
      calls$analysis <- calls$analysis + 1L
      NULL
    },
    .package = "RBExT"
  )

  expect_equal(calls$freq, 1L)
  expect_equal(calls$concat_filenames, "results_frequentist.csv")
  expect_equal(calls$analysis, 1L)
  # delete_old_results = TRUE recreates the results directory, clearing stale files.
  expect_true(dir.exists("results/test_env"))
  expect_false(file.exists("results/test_env/stale.csv"))
})


test_that("run_simulation_env dispatches to the Bayesian pipeline when compute_bayesian_ocs_mc is TRUE", {
  withr::local_dir(withr::local_tempdir())
  writeLines("parallelization: false", "scenarios_config.yml")

  old_error_opt <- options("error")
  withr::defer(options(old_error_opt))
  old_appender <- futile.logger::flog.appender("ROOT")
  withr::defer(futile.logger::flog.appender(old_appender))

  calls <- new.env(parent = emptyenv())
  calls$bayesian <- 0L
  calls$concat_filenames <- character()

  testthat::with_mocked_bindings(
    run_simulation_env(
      env = "test_env",
      config_dir = "./",
      case_studies_config_dir = "./",
      simulation_config = list(
        compute_frequentist_ocs = FALSE,
        compute_bayesian_ocs_mc = TRUE,
        delete_old_results = FALSE
      ),
      analysis_config = list(),
      frequentist_metrics = list()
    ),
    simulation_bayesian_ocs = function(...) {
      calls$bayesian <- calls$bayesian + 1L
      NULL
    },
    concatenate_simulation_results = function(results_dir, ocs_filename) {
      calls$concat_filenames <- c(calls$concat_filenames, ocs_filename)
      NULL
    },
    .package = "RBExT"
  )

  expect_equal(calls$bayesian, 1L)
  expect_equal(calls$concat_filenames, "results_bayesian_mc.csv")
})
