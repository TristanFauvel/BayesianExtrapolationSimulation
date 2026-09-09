## The post-processing analysis used to run single-threaded whatever the
## environment asked for: simulation_analysis() called
## frequentist_power_at_equivalent_tie() with parallelization = FALSE
## hardcoded, and that function's workers loaded RBExT with a bare
## library(RBExT), which fails when the package is only load_all()ed from
## source (main.R and the Shiny app both do exactly that).

test_that("analysis_runs_in_parallel reads both forms of the config entry", {
  expect_false(analysis_runs_in_parallel(NULL))
  expect_false(analysis_runs_in_parallel(FALSE))
  expect_true(analysis_runs_in_parallel(TRUE))

  # The per-method form: the analysis loops over every row at once, so any
  # method running in parallel means the analysis does too.
  expect_true(analysis_runs_in_parallel(list("RMP", "NPP")))
  expect_true(analysis_runs_in_parallel("RMP"))
})

test_that("analysis_runs_in_parallel rejects a malformed config entry", {
  expect_error(analysis_runs_in_parallel(c(TRUE, FALSE)), "single TRUE or FALSE")
  expect_error(analysis_runs_in_parallel(NA), "single TRUE or FALSE")
  expect_error(analysis_runs_in_parallel(list()), "single TRUE or FALSE")
  expect_error(analysis_runs_in_parallel(list(1, 2)), "single TRUE or FALSE")
})

test_that("simulation_analysis takes parallelization from the scenarios config", {
  expect_true("parallelization" %in% names(formals(simulation_analysis)))
  body_text <- paste(deparse(body(simulation_analysis)), collapse = " ")
  # The equivalent-TIE step must be handed the resolved setting, not FALSE.
  expect_match(body_text, "parallelization = run_in_parallel", fixed = TRUE)
})

test_that("workers can call RBExT functions when it is only loaded from source", {
  skip_on_cran()
  skip_if_not_installed("parallel")

  cl <- parallel::makeCluster(1L)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  load_rbext_in_workers(cl, packages = c("dplyr"))

  # get_parallel_worker_count() is internal to RBExT, so it only resolves in
  # the worker if the package itself really loaded there.
  worker_sees_rbext <- parallel::clusterEvalQ(cl, {
    is.function(RBExT:::get_parallel_worker_count) && is.function(dplyr::bind_rows)
  })
  expect_true(all(unlist(worker_sees_rbext)))
})

test_that("a small run skips the cluster, which costs more than it saves", {
  # ~12s to stand the cluster up against ~0.07s a row: below the threshold
  # the sequential path finishes first.
  expect_false(analysis_uses_cluster(TRUE, n_rows = 108))
  expect_false(analysis_uses_cluster(TRUE, n_rows = ANALYSIS_PARALLEL_MIN_ROWS - 1L))
  expect_true(analysis_uses_cluster(TRUE, n_rows = ANALYSIS_PARALLEL_MIN_ROWS))
  expect_true(analysis_uses_cluster(TRUE, n_rows = 10000))
})

test_that("the cluster is never used when parallelization is off", {
  expect_false(analysis_uses_cluster(FALSE, n_rows = 1e6))
  expect_false(analysis_uses_cluster(NULL, n_rows = 1e6))
  expect_false(analysis_uses_cluster(NA, n_rows = 1e6))
})

test_that("target data is validated once per row, not once per sampled alpha", {
  # compute_power_with_tie_ci() evaluates power at 1000 sampled alphas per
  # row; asserting inside compute_freq_power() made argument checking ~87% of
  # the analysis.
  hot <- paste(deparse(body(RBExT:::compute_freq_power)), collapse = " ")
  expect_false(grepl("assert_target_data_numbers", hot, fixed = TRUE))
  expect_false(grepl("assert_number", hot, fixed = TRUE))

  caller <- paste(deparse(body(RBExT:::compute_power_with_tie_ci)), collapse = " ")
  expect_true(grepl("assert_target_data_numbers", caller, fixed = TRUE))
})

test_that("assert_target_data_numbers still rejects malformed target data", {
  good <- list(treatment_effect = 0.3, standard_deviation = 1.1, sample_size_per_arm = 50)
  expect_silent(assert_target_data_numbers(good))

  expect_error(assert_target_data_numbers(utils::modifyList(good, list(treatment_effect = "a"))))
  expect_error(assert_target_data_numbers(utils::modifyList(good, list(standard_deviation = c(1, 2)))))
  expect_error(assert_target_data_numbers(list(treatment_effect = 0.3, standard_deviation = 1.1)))
})
