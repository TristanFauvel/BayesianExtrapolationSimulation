test_that("deterministic Bayesian OC analysis handles an empty result set", {
  empty_results <- data.frame(case_study = character())

  result <- RBExT:::compute_bayesian_ocs(
    results_freq_df = empty_results,
    env = "pipeline_tests"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
