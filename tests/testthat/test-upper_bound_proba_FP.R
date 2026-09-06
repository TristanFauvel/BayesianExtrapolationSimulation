test_that("Monte Carlo false-positive bound forwards simulation context", {
  case_study_config <- list(
    name = "unit_test",
    endpoint = "continuous",
    summary_measure_likelihood = "normal",
    sampling_approximation = TRUE,
    source = list(
      control = 20,
      treatment = 20,
      treatment_effect = 0.2,
      standard_error = 0.1
    )
  )
  source_data <- SourceData$new(case_study_config)
  captured <- new.env(parent = emptyenv())
  model <- list(
    simulation_for_given_treatment_effect = function(target_data, ...) {
      captured$target_data <- target_data
      captured$arguments <- list(...)
      list(test_decisions = c(TRUE, FALSE))
    }
  )

  result <- upper_bound_proba_FP_MC(
    model = model,
    prior_proba_no_benefit = 0.4,
    source_data = source_data,
    theta_0 = 0,
    target_sample_size_per_arm = 10,
    case_study_config = case_study_config,
    target_to_source_std_ratio = 2,
    n_replicates = 2,
    confidence_level = 0.95,
    null_space = "left",
    critical_value = 0.975,
    case_study = "unit_test",
    method = "separate",
    n_samples_quantiles_estimation = 100
  )

  expect_equal(result, 0.2)
  expect_equal(captured$target_data$treatment_effect, 0)
  expect_equal(captured$arguments$case_study, "unit_test")
  expect_equal(captured$arguments$method, "separate")
  expect_equal(captured$arguments$n_samples_quantiles_estimation, 100)
})
