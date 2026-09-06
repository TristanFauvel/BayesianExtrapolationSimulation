test_that("Monte Carlo Bayesian operating characteristics use explicit inputs", {
  test_model_class <- R6::R6Class(
    "BayesianOCTestModel",
    inherit = Model,
    public = list(
      simulation_for_given_treatment_effect = function(target_data,
                                                       n_replicates,
                                                       ...) {
        decisions <- if (target_data$treatment_effect <= 0) {
          rep(c(TRUE, FALSE), length.out = n_replicates)
        } else {
          rep(TRUE, n_replicates)
        }
        list(test_decisions = decisions)
      }
    )
  )

  case_study_config <- list(
    name = "unit_test",
    endpoint = "continuous",
    summary_measure_likelihood = "normal",
    sampling_approximation = TRUE,
    source = list(
      control = 20,
      treatment = 20,
      treatment_effect = 0,
      standard_error = 0.1
    )
  )
  source_data <- SourceData$new(case_study_config)
  design_prior <- list(
    sample = function(n) c(-0.1, 0.1),
    cdf = function(x) 0.5
  )

  result <- test_model_class$new()$estimate_bayesian_operating_characteristics(
    design_prior = design_prior,
    theta_0 = 0,
    source_data = source_data,
    n_replicates = 2,
    critical_value = 0.975,
    confidence_level = 0.95,
    null_space = "left",
    n_samples_design_prior = 2,
    target_sample_size_per_arm = 10,
    case_study_config = case_study_config,
    target_to_source_std_ratio = 1,
    simulation_config = list(),
    case_study = "unit_test",
    method = "separate",
    n_samples_quantiles_estimation = 100
  )

  expect_equal(result$prior_proba_success, 0.75)
  expect_equal(result$prior_proba_no_benefit, 0.5)
  expect_equal(result$prepost_proba_FP, 0.25)
  expect_equal(result$prepost_proba_TP, 0.5)
  expect_equal(result$average_tie, 0.5)
  expect_equal(result$average_power, 1)
  expect_equal(result$upper_bound_proba_FP, 0.25)
})
