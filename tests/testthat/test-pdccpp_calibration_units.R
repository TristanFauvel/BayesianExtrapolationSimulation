# findCalibrationParameter reports its answer as a z-score: internally it forms
# the tail probability B = 2 * pnorm(-Z) and then takes qnorm(1 - B / 2), which
# returns Z, so Z itself is the number of predictive standard deviations at
# which full borrowing stops. The reference implementation in Nikolakopoulos
# et al (2018) confirms it: the borrowing weight there is
#   ifelse(X > m0 + sP * qnorm(1 - B / 2) | X < m0 + sP * qnorm(B / 2), ...)
# with B = 2 * pnorm(-S), so the multiplier on the predictive standard
# deviation sP is S itself -- the z-score of a two-sided (1 - c) prediction
# interval around the source estimate. PDCCPP has to apply that same cut-off. The shared
# estimator formula it borrows from Gaussian_Gravestock_EBPP is written in terms
# of a tail probability instead -- Gravestock passes 2 * (1 - pnorm(1)), whose
# qnorm(1 - . / 2) is 1 -- so handing it a z-score converts it a second time.
# These tests pin the cut-off that is actually applied to the one calibrated.

pdccpp_fixture <- function(desired_tie = 0.065,
                           target_sample_size_per_arm = 52,
                           target_standard_error = 0.284) {
  case_study_config <- list(
    name = "unit_test", endpoint = "continuous",
    summary_measure_likelihood = "normal", sampling_approximation = TRUE,
    theta_0 = 0, null_space = "left",
    source = list(control = 562, treatment = 563,
                  treatment_effect = 0.481, standard_error = 0.1208)
  )
  source_data <- SourceData$new(case_study_config)
  model <- Model$new()$create(
    case_study_config = case_study_config, method = "PDCCPP",
    method_parameters = list(
      initial_prior = list("noninformative"),
      desired_tie = list(desired_tie), significance_level = list(0.05),
      tolerance = list(1e-4), n_iter = list(1e6)
    ),
    source_data = source_data
  )

  target_data_at <- function(treatment_effect_estimate) {
    ObservedTargetData$new(
      treatment_effect_estimate = treatment_effect_estimate,
      treatment_effect_standard_error = target_standard_error,
      target_sample_size_per_arm = target_sample_size_per_arm,
      summary_measure_likelihood = "normal"
    )
  }

  target_data_sampling_variance <-
    target_standard_error^2 * target_sample_size_per_arm
  source_data_sampling_variance <-
    source_data$equivalent_source_sample_size_per_arm * source_data$standard_error^2
  equivalent_target_sample_size <- source_data$equivalent_source_sample_size_per_arm *
    target_data_sampling_variance / source_data_sampling_variance

  calibration_parameter <- as.numeric(findCalibrationParameter(
    source_sample_size_per_arm = source_data$equivalent_source_sample_size_per_arm,
    target_sample_size_per_arm = target_sample_size_per_arm,
    source_treatment_effect_estimate = source_data$treatment_effect_estimate,
    desired_tie = desired_tie, significance_level = 0.05,
    target_data_sampling_variance = target_data_sampling_variance,
    source_data_sampling_variance = source_data_sampling_variance,
    tolerance = 1e-4, theta_0 = 0
  )[1, 2])

  list(
    model = model,
    source_data = source_data,
    target_data_at = target_data_at,
    calibration_parameter = calibration_parameter,
    desired_tie = desired_tie,
    source_sample_size_per_arm = source_data$equivalent_source_sample_size_per_arm,
    target_sample_size_per_arm = target_sample_size_per_arm,
    target_data_sampling_variance = target_data_sampling_variance,
    source_data_sampling_variance = source_data_sampling_variance,
    std_predictive_dist = sqrt(
      target_data_sampling_variance / equivalent_target_sample_size +
        target_data_sampling_variance / target_sample_size_per_arm
    )
  )
}

test_that("PDCCPP stops borrowing in full at the cut-off the search calibrated", {
  fixture <- pdccpp_fixture()
  cutoff <- fixture$std_predictive_dist * fixture$calibration_parameter

  just_inside <- fixture$source_data$treatment_effect_estimate + 0.999 * cutoff
  just_outside <- fixture$source_data$treatment_effect_estimate + 1.001 * cutoff

  expect_equal(
    fixture$model$power_parameter_estimation(fixture$target_data_at(just_inside)), 1
  )
  expect_lt(
    fixture$model$power_parameter_estimation(fixture$target_data_at(just_outside)), 1
  )
})

test_that("PDCCPP attains the type I error it was calibrated for", {
  fixture <- pdccpp_fixture()

  # Recover the cut-off the estimator actually applies, by bisecting on the
  # target estimate for the point where full borrowing stops.
  borrows_fully <- function(deviation) {
    estimate <- fixture$source_data$treatment_effect_estimate + deviation
    fixture$model$power_parameter_estimation(fixture$target_data_at(estimate)) == 1
  }
  lower <- 0
  upper <- 20 * fixture$std_predictive_dist
  stopifnot(borrows_fully(lower), !borrows_fully(upper))
  for (step in seq_len(200)) {
    middle <- (lower + upper) / 2
    if (borrows_fully(middle)) lower <- middle else upper <- middle
  }
  applied_multiplier <- ((lower + upper) / 2) / fixture$std_predictive_dist

  expect_equal(applied_multiplier, fixture$calibration_parameter, tolerance = 1e-6)

  attained <- adaptive_power_prior_type_I_error(
    calibration_parameter = applied_multiplier,
    source_sample_size_per_arm = fixture$source_sample_size_per_arm,
    target_sample_size_per_arm = fixture$target_sample_size_per_arm,
    source_treatment_effect_estimate = fixture$source_data$treatment_effect_estimate,
    significance_level = 0.05,
    target_data_sampling_variance = fixture$target_data_sampling_variance,
    source_data_sampling_variance = fixture$source_data_sampling_variance,
    theta_0 = 0
  )
  expect_equal(attained, fixture$desired_tie, tolerance = 1e-3)
})

test_that("Gravestock's EBPP applies a cut-off of one predictive standard deviation", {
  # This is the anchor for the units above. Gravestock fixes the cut-off at
  # 2 * (1 - pnorm(1)) as a tail probability, whose qnorm(1 - . / 2) is exactly
  # 1, and the comment on its estimator states that PDCCPP is the same method
  # with the cut-off calibrated rather than fixed. So a calibration parameter of
  # 1 has to mean the same cut-off in both, which it does only when PDCCPP
  # applies its z-score directly.
  case_study_config <- list(
    name = "unit_test", endpoint = "continuous",
    summary_measure_likelihood = "normal", sampling_approximation = TRUE,
    theta_0 = 0, null_space = "left",
    source = list(control = 562, treatment = 563,
                  treatment_effect = 0.481, standard_error = 0.1208)
  )
  source_data <- SourceData$new(case_study_config)
  model <- Model$new()$create(
    case_study_config = case_study_config, method = "EB_PP",
    method_parameters = list(initial_prior = list("noninformative")),
    source_data = source_data
  )

  target_sample_size_per_arm <- 52
  target_standard_error <- 0.284
  target_data_sampling_variance <-
    target_standard_error^2 * target_sample_size_per_arm
  source_data_sampling_variance <-
    source_data$equivalent_source_sample_size_per_arm * source_data$standard_error^2
  equivalent_target_sample_size <- source_data$equivalent_source_sample_size_per_arm *
    target_data_sampling_variance / source_data_sampling_variance
  std_predictive_dist <- sqrt(
    target_data_sampling_variance / equivalent_target_sample_size +
      target_data_sampling_variance / target_sample_size_per_arm
  )

  borrows_fully <- function(deviation) {
    model$power_parameter_estimation(ObservedTargetData$new(
      treatment_effect_estimate =
        source_data$treatment_effect_estimate + deviation,
      treatment_effect_standard_error = target_standard_error,
      target_sample_size_per_arm = target_sample_size_per_arm,
      summary_measure_likelihood = "normal"
    )) == 1
  }
  lower <- 0
  upper <- 20 * std_predictive_dist
  for (step in seq_len(200)) {
    middle <- (lower + upper) / 2
    if (borrows_fully(middle)) lower <- middle else upper <- middle
  }

  expect_equal(((lower + upper) / 2) / std_predictive_dist, 1, tolerance = 1e-6)
})
