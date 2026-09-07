# Nikolakopoulos et al (2018) test H0: mu = 0 by concluding mu > 0 if
# Pr(mu > 0 | D1) > eta, and equation (5) writes the resulting type I error in
# terms of z_{1-eta}. findCalibrationParameter takes 1 - eta, so the calibration
# only controls the error rate of the test the analysis actually performs when
# that argument matches the threshold the analysis decides at.
#
# The simulation decides at critical_value, so 1 - eta is 1 - critical_value.
# The method configuration's significance_level is 0.05, which is the right
# number only when the analysis decides at 0.95.

pdccpp_model_at <- function(critical_value) {
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
      initial_prior = list("noninformative"), desired_tie = list(0.065),
      significance_level = list(0.05), tolerance = list(1e-4), n_iter = list(1e6)
    ),
    source_data = source_data
  )
  model$analysis_critical_value <- critical_value
  list(model = model, source_data = source_data)
}

# The multiplier on the predictive standard deviation that the estimator
# actually applies, recovered by bisecting on the target estimate.
applied_multiplier <- function(model, source_data, n1, se_target) {
  tau2 <- se_target^2 * n1
  ess <- source_data$equivalent_source_sample_size_per_arm
  sv <- ess * source_data$standard_error^2
  n0 <- ess * tau2 / sv
  sigma_pr <- sqrt(tau2 / n0 + tau2 / n1)
  borrows_fully <- function(deviation) {
    model$power_parameter_estimation(ObservedTargetData$new(
      treatment_effect_estimate = source_data$treatment_effect_estimate + deviation,
      treatment_effect_standard_error = se_target,
      target_sample_size_per_arm = n1,
      summary_measure_likelihood = "normal"
    )) == 1
  }
  lower <- 0
  upper <- 20 * sigma_pr
  for (step in seq_len(200)) {
    middle <- (lower + upper) / 2
    if (borrows_fully(middle)) lower <- middle else upper <- middle
  }
  ((lower + upper) / 2) / sigma_pr
}

test_that("the calibration uses the threshold the analysis decides at", {
  n1 <- 52
  se_target <- 0.284
  for (critical_value in c(0.975, 0.95)) {
    fixture <- pdccpp_model_at(critical_value)
    tau2 <- se_target^2 * n1
    ess <- fixture$source_data$equivalent_source_sample_size_per_arm

    expected <- as.numeric(findCalibrationParameter(
      source_sample_size_per_arm = ess, target_sample_size_per_arm = n1,
      source_treatment_effect_estimate = fixture$source_data$treatment_effect_estimate,
      desired_tie = 0.065,
      significance_level = 1 - critical_value,
      target_data_sampling_variance = tau2,
      source_data_sampling_variance = ess * fixture$source_data$standard_error^2,
      tolerance = 1e-4, theta_0 = 0
    )[1, 2])

    expect_equal(
      applied_multiplier(fixture$model, fixture$source_data, n1, se_target),
      expected, tolerance = 1e-6,
      label = sprintf("applied multiplier at critical_value = %g", critical_value)
    )
  }
})

test_that("the simulation records the threshold it decides at", {
  fixture <- pdccpp_model_at(0.975)
  fixture$model$analysis_critical_value <- NULL
  target_data <- TargetDataFactory$new()$create(
    source_data = fixture$source_data,
    case_study_config = list(
      name = "unit_test", endpoint = "continuous",
      summary_measure_likelihood = "normal", sampling_approximation = TRUE,
      theta_0 = 0, null_space = "left",
      source = list(control = 562, treatment = 563,
                    treatment_effect = 0.481, standard_error = 0.1208)
    ),
    target_sample_size_per_arm = 52, control_drift = 0, treatment_drift = 0,
    summary_measure_likelihood = "normal", target_to_source_std_ratio = 1
  )
  set.seed(1)
  fixture$model$simulation_for_given_treatment_effect(
    target_data = target_data, n_replicates = 2, critical_value = 0.975,
    theta_0 = 0, confidence_level = 0.95, null_space = "left",
    case_study = "unit_test", method = "PDCCPP"
  )
  expect_equal(fixture$model$analysis_critical_value, 0.975)
})
