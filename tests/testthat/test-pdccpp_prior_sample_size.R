# Nikolakopoulos et al (2018) carry a single prior sample size n0, defined by
# the prior variance: "We assume a prior for mu, mu|sigma^2 ~ N(mu_0,
# sigma^2/n0)". Everything else follows from it -- the predictive standard
# deviation sigma_pr = sqrt(sigma^2/n0 + sigma^2/n1), the discount factor
# gamma = (sigma^2/n0) / (((X-mu_0)/z)^2 - sigma^2/n1), and the posterior
# N(., sigma^2/(gamma*n0 + n1)) of equation (6).
#
# Here sigma^2 is the target sampling variance and the prior variance is the
# source standard error squared, so n0 = sigma^2 / se_source^2. These tests
# pin the consequences of that, which is what makes the calibration describe
# the posterior the model actually forms.

pdccpp_design <- function(target_sample_size_per_arm = 52,
                          target_standard_error = 0.284) {
  case_study_config <- list(
    name = "unit_test", endpoint = "continuous",
    summary_measure_likelihood = "normal", sampling_approximation = TRUE,
    theta_0 = 0, null_space = "left",
    source = list(control = 562, treatment = 563,
                  treatment_effect = 0.481, standard_error = 0.1208)
  )
  source_data <- SourceData$new(case_study_config)
  list(
    case_study_config = case_study_config,
    source_data = source_data,
    n1 = target_sample_size_per_arm,
    se_target = target_standard_error,
    se_source = source_data$standard_error,
    tau2 = target_standard_error^2 * target_sample_size_per_arm,
    ess = source_data$equivalent_source_sample_size_per_arm
  )
}

test_that("the predictive standard deviation is prior plus sampling variance", {
  d <- pdccpp_design()
  calibration <- findCalibrationParameter(
    source_sample_size_per_arm = d$ess,
    target_sample_size_per_arm = d$n1,
    source_treatment_effect_estimate = d$source_data$treatment_effect_estimate,
    desired_tie = 0.065, significance_level = 0.05,
    target_data_sampling_variance = d$tau2,
    source_data_sampling_variance = d$ess * d$se_source^2,
    tolerance = 1e-4, theta_0 = 0
  )
  # sigma_pr^2 = sigma^2/n0 + sigma^2/n1 = se_source^2 + se_target^2.
  expect_equal(as.numeric(calibration[2, 2]),
               sqrt(d$se_source^2 + d$se_target^2),
               tolerance = 1e-10)
})

test_that("the calibration describes the posterior the model actually forms", {
  d <- pdccpp_design()
  model <- Model$new()$create(
    case_study_config = d$case_study_config, method = "PDCCPP",
    method_parameters = list(
      initial_prior = list("noninformative"), desired_tie = list(0.065),
      significance_level = list(0.05), tolerance = list(1e-4), n_iter = list(1e6)
    ),
    source_data = d$source_data
  )
  target_data <- ObservedTargetData$new(
    treatment_effect_estimate = 0.30,
    treatment_effect_standard_error = d$se_target,
    target_sample_size_per_arm = d$n1,
    summary_measure_likelihood = "normal"
  )
  gamma <- model$power_parameter_estimation(target_data)
  model$inference(target_data = target_data)

  # The model discounts a prior whose variance is the source standard error
  # squared, so after inference its prior variance is se_source^2 / gamma.
  # This is the premise that fixes n0: equation (6) writes the same prior
  # variance as sigma^2 / (gamma * n0), so n0 must be sigma^2 / se_source^2.
  expect_equal(model$prior_var * gamma, d$se_source^2, tolerance = 1e-10)

  n0 <- d$tau2 / d$se_source^2
  expect_equal(d$tau2 / (gamma * n0), model$prior_var, tolerance = 1e-10)
  # n0 is not the effective source sample size, and not the target's either.
  expect_false(isTRUE(all.equal(n0, d$ess)))
  expect_false(isTRUE(all.equal(n0, d$n1)))
})
