# findCalibrationParameter bounded its search by maxZ_1_m_c2, and short-circuited
# to it when full borrowing already attained the desired type I error.
#
# maxZ_1_m_c2 is the cut-off at which the lower discounting boundary meets the
# full-borrowing rejection boundary, so it is the widest cut-off at which
# discounting *below* the source estimate still touches the rejection region. It
# is negative whenever the source estimate itself sits below that boundary,
# which is most of the parameter space -- and it is not a bound on the search in
# any case, because the type I error also varies with the cut-off through the
# upper branch. Bounding the search by a negative value inverted the bisection
# interval and fed a negative cut-off to the type I error integrand; returning it
# handed PDCCPP a cut-off that discounts everything, the opposite of the "no
# discounting needed" case it stands for.
#
# The admissible cut-offs are (0, 1]. 1 is the cut-off Gaussian_Gravestock_EBPP
# fixes, and PDCCPP is that method with the cut-off calibrated downward.

calibrate <- function(source_sample_size_per_arm,
                      target_sample_size_per_arm,
                      source_treatment_effect_estimate,
                      target_data_sampling_variance,
                      source_data_sampling_variance,
                      desired_tie = 0.065) {
  as.numeric(findCalibrationParameter(
    source_sample_size_per_arm = source_sample_size_per_arm,
    target_sample_size_per_arm = target_sample_size_per_arm,
    source_treatment_effect_estimate = source_treatment_effect_estimate,
    desired_tie = desired_tie, significance_level = 0.05,
    target_data_sampling_variance = target_data_sampling_variance,
    source_data_sampling_variance = source_data_sampling_variance,
    tolerance = 1e-4, theta_0 = 0
  )[1, 2])
}

test_that("the calibration searches when the source estimate is below the full-borrowing rejection boundary", {
  # maxZ_1_m_c2 is -0.511 here while full borrowing attains 0.163, well above
  # the target, so there is a genuine cut-off to find: the type I error crosses
  # 0.065 between 0.5 and 0.75. The search used to bisect over [0, -0.511].
  calibration_parameter <- calibrate(1500, 200, 0.2, 40, 30)

  expect_gt(calibration_parameter, 0)
  expect_lte(calibration_parameter, 1)
  expect_equal(
    adaptive_power_prior_type_I_error(
      calibration_parameter = calibration_parameter,
      source_sample_size_per_arm = 1500, target_sample_size_per_arm = 200,
      source_treatment_effect_estimate = 0.2, significance_level = 0.05,
      target_data_sampling_variance = 40, source_data_sampling_variance = 30,
      theta_0 = 0
    ),
    0.065, tolerance = 1e-3
  )
})

test_that("the calibration always returns an admissible cut-off", {
  set.seed(20240908)
  for (case in seq_len(200)) {
    source_sample_size_per_arm <- runif(1, 5, 2000)
    target_sample_size_per_arm <- round(runif(1, 10, 500))
    target_data_sampling_variance <- runif(1, 0.05, 40)
    source_data_sampling_variance <- runif(1, 0.05, 40)
    source_treatment_effect_estimate <- runif(1, -5, 5)

    calibration_parameter <- calibrate(
      source_sample_size_per_arm, target_sample_size_per_arm,
      source_treatment_effect_estimate,
      target_data_sampling_variance, source_data_sampling_variance
    )

    label <- sprintf(
      "cut-off for source estimate %g, n0 = %g, n1 = %g, tau2 = %g, sv = %g",
      source_treatment_effect_estimate, source_sample_size_per_arm,
      target_sample_size_per_arm, target_data_sampling_variance,
      source_data_sampling_variance
    )
    expect_true(is.finite(calibration_parameter) && calibration_parameter > 0,
                label = label)

    # Admissibility alone is weak: the cut-off also has to do its job. The
    # bisection assumes the type I error is monotone in the cut-off, which it is
    # not everywhere, so this pins that it lands on a cut-off that controls the
    # error rather than merely a legal one.
    expect_lt(
      adaptive_power_prior_type_I_error(
        calibration_parameter = calibration_parameter,
        source_sample_size_per_arm = source_sample_size_per_arm,
        target_sample_size_per_arm = target_sample_size_per_arm,
        source_treatment_effect_estimate = source_treatment_effect_estimate,
        significance_level = 0.05,
        target_data_sampling_variance = target_data_sampling_variance,
        source_data_sampling_variance = source_data_sampling_variance,
        theta_0 = 0
      ),
      0.065 + 1e-4,
      label = sub("^cut-off", "type I error attained by the cut-off", label)
    )
  }
})

test_that("PDCCPP borrows instead of aborting when the source estimate is conservative", {
  # The original symptom. With a right null space the transformation negates the
  # source estimate, putting it on the conservative side of the boundary, so the
  # calibration short-circuited and PDCCPP rejected the cut-off it got back.
  case_study_config <- list(
    name = "unit_test", endpoint = "continuous",
    summary_measure_likelihood = "normal", sampling_approximation = TRUE,
    theta_0 = 0, null_space = "right",
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

  power_parameter <- model$power_parameter_estimation(ObservedTargetData$new(
    treatment_effect_estimate = 0.481,
    treatment_effect_standard_error = 0.284,
    target_sample_size_per_arm = 52,
    summary_measure_likelihood = "normal"
  ))

  expect_true(power_parameter > 0 && power_parameter <= 1)
})

# The reference implementation returns As -- the cut-off at which the type I
# error equals that of full borrowing -- whenever full borrowing already attains
# the desired error: "if (t1<aD){S <- As}". As is a number of predictive
# standard deviations, not a probability, and the reference returns it
# uncapped. These two pin that contract.

reference_domain_fixture <- function() {
  case_study_config <- list(
    name = "unit_test", endpoint = "continuous",
    summary_measure_likelihood = "normal", sampling_approximation = TRUE,
    theta_0 = 0, null_space = "left",
    source = list(control = 562, treatment = 563,
                  treatment_effect = 2.1148, standard_error = 1.736)
  )
  source_data <- SourceData$new(case_study_config)
  target_sample_size_per_arm <- 474
  target_standard_error <- 0.1809
  target_data_sampling_variance <-
    target_standard_error^2 * target_sample_size_per_arm
  source_data_sampling_variance <-
    source_data$equivalent_source_sample_size_per_arm * source_data$standard_error^2
  prior_sample_size <- source_data$equivalent_source_sample_size_per_arm *
    target_data_sampling_variance / source_data_sampling_variance

  list(
    case_study_config = case_study_config,
    source_data = source_data,
    target_sample_size_per_arm = target_sample_size_per_arm,
    target_standard_error = target_standard_error,
    target_data_sampling_variance = target_data_sampling_variance,
    source_data_sampling_variance = source_data_sampling_variance,
    std_predictive_dist = sqrt(
      target_data_sampling_variance / prior_sample_size +
        target_data_sampling_variance / target_sample_size_per_arm
    )
  )
}

test_that("the calibration returns the full-borrowing cut-off uncapped when borrowing is already safe", {
  fixture <- reference_domain_fixture()
  calibration <- findCalibrationParameter(
    source_sample_size_per_arm = fixture$source_data$equivalent_source_sample_size_per_arm,
    target_sample_size_per_arm = fixture$target_sample_size_per_arm,
    source_treatment_effect_estimate = fixture$source_data$treatment_effect_estimate,
    desired_tie = 0.065, significance_level = 0.05,
    target_data_sampling_variance = fixture$target_data_sampling_variance,
    source_data_sampling_variance = fixture$source_data_sampling_variance,
    tolerance = 1e-4, theta_0 = 0
  )
  calibration_parameter <- as.numeric(calibration[1, 2])
  maximum <- as.numeric(calibration[3, 2])
  full_borrowing_type_I_error <- as.numeric(calibration[5, 2])

  # This configuration is inside the reference's domain and full borrowing is
  # already safe, so the reference short-circuits to As -- which here is wider
  # than one predictive standard deviation.
  expect_lt(full_borrowing_type_I_error, 0.065)
  expect_gt(maximum, 1)
  expect_equal(calibration_parameter, maximum)
})

test_that("PDCCPP borrows fully out to a calibrated cut-off wider than one predictive standard deviation", {
  fixture <- reference_domain_fixture()
  model <- Model$new()$create(
    case_study_config = fixture$case_study_config, method = "PDCCPP",
    method_parameters = list(
      initial_prior = list("noninformative"), desired_tie = list(0.065),
      significance_level = list(0.05), tolerance = list(1e-4), n_iter = list(1e6)
    ),
    source_data = fixture$source_data
  )

  # 1.03 predictive standard deviations from the source estimate: outside a
  # cut-off of 1, inside the calibrated cut-off of about 1.053.
  power_parameter <- model$power_parameter_estimation(ObservedTargetData$new(
    treatment_effect_estimate = fixture$source_data$treatment_effect_estimate +
      1.03 * fixture$std_predictive_dist,
    treatment_effect_standard_error = fixture$target_standard_error,
    target_sample_size_per_arm = fixture$target_sample_size_per_arm,
    summary_measure_likelihood = "normal"
  ))

  expect_equal(power_parameter, 1)
})
