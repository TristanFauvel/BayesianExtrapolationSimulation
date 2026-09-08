# hypothesis_space_transformation() translates every treatment effect so that
# the boundary of the null hypothesis space sits at 0, and PDCCPP hands the
# translated source estimate to findCalibrationParameter(). The calibration's
# theta_0 argument is the mean of the target estimate's sampling distribution
# under the null, so on that translated scale it is 0, not the original
# boundary. Passing the original integrated the type I error at a point the
# calibrated decision rule is not centred on. Every case study configuration
# sets theta_0 = 0, which hides it. The method is defined relative to the
# boundary, so shifting the boundary and every treatment effect together must
# leave the borrowing weight unchanged.

pdccpp_model <- function(theta_0, source_treatment_effect, null_space) {
  case_study_config <- list(
    name = "unit_test", endpoint = "continuous",
    summary_measure_likelihood = "normal", sampling_approximation = TRUE,
    theta_0 = theta_0, null_space = null_space,
    source = list(control = 562, treatment = 563,
                  treatment_effect = source_treatment_effect,
                  standard_error = 0.1208)
  )
  source_data <- SourceData$new(case_study_config)
  Model$new()$create(
    case_study_config = case_study_config, method = "PDCCPP",
    method_parameters = list(
      initial_prior = list("noninformative"), desired_tie = list(0.065),
      significance_level = list(0.05), tolerance = list(1e-4), n_iter = list(1e6)
    ),
    source_data = source_data
  )
}

pdccpp_target_data_at <- function(treatment_effect_estimate) {
  ObservedTargetData$new(
    treatment_effect_estimate = treatment_effect_estimate,
    treatment_effect_standard_error = 0.284,
    target_sample_size_per_arm = 52,
    summary_measure_likelihood = "normal"
  )
}

# Estimates spanning the full-borrowing region and both discounted tails.
pdccpp_target_estimates <- c(0.1, 0.481, 0.9)

test_that("PDCCPP borrowing is unchanged by shifting the null boundary and the effects with it", {
  shift <- 0.2

  # A source effect on the risky side of the boundary in each orientation, so
  # that the calibration search runs rather than short-circuiting to its cap.
  for (parameters in list(list(null_space = "left", source_treatment_effect = 0.481),
                          list(null_space = "right", source_treatment_effect = -0.481))) {
    at_zero <- pdccpp_model(0, parameters$source_treatment_effect,
                            parameters$null_space)
    shifted <- pdccpp_model(shift, parameters$source_treatment_effect + shift,
                            parameters$null_space)

    for (target_estimate in pdccpp_target_estimates) {
      sign <- if (parameters$null_space == "right") -1 else 1
      expect_equal(
        shifted$power_parameter_estimation(
          pdccpp_target_data_at(sign * target_estimate + shift)
        ),
        at_zero$power_parameter_estimation(
          pdccpp_target_data_at(sign * target_estimate)
        ),
        label = sprintf(
          "power parameter at target estimate %g, null space %s",
          sign * target_estimate, parameters$null_space
        )
      )
    }
  }
})

test_that("PDCCPP borrows identically on either side of a mirrored null space", {
  # The transformation maps a right null space onto the left one by negating
  # every effect, so the two orientations are the same problem and must agree.
  # This pins the transformed frame the calibration is expected to work in.
  on_the_left <- pdccpp_model(0.2, 0.681, "left")
  on_the_right <- pdccpp_model(-0.2, -0.681, "right")

  for (target_estimate in pdccpp_target_estimates) {
    expect_equal(
      on_the_right$power_parameter_estimation(
        pdccpp_target_data_at(-(target_estimate + 0.2))
      ),
      on_the_left$power_parameter_estimation(
        pdccpp_target_data_at(target_estimate + 0.2)
      ),
      label = sprintf("power parameter at target estimate %g", target_estimate)
    )
  }
})
