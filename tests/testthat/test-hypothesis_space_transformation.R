test_that("hypothesis_space_transformation is repeatable for a right null space", {
  # The transformation used to negate self$parameters$theta_0 in place, so the
  # sign flipped on every call and consecutive replicates were transformed
  # differently. That is invisible while theta_0 is 0, as it is in every case
  # study configuration, but it makes the replicate loop and any vectorised
  # equivalent disagree as soon as theta_0 is not 0.
  prior <- list(
    source = list(treatment_effect_estimate = 0.5, standard_error = 0.12),
    method_parameters = list(initial_prior = list("noninformative"),
                             power_parameter = list(0.5))
  )
  model <- Gaussian_Gravestock_EBPP$new(prior = prior,
                                        null_space = "right",
                                        theta_0 = 0.2)
  # Model$create() assigns this after construction.
  model$prior <- prior

  target_data <- list(
    sample = data.frame(treatment_effect_estimate = 0.4,
                        treatment_effect_standard_error = 0.3),
    sample_size_per_arm = 50
  )

  first <- model$hypothesis_space_transformation(target_data)
  second <- model$hypothesis_space_transformation(target_data)

  expect_equal(second, first)
  expect_equal(model$parameters$theta_0, 0.2)
})
