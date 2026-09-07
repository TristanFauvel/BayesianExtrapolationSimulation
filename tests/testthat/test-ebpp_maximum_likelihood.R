# EB_PP is the maximum likelihood estimator of Gravestock and Held (2017):
# gamma maximising the marginal likelihood L(gamma) = integral L(theta|D1)
# pi(theta, gamma|D0) dtheta. Nikolakopoulos et al (2018) note that their
# equation (9) "results in the ML estimator suggested in Gravestock and Held
# (2017) for the special case of z_{1-c/2} = 1", which is how EB_PP is
# implemented here -- as that equation with the cut-off fixed at one predictive
# standard deviation.
#
# That identity only holds when the prior sample size follows from the prior
# variance, so these tests are the check on it: they maximise the marginal
# likelihood numerically, assuming no closed form, and compare.

ebpp_fixture <- function(target_standard_error = 0.21,
                         target_sample_size_per_arm = 52) {
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
    model = Model$new()$create(
      case_study_config = case_study_config, method = "EB_PP",
      method_parameters = list(initial_prior = list("noninformative")),
      source_data = source_data
    ),
    se_source = source_data$standard_error,
    se_target = target_standard_error,
    n1 = target_sample_size_per_arm
  )
}

test_that("EB_PP maximises the marginal likelihood of the power parameter", {
  f <- ebpp_fixture()
  m0 <- f$source_data$treatment_effect_estimate

  # The prior discounted by gamma is N(m0, se_source^2 / gamma), so the target
  # estimate is marginally N(m0, se_source^2 / gamma + se_target^2).
  maximise <- function(x) {
    fit <- optimize(
      function(g) dnorm(x, m0, sqrt(f$se_source^2 / g + f$se_target^2), log = TRUE),
      interval = c(1e-10, 1), maximum = TRUE
    )$maximum
    # optimize cannot land on the boundary exactly.
    if (fit > 0.999) 1 else fit
  }

  for (deviation in c(0, 0.05, 0.12, 0.2, 0.3, 0.5, 0.8, -0.3, -0.6)) {
    estimate <- m0 + deviation
    got <- f$model$power_parameter_estimation(
      ObservedTargetData$new(estimate, f$se_target, f$n1, "normal")
    )
    expect_equal(got, maximise(estimate), tolerance = 1e-4,
                 label = sprintf("power parameter at deviation %g", deviation))
  }
})

test_that("EB_PP borrows in full inside one predictive standard deviation", {
  # z_{1-c/2} = 1, and sigma_pr^2 is the prior variance plus the sampling
  # variance of the target estimate.
  f <- ebpp_fixture()
  m0 <- f$source_data$treatment_effect_estimate
  sigma_pr <- sqrt(f$se_source^2 + f$se_target^2)

  borrows_fully <- function(deviation) {
    f$model$power_parameter_estimation(
      ObservedTargetData$new(m0 + deviation, f$se_target, f$n1, "normal")
    ) == 1
  }
  lower <- 0
  upper <- 5 * sigma_pr
  for (step in seq_len(200)) {
    middle <- (lower + upper) / 2
    if (borrows_fully(middle)) lower <- middle else upper <- middle
  }
  expect_equal((lower + upper) / 2, sigma_pr, tolerance = 1e-8)
})

test_that("the vectorised EB_PP estimator reproduces the scalar one", {
  f <- ebpp_fixture()
  target_data <- TargetDataFactory$new()$create(
    source_data = f$source_data, case_study_config = f$case_study_config,
    target_sample_size_per_arm = f$n1, control_drift = 0, treatment_drift = 0,
    summary_measure_likelihood = "normal", target_to_source_std_ratio = 1
  )
  set.seed(3)
  samples <- target_data$generate(200)

  vectorised <- f$model$vectorised_power_parameter(target_data, samples)
  scalar <- vapply(seq_len(nrow(samples)), function(r) {
    f$model$power_parameter_estimation(ObservedTargetData$new(
      samples$treatment_effect_estimate[r],
      samples$treatment_effect_standard_error[r], f$n1, "normal"
    ))
  }, numeric(1))

  expect_equal(vectorised, scalar, tolerance = 0)
})
