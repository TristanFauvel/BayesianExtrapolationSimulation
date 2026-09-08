TestModel <- R6::R6Class(
  "TestModel",
  inherit = Model,
  public = list(
    posterior_summary = NULL,
    credible_interval_97.5 = NULL,
    credible_interval_2.5 = NULL,
    posterior_location = NULL,
    posterior_scale = NULL,
    posterior_cdf = function(target_treatment_effect) {
      stats::pnorm(
        target_treatment_effect,
        mean = self$posterior_location,
        sd = self$posterior_scale
      )
    }
  )
)

model_mcmc <- TestModel$new()
model_mcmc$mcmc <- TRUE
model_mcmc$credible_interval_97.5 <- 1.5
model_mcmc$credible_interval_2.5 <- 0.5

model_analytic <- TestModel$new()
model_analytic$mcmc <- FALSE
model_analytic$posterior_summary <- c(cri95L = 0.4, cri95U = 1.2)
model_analytic$posterior_location <- 0.8
model_analytic$posterior_scale <- 0.2

test_that("test_decision works for an MCMC model with a left null space", {
  result <- model_mcmc$test_decision(
    critical_value = 0.975,
    theta_0 = 0.3,
    null_space = "left",
    confidence_level = 0.95
  )

  expect_true(result)
})

test_that("test_decision works for an MCMC model with a right null space", {
  result <- model_mcmc$test_decision(
    critical_value = 0.975,
    theta_0 = 2,
    null_space = "right",
    confidence_level = 0.95
  )

  expect_true(result)
})

test_that("test_decision works for an analytical model with a left null space", {
  result <- model_analytic$test_decision(
    critical_value = 0.975,
    theta_0 = 0.5,
    null_space = "left",
    confidence_level = 0.95
  )

  expect_false(result)
})

test_that("test_decision works for an analytical model with a right null space", {
  result <- model_analytic$test_decision(
    critical_value = 0.975,
    theta_0 = 1,
    null_space = "right",
    confidence_level = 0.95
  )

  expect_false(result)
})

test_that("analytical decisions honour non-default critical values", {
  expect_true(model_analytic$test_decision(
    critical_value = 0.9,
    theta_0 = 0.5,
    null_space = "left",
    confidence_level = 0.95
  ))
  expect_false(model_analytic$test_decision(
    critical_value = 0.95,
    theta_0 = 0.5,
    null_space = "left",
    confidence_level = 0.95
  ))

  expect_true(model_analytic$test_decision(
    critical_value = 0.8,
    theta_0 = 1,
    null_space = "right",
    confidence_level = 0.95
  ))
  expect_false(model_analytic$test_decision(
    critical_value = 0.9,
    theta_0 = 1,
    null_space = "right",
    confidence_level = 0.95
  ))
})

test_that("test_decision rejects incompatible MCMC critical values", {
  expect_error(
    model_mcmc$test_decision(
      critical_value = 0.95,
      theta_0 = 0.3,
      null_space = "left",
      confidence_level = 0.95
    ),
    "Only the 97.5 and 2.5 percentiles are computed"
  )
})
