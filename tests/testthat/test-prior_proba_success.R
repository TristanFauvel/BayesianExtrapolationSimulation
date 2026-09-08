test_that("prior probability of success adds both design-prior tails for a left null space", {
  treatment_effect_values <- seq(-1, 1, length.out = 201)
  rejection_rate <- function(x) stats::pnorm(5 * x)

  result <- prior_proba_success(
    conditional_proba_success = rejection_rate(treatment_effect_values),
    treatment_effect_values = treatment_effect_values,
    design_prior_pdf = stats::dnorm(treatment_effect_values),
    design_prior_cdf = function(x) stats::pnorm(x),
    null_space = "left"
  )

  on_grid <- stats::integrate(function(x) stats::dnorm(x) * rejection_rate(x), -1, 1)$value

  # The rejection rate tends to 0 below the grid and to 1 above it, so only the
  # upper tail contributes.
  expect_equal(result, on_grid + (1 - stats::pnorm(1)), tolerance = 1e-5)
})

test_that("prior probability of success adds both design-prior tails for a right null space", {
  treatment_effect_values <- seq(-1, 1, length.out = 201)
  rejection_rate <- function(x) stats::pnorm(-5 * x)

  result <- prior_proba_success(
    conditional_proba_success = rejection_rate(treatment_effect_values),
    treatment_effect_values = treatment_effect_values,
    design_prior_pdf = stats::dnorm(treatment_effect_values),
    design_prior_cdf = function(x) stats::pnorm(x),
    null_space = "right"
  )

  on_grid <- stats::integrate(function(x) stats::dnorm(x) * rejection_rate(x), -1, 1)$value

  # The rejection rate tends to 1 below the grid and to 0 above it, so only the
  # lower tail contributes.
  expect_equal(result, on_grid + stats::pnorm(-1), tolerance = 1e-5)
})

test_that("prior probability of success rejects an invalid null space", {
  expect_error(
    prior_proba_success(
      conditional_proba_success = c(0.1, 0.9),
      treatment_effect_values = c(-1, 1),
      design_prior_pdf = c(0.5, 0.5),
      design_prior_cdf = function(x) stats::pnorm(x),
      null_space = "middle"
    ),
    "Null space must be either 'left' or 'right'",
    fixed = TRUE
  )
})
