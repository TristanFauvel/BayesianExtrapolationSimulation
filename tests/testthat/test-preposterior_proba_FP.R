test_that("Monte Carlo false-positive probability averages over all prior draws", {
  conditional_success <- c(0.1, 0.2, 0.9)
  prior_draws <- c(-1, 0, 1)

  result <- preposterior_proba_FP_MC(
    conditional_proba_success = conditional_success,
    design_prior_samples = prior_draws,
    theta_0 = 0,
    null_space = "left"
  )

  expect_equal(result, 0.1)
})

test_that("Monte Carlo false-positive probability respects a right null space", {
  conditional_success <- c(0.9, 0.2, 0.1)
  prior_draws <- c(-1, 0, 1)

  result <- preposterior_proba_FP_MC(
    conditional_proba_success = conditional_success,
    design_prior_samples = prior_draws,
    theta_0 = 0,
    null_space = "right"
  )

  expect_equal(result, 0.1)
})

test_that("false-positive probability rejects an invalid null space", {
  expect_error(
    preposterior_proba_FP_MC(
      conditional_proba_success = c(0.1, 0.9),
      design_prior_samples = c(-1, 1),
      theta_0 = 0,
      null_space = "middle"
    ),
    "Null space must be either 'left' or 'right'",
    fixed = TRUE
  )

  expect_error(
    preposterior_proba_FP(
      conditional_proba_success = c(0.1, 0.9),
      treatment_effect_values = c(-1, 1),
      theta_0 = 0,
      null_space = "middle",
      design_prior_pdf = c(0.5, 0.5)
    ),
    "Null space must be either 'left' or 'right'",
    fixed = TRUE
  )
})

test_that("false-positive probability rejects misaligned inputs", {
  expect_error(
    preposterior_proba_FP_MC(
      conditional_proba_success = c(0.1, 0.9),
      design_prior_samples = c(-2, -1, 1, 2),
      theta_0 = 0,
      null_space = "left"
    ),
    "must have the same length",
    fixed = TRUE
  )

  expect_error(
    preposterior_proba_FP(
      conditional_proba_success = c(0.1, 0.5),
      treatment_effect_values = c(-1, 0, 1),
      theta_0 = 0,
      null_space = "left",
      design_prior_pdf = c(0.2, 0.6, 0.2)
    ),
    "must have the same length",
    fixed = TRUE
  )
})
