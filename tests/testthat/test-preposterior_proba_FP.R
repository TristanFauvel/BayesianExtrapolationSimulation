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
