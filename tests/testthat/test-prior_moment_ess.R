mock_posterior_moment_ess <- function(model, method = "moment", sigma = NULL) {
  200
}

mock_reference_sigma <- function(model) {
  1
}

test_that("prior_moment_ess subtracts the target sample size", {
  with_mocked_bindings(
    {
      rbest_model <- list()
      target_data <- list(sample_size_per_arm = 100)

      result <- prior_moment_ess(rbest_model, target_data)

      expect_equal(result, 100)
    },
    ess = mock_posterior_moment_ess,
    sigma = mock_reference_sigma,
    .package = "RBesT"
  )
})
