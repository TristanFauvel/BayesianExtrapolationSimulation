mock_posterior_precision_ess <- function(model) {
  200
}

test_that("prior_precision_ess subtracts the target sample size", {
  with_mocked_bindings(
    {
      rbest_model <- list()
      target_data <- list(
        sample_size_per_arm = 100,
        summary_measure_likelihood = "normal"
      )

      result <- prior_precision_ess(rbest_model, target_data)

      expect_equal(result, 100)
    },
    gaussian_mix_precision_ess = mock_posterior_precision_ess,
    .package = "BExTE"
  )
})
