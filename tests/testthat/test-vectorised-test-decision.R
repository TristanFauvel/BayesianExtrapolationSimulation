test_that("vectorised decisions honour non-default critical values", {
  posterior_probabilities <- c(0.94, 0.96, 0.98)
  posterior_sds <- matrix(1, nrow = 3, ncol = 1)

  left_decisions <- function(critical_value) {
    vectorised_test_decision(
      posterior_weights = matrix(1, nrow = 3, ncol = 1),
      posterior_means = matrix(stats::qnorm(posterior_probabilities), ncol = 1),
      posterior_sds = posterior_sds,
      critical_value = critical_value,
      theta_0 = 0,
      null_space = "left"
    )
  }
  right_decisions <- function(critical_value) {
    vectorised_test_decision(
      posterior_weights = matrix(1, nrow = 3, ncol = 1),
      posterior_means = matrix(-stats::qnorm(posterior_probabilities), ncol = 1),
      posterior_sds = posterior_sds,
      critical_value = critical_value,
      theta_0 = 0,
      null_space = "right"
    )
  }

  expect_equal(left_decisions(0.95), c(FALSE, TRUE, TRUE))
  expect_equal(left_decisions(0.975), c(FALSE, FALSE, TRUE))
  expect_equal(right_decisions(0.95), c(FALSE, TRUE, TRUE))
  expect_equal(right_decisions(0.975), c(FALSE, FALSE, TRUE))
})
