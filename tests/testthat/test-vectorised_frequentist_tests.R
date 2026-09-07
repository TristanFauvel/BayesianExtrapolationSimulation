# The empirical Bayes power priors and both test-then-pool variants decide what
# to borrow from a two-sample test computed from summary statistics. The scalar
# path calls BSDA once per replicate; these helpers must give the same answer
# for all replicates at once.

test_that("summary_t_test_p_value matches BSDA::tsum.test for every alternative", {
  mean_x <- 0.481
  sd_x <- 0.1208 * sqrt(281)
  n_x <- 281

  mean_y <- c(0.3, 0.62, 0.481, -0.1)
  sd_y <- c(1.9, 2.4, 2.0, 1.7)
  n_y <- 46

  for (alternative in c("less", "greater", "two.sided")) {
    for (mu in c(0, 0.1, -0.1)) {
      actual <- summary_t_test_p_value(
        mean_x = mean_x, sd_x = sd_x, n_x = n_x,
        mean_y = mean_y, sd_y = sd_y, n_y = n_y,
        mu = mu, alternative = alternative
      )

      expected <- vapply(seq_along(mean_y), function(r) {
        BSDA::tsum.test(
          mean.x = mean_x, s.x = sd_x, n.x = n_x,
          mean.y = mean_y[r], s.y = sd_y[r], n.y = n_y,
          mu = mu, alternative = alternative
        )$p.value
      }, numeric(1))

      expect_equal(actual, expected)
    }
  }
})

test_that("summary_z_test_p_value matches BSDA::zsum.test for every alternative", {
  mean_x <- 0.481
  sd_x <- 0.1208 * sqrt(281)
  n_x <- 281

  mean_y <- c(0.3, 0.62, -0.1)
  sd_y <- c(1.9, 2.4, 1.7)
  n_y <- 46

  for (alternative in c("less", "greater", "two.sided")) {
    for (mu in c(0, 0.1)) {
      actual <- summary_z_test_p_value(
        mean_x = mean_x, sd_x = sd_x, n_x = n_x,
        mean_y = mean_y, sd_y = sd_y, n_y = n_y,
        mu = mu, alternative = alternative
      )

      expected <- vapply(seq_along(mean_y), function(r) {
        BSDA::zsum.test(
          mean.x = mean_x, sigma.x = sd_x, n.x = n_x,
          mean.y = mean_y[r], sigma.y = sd_y[r], n.y = n_y,
          mu = mu, alternative = alternative
        )$p.value
      }, numeric(1))

      expect_equal(actual, expected)
    }
  }
})
