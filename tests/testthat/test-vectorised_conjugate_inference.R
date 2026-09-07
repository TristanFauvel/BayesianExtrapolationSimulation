test_that("normal_mixture_posterior matches RBesT::postmix across replicates", {
  weights <- c(0.5, 0.5)
  means <- c(1.2, 0.0)
  sds <- c(0.4, 3.0)

  estimates <- c(0.8, -0.3, 2.1)
  standard_errors <- c(0.35, 0.9, 0.12)

  posterior <- normal_mixture_posterior(
    weights = weights,
    means = means,
    sds = sds,
    estimate = estimates,
    standard_error = standard_errors
  )

  prior <- RBesT::mixnorm(info = c(weights[1], means[1], sds[1]),
                          vague = c(weights[2], means[2], sds[2]))

  for (r in seq_along(estimates)) {
    expected <- RBesT::postmix(prior, m = estimates[r], se = standard_errors[r])

    expect_equal(posterior$weights[r, ], as.numeric(expected[1, ]))
    expect_equal(posterior$means[r, ], as.numeric(expected[2, ]))
    expect_equal(posterior$sds[r, ], as.numeric(expected[3, ]))
  }
})

test_that("normal_mixture_summary matches the RBesT mixture mean and standard deviation", {
  weights <- matrix(c(0.8, 0.2,
                      0.35, 0.65), nrow = 2, byrow = TRUE)
  means <- matrix(c(0.97, 0.79,
                    -0.2, 0.4), nrow = 2, byrow = TRUE)
  sds <- matrix(c(0.26, 0.35,
                  0.5, 1.1), nrow = 2, byrow = TRUE)

  summaries <- normal_mixture_summary(weights, means, sds)

  for (r in 1:2) {
    mix <- RBesT::mixnorm(a = c(weights[r, 1], means[r, 1], sds[r, 1]),
                          b = c(weights[r, 2], means[r, 2], sds[r, 2]))
    expected <- summary(mix)

    expect_equal(summaries$mean[r], as.numeric(expected["mean"]))
    expect_equal(summaries$sd[r], as.numeric(expected["sd"]))
  }
})

test_that("normal_mixture_summary quantiles invert the mixture CDF", {
  weights <- matrix(c(0.8, 0.2), nrow = 1)
  means <- matrix(c(0.97, 0.79), nrow = 1)
  sds <- matrix(c(0.26, 0.35), nrow = 1)

  probs <- c(0.025, 0.5, 0.975)
  summaries <- normal_mixture_summary(weights, means, sds, probs = probs)

  mix <- RBesT::mixnorm(a = c(weights[1, 1], means[1, 1], sds[1, 1]),
                        b = c(weights[1, 2], means[1, 2], sds[1, 2]))

  attained <- RBesT::pmix(mix, summaries$quantiles[1, ])
  expect_equal(attained, probs, tolerance = 1e-9)
})

test_that("normal_mixture_summary quantiles agree with RBesT summary to its own tolerance", {
  weights <- matrix(c(0.8, 0.2,
                      0.35, 0.65), nrow = 2, byrow = TRUE)
  means <- matrix(c(0.97, 0.79,
                    -0.2, 0.4), nrow = 2, byrow = TRUE)
  sds <- matrix(c(0.26, 0.35,
                  0.5, 1.1), nrow = 2, byrow = TRUE)

  summaries <- normal_mixture_summary(weights, means, sds)

  for (r in 1:2) {
    mix <- RBesT::mixnorm(a = c(weights[r, 1], means[r, 1], sds[r, 1]),
                          b = c(weights[r, 2], means[r, 2], sds[r, 2]))
    # summary() names the lower quantile " 2.5%", with a leading space.
    expected <- as.numeric(summary(mix)[3:5])

    # RBesT resolves mixture quantiles with uniroot at its default tolerance,
    # so it is the looser of the two; the check above pins down our accuracy.
    expect_equal(summaries$quantiles[r, ], expected, tolerance = 1e-4)
  }
})

test_that("normal_mixture_summary reduces to the normal quantile for one component", {
  summaries <- normal_mixture_summary(
    weights = matrix(1, nrow = 2),
    means = matrix(c(0.4, -1.2), ncol = 1),
    sds = matrix(c(0.7, 2.3), ncol = 1)
  )

  expect_equal(summaries$mean, c(0.4, -1.2))
  expect_equal(summaries$sd, c(0.7, 2.3))
  expect_equal(summaries$quantiles[, 1], qnorm(0.025, c(0.4, -1.2), c(0.7, 2.3)))
  expect_equal(summaries$quantiles[, 3], qnorm(0.975, c(0.4, -1.2), c(0.7, 2.3)))
})

test_that("normal_mixture_elir_ess is exact for a single normal component", {
  # The Fisher information of a normal density is 1 / variance everywhere, so
  # ELIR collapses to sigma^2 / variance with no integration at all.
  variances <- c(0.04, 1, 9)
  sigmas <- c(1, 3.7, 0.5)

  ess <- normal_mixture_elir_ess(
    weights = matrix(1, nrow = 3),
    means = matrix(c(0.3, -1, 2), ncol = 1),
    sds = matrix(sqrt(variances), ncol = 1),
    sigma = sigmas
  )

  expect_equal(ess, sigmas^2 / variances)
})

test_that("normal_mixture_elir_ess matches RBesT for two-component mixtures", {
  # Includes a mixture whose component scales differ by 5000x, where a single
  # fixed integration range misses the sharp component entirely.
  specs <- list(
    c(w = 0.5, m1 = 0.6, s1 = 0.15, m2 = 0.0, s2 = 1.0),
    c(w = 0.9, m1 = 1.2, s1 = 0.05, m2 = 0.0, s2 = 3.0),
    c(w = 0.2, m1 = -0.4, s1 = 0.8, m2 = 0.3, s2 = 0.9),
    c(w = 0.5, m1 = 0.0, s1 = 0.1, m2 = 0.0, s2 = 31.6),
    c(w = 0.05, m1 = 0.3, s1 = 0.02, m2 = 0.0, s2 = 100)
  )
  sigma <- 2.3

  weights <- t(vapply(specs, function(s) c(s[["w"]], 1 - s[["w"]]), numeric(2)))
  means <- t(vapply(specs, function(s) c(s[["m1"]], s[["m2"]]), numeric(2)))
  sds <- t(vapply(specs, function(s) c(s[["s1"]], s[["s2"]]), numeric(2)))

  actual <- normal_mixture_elir_ess(weights, means, sds, sigma = sigma)

  expected <- vapply(seq_along(specs), function(r) {
    mix <- RBesT::mixnorm(a = c(weights[r, 1], means[r, 1], sds[r, 1]),
                          b = c(weights[r, 2], means[r, 2], sds[r, 2]))
    RBesT::ess(mix, method = "elir", sigma = sigma)
  }, numeric(1))

  # Same Gauss-Hermite rule and same convergence criterion as RBesT, so the two
  # agree far inside RBesT's own 1e-4 relative stopping tolerance.
  expect_equal(actual, expected, tolerance = 1e-6)
})

test_that("normal_mixture_elir_ess scales with the square of the reference scale", {
  weights <- matrix(c(0.4, 0.6), nrow = 1)
  means <- matrix(c(0.7, 0.0), nrow = 1)
  sds <- matrix(c(0.2, 1.5), nrow = 1)

  at_one <- normal_mixture_elir_ess(weights, means, sds, sigma = 1)
  at_three <- normal_mixture_elir_ess(weights, means, sds, sigma = 3)

  expect_equal(at_three, 9 * at_one)
})

test_that("normal_mixture_posterior accepts a different prior for each replicate", {
  # Empirical Bayes methods re-derive the prior from each replicate, so the
  # prior arrives as one row per replicate rather than as a shared vector.
  weights <- matrix(c(0.3, 0.7,
                      0.6, 0.4), nrow = 2, byrow = TRUE)
  means <- matrix(c(1.2, 0.0,
                    1.2, 0.5), nrow = 2, byrow = TRUE)
  sds <- matrix(c(0.4, 3.0,
                  0.4, 1.7), nrow = 2, byrow = TRUE)

  estimates <- c(0.8, -0.3)
  standard_errors <- c(0.35, 0.9)

  posterior <- normal_mixture_posterior(
    weights = weights,
    means = means,
    sds = sds,
    estimate = estimates,
    standard_error = standard_errors
  )

  for (r in seq_along(estimates)) {
    prior <- RBesT::mixnorm(info = c(weights[r, 1], means[r, 1], sds[r, 1]),
                            vague = c(weights[r, 2], means[r, 2], sds[r, 2]))
    expected <- RBesT::postmix(prior, m = estimates[r], se = standard_errors[r])

    expect_equal(posterior$weights[r, ], as.numeric(expected[1, ]))
    expect_equal(posterior$means[r, ], as.numeric(expected[2, ]))
    expect_equal(posterior$sds[r, ], as.numeric(expected[3, ]))
  }
})
