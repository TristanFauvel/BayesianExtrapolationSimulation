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

  # RBesT stops when successive Gauss-Hermite estimates agree to 1e-4 relative,
  # and falls back to adaptive quadrature when they never do. The panelled rule
  # used here is converged to 13 digits by comparison, so the two agree to
  # RBesT's accuracy rather than to ours.
  expect_equal(actual, expected, tolerance = 1e-5)
})

test_that("normal_mixture_elir_ess matches RBesT for realistic robust mixture priors", {
  # The configured robust mixture priors pair an informative component whose
  # scale is the source standard error with a vague component whose scale is
  # the target sampling standard deviation. The two differ by a factor of
  # around twenty, which is the case that decides how ELIR has to be computed.
  specs <- list(
    c(w = 0.5, info_mean = 0.481, info_sd = 0.1208, vague_sd = 2.88),
    c(w = 0.1, info_mean = 0.481, info_sd = 0.1208, vague_sd = 2.88),
    c(w = 0.9, info_mean = 0.481, info_sd = 0.1208, vague_sd = 2.88),
    c(w = 0.5, info_mean = -0.693, info_sd = 0.1304, vague_sd = 2.49)
  )

  weights <- t(vapply(specs, function(s) c(s[["w"]], 1 - s[["w"]]), numeric(2)))
  means <- t(vapply(specs, function(s) c(s[["info_mean"]], 0), numeric(2)))
  sds <- t(vapply(specs, function(s) c(s[["info_sd"]], s[["vague_sd"]]), numeric(2)))
  sigma <- sds[, 2]

  actual <- normal_mixture_elir_ess(weights, means, sds, sigma = sigma)

  expected <- vapply(seq_along(specs), function(r) {
    mix <- RBesT::mixnorm(a = c(weights[r, 1], means[r, 1], sds[r, 1]),
                          b = c(weights[r, 2], means[r, 2], sds[r, 2]))
    RBesT::ess(mix, method = "elir", sigma = sigma[r])
  }, numeric(1))

  expect_equal(actual, expected, tolerance = 1e-6)
})

test_that("normal_mixture_elir_ess accepts a prior shared by every replicate", {
  # A prior that does not vary by replicate only needs the integral evaluating
  # once, since ELIR is proportional to the squared reference scale. The
  # normalised power prior relies on this: its mixture has dozens of components
  # but is the same for every replicate.
  weights <- c(0.4, 0.6)
  means <- c(0.7, 0.0)
  sds <- c(0.2, 1.5)
  sigma <- c(1, 2.5, 0.4)

  shared <- normal_mixture_elir_ess(weights, means, sds, sigma = sigma)

  per_replicate <- normal_mixture_elir_ess(
    weights = matrix(weights, nrow = 3, ncol = 2, byrow = TRUE),
    means = matrix(means, nrow = 3, ncol = 2, byrow = TRUE),
    sds = matrix(sds, nrow = 3, ncol = 2, byrow = TRUE),
    sigma = sigma
  )

  expect_equal(shared, per_replicate)
  expect_length(shared, 3)
})

test_that("normal_mixture_elir_ess scales with the square of the reference scale", {
  weights <- matrix(c(0.4, 0.6), nrow = 1)
  means <- matrix(c(0.7, 0.0), nrow = 1)
  sds <- matrix(c(0.2, 1.5), nrow = 1)

  at_one <- normal_mixture_elir_ess(weights, means, sds, sigma = 1)
  at_three <- normal_mixture_elir_ess(weights, means, sds, sigma = 3)

  expect_equal(at_three, 9 * at_one)
})

test_that("centred scale-mixture ELIR shortcut matches RBesT", {
  weights <- c(0.15, 0.35, 0.5)
  means <- rep(0.4, 3)
  sds <- c(0.03, 0.4, 20)
  sigma <- 2.3

  actual <- normal_mixture_elir_ess(weights, means, sds, sigma)
  mixture <- RBesT::mixnorm(
    narrow = c(weights[1], means[1], sds[1]),
    middle = c(weights[2], means[2], sds[2]),
    wide = c(weights[3], means[3], sds[3])
  )
  expected <- RBesT::ess(mixture, method = "elir", sigma = sigma)

  expect_equal(actual, expected, tolerance = 1e-6)
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

test_that("a shared prior gives the same posterior as one repeated per replicate", {
  weights <- c(0.15, 0.35, 0.5)
  means <- c(0.4, 0.4, 0.4)
  sds <- c(0.05, 0.6, 4.0)

  set.seed(4)
  estimates <- rnorm(25, 0.5, 0.2)
  standard_errors <- runif(25, 0.05, 0.4)

  shared <- normal_mixture_posterior(
    weights = weights, means = means, sds = sds,
    estimate = estimates, standard_error = standard_errors
  )

  # Handing the same prior over as one row per replicate takes the matrix
  # branch instead, which is the branch RBesT is checked against above. The
  # shared branch exists only to keep three constant copies of the prior out of
  # memory, so it has to agree with it to the last bit that matters.
  per_replicate <- normal_mixture_posterior(
    weights = recycle_prior_component(weights, length(estimates)),
    means = recycle_prior_component(means, length(estimates)),
    sds = recycle_prior_component(sds, length(estimates)),
    estimate = estimates, standard_error = standard_errors
  )

  expect_equal(shared$weights, per_replicate$weights)
  expect_equal(shared$means, per_replicate$means)
  expect_equal(shared$sds, per_replicate$sds)
})


test_that("a shared prior is broadcast against a single standard error", {
  weights <- c(0.3, 0.7)
  means <- c(0.2, 0.2)
  sds <- c(0.1, 1.5)
  estimates <- c(0.1, 0.35, 0.6)

  # A scalar standard error used to reach the arithmetic by recycling; the
  # shared branch broadcasts explicitly, so it has to be expanded first.
  posterior <- normal_mixture_posterior(
    weights = weights, means = means, sds = sds,
    estimate = estimates, standard_error = 0.25
  )

  expect_equal(dim(posterior$weights), c(3L, 2L))
  expect_equal(rowSums(posterior$weights), rep(1, 3))
  expect_equal(
    posterior$sds,
    normal_mixture_posterior(
      weights = weights, means = means, sds = sds,
      estimate = estimates, standard_error = rep(0.25, 3)
    )$sds
  )
})
