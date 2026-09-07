# The expected local information ratio effective sample size of a prior is
#
#   ELIR = sigma^2 * E_prior[ -(log prior)''(theta) ]
#
# for a normal likelihood of known scale sigma. RBesT evaluates it by integrating
# that expectation over a normal mixture, so an untruncated mixture is the
# reference these tests check the closed-form route against.

reference_elir <- function(weights, means, sds, lower, upper, sigma) {
  component_density <- function(theta) {
    outer(theta, seq_along(weights), function(x, k) {
      normaliser <- stats::pnorm(upper, means[k], sds[k]) -
        stats::pnorm(lower, means[k], sds[k])
      weights[k] * stats::dnorm(x, means[k], sds[k]) / normaliser
    })
  }
  density <- function(theta) rowSums(component_density(theta))

  # Central differences on the log density, an independent route to the second
  # derivative the implementation differentiates analytically.
  step <- 1e-5
  negative_log_hessian <- function(theta) {
    -(log(density(theta + step)) - 2 * log(density(theta)) +
        log(density(theta - step))) / step^2
  }

  nodes <- seq(lower + 1e-6, upper - 1e-6, length.out = 400001)
  spacing <- nodes[2] - nodes[1]
  integrand <- density(nodes) * negative_log_hessian(nodes)
  weights_simpson <- c(1, rep(c(4, 2), length.out = length(nodes) - 2), 1)

  sigma^2 * sum(weights_simpson * integrand) * spacing / 3
}


test_that("the closed-form ELIR reproduces RBesT on an untruncated mixture", {
  weights <- c(0.6, 0.4)
  means <- c(0.1, -0.2)
  sds <- c(0.3, 0.7)
  mixture <- RBesT::mixnorm(
    a = c(weights[1], means[1], sds[1]),
    b = c(weights[2], means[2], sds[2]),
    sigma = 1.5
  )

  # Bounds far enough out that the truncation removes no appreciable mass.
  result <- truncated_normal_mixture_elir(
    weights = weights, means = means, sds = sds,
    lower = -12, upper = 12, sigma = 1.5
  )

  expect_equal(result, RBesT::ess(mixture, method = "elir", sigma = 1.5),
               tolerance = 1e-5)
})


test_that("the closed-form ELIR scales with the square of the reference scale", {
  arguments <- list(
    weights = c(0.5, 0.5), means = c(0, 0.13), sds = c(0.49, 0.035),
    lower = -1, upper = 1
  )

  at_one <- do.call(truncated_normal_mixture_elir, c(arguments, sigma = 1))
  at_half <- do.call(truncated_normal_mixture_elir, c(arguments, sigma = 0.5))

  expect_equal(at_half, at_one * 0.25, tolerance = 1e-8)
})


test_that("the closed-form ELIR integrates the truncated prior the model samples", {
  # Each component is truncated and renormalised on its own, which is how the
  # robust mixture prior is drawn from, rather than the mixture being truncated
  # as a whole.
  weights <- c(0.5, 0.5)
  means <- c(0, 0.1315456)
  sds <- c(0.49, 0.03505291)

  result <- truncated_normal_mixture_elir(
    weights = weights, means = means, sds = sds,
    lower = -1, upper = 1, sigma = 0.5
  )

  expect_equal(
    result,
    reference_elir(weights, means, sds, -1, 1, 0.5),
    tolerance = 1e-4
  )
})


test_that("the closed-form ELIR resolves a component much narrower than the range", {
  weights <- c(0.5, 0.5)
  means <- c(0, 0.2)
  sds <- c(0.8, 0.004)

  result <- truncated_normal_mixture_elir(
    weights = weights, means = means, sds = sds,
    lower = -1, upper = 1, sigma = 0.5
  )

  expect_equal(
    result,
    reference_elir(weights, means, sds, -1, 1, 0.5),
    tolerance = 1e-3
  )
})


test_that("the closed-form ELIR handles a single component", {
  mixture <- RBesT::mixnorm(a = c(1, 0.2, 0.5), sigma = 1)

  result <- truncated_normal_mixture_elir(
    weights = 1, means = 0.2, sds = 0.5,
    lower = -12, upper = 12, sigma = 1
  )

  expect_equal(result, RBesT::ess(mixture, method = "elir", sigma = 1),
               tolerance = 1e-5)
})
