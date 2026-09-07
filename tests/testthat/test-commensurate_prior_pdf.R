# joint_prior_pdf needs only the prior and the family, so this harness skips the
# Stan compilation that the real constructor performs.
CommensuratePriorHarness <- R6::R6Class(
  "CommensuratePriorHarness",
  inherit = GaussianCommensuratePowerPrior,
  public = list(
    initialize = function(prior, heterogeneity_prior_family) {
      self$prior <- prior
      self$heterogeneity_prior_family <- heterogeneity_prior_family
    }
  )
)

commensurate_prior <- function(family, ...) {
  list(
    source = list(
      standard_error = 0.2,
      equivalent_source_sample_size_per_arm = 50,
      treatment_effect_estimate = 0.4
    ),
    method_parameters = list(
      heterogeneity_prior = c(list(family = family), list(...))
    )
  )
}


test_that("the inverse gamma heterogeneity prior uses beta as the scale", {
  # Every shipped configuration sets beta = 1, where the scale and its
  # reciprocal coincide, so a non-unit scale is needed to see the difference.
  alpha <- 1 / 7
  beta <- 4

  model <- CommensuratePriorHarness$new(
    prior = commensurate_prior("inverse_gamma", alpha = alpha, beta = beta),
    heterogeneity_prior_family = "inverse_gamma"
  )

  treatment_effect <- 0.35
  gamma <- 0.6
  tau <- c(0.05, 0.3, 1, 2.5, 8)

  source_variance <- 0.2^2 * 50
  expected <- dnorm(
    treatment_effect,
    mean = 0.4,
    sd = sqrt(1 / tau + source_variance / (gamma * 50))
  ) *
    dbeta(gamma, shape1 = g_function(log(tau)), shape2 = 1) *
    # extraDistr, extraDistr::rinvgamma in sample_prior and Stan's inv_gamma all
    # take beta as the scale.
    2 * tau * extraDistr::dinvgamma(tau^2, alpha = alpha, beta = beta)

  expect_equal(
    model$joint_prior_pdf(treatment_effect, gamma, tau),
    expected,
    tolerance = 1e-10
  )
})


test_that("the inverse gamma prior density agrees with the way the prior is sampled", {
  alpha <- 2
  beta <- 4
  model <- CommensuratePriorHarness$new(
    prior = commensurate_prior("inverse_gamma", alpha = alpha, beta = beta),
    heterogeneity_prior_family = "inverse_gamma"
  )

  # sample_prior draws tau^2 with extraDistr::rinvgamma(alpha, beta); the
  # density joint_prior_pdf applies to tau must be the density of those draws.
  # tau^2 has mean beta / (alpha - 1) = 4 here, so these sit in the bulk of the
  # distribution where a kernel density estimate is reliable.
  tau <- c(1.2, 1.8, 2.5, 3.5)
  density_ratio <- model$joint_prior_pdf(0.35, 0.6, tau) /
    (dnorm(0.35, 0.4, sqrt(1 / tau + 0.2^2 * 50 / (0.6 * 50))) *
       dbeta(0.6, g_function(log(tau)), 1))

  set.seed(1)
  samples <- sqrt(extraDistr::rinvgamma(4e6, alpha = alpha, beta = beta))
  kde <- density(samples, n = 4096, from = 0, to = 10)
  empirical <- approx(kde$x, kde$y, xout = tau)$y

  expect_equal(density_ratio, empirical, tolerance = 0.02)
})


test_that("the half normal heterogeneity prior is unchanged", {
  model <- CommensuratePriorHarness$new(
    prior = commensurate_prior("half_normal", std_dev = 2),
    heterogeneity_prior_family = "half_normal"
  )
  tau <- c(0.2, 1, 3)

  ratio <- model$joint_prior_pdf(0.35, 0.6, tau) /
    (dnorm(0.35, 0.4, sqrt(1 / tau + 0.2^2 * 50 / (0.6 * 50))) *
       dbeta(0.6, g_function(log(tau)), 1))

  expect_equal(ratio, extraDistr::dhnorm(tau, sigma = 2), tolerance = 1e-10)
})
