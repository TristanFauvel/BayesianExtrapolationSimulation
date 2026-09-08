# Fixtures live in helper-commensurate.R, alongside the ones the Stan program
# tests use.

commensurate_replicate_samples <- function() {
  data.frame(
    treatment_effect_estimate = c(0.2, 0.5, 0.9),
    treatment_effect_standard_error = rep(0.1, 3),
    standard_deviation = rep(0.1 * sqrt(60), 3)
  )
}

commensurate_fast_path_run <- function(model, to_return,
                                       critical_value = 0.975,
                                       confidence_level = 0.95,
                                       samples = commensurate_replicate_samples()) {
  model$vectorised_replicate_inference(
    target_data = list(sample_size_per_arm = 60),
    samples = samples,
    to_return = to_return,
    critical_value = critical_value,
    theta_0 = 0,
    confidence_level = confidence_level,
    null_space = "left"
  )
}


test_that("commensurate quadrature represents each configured prior family", {
  for (prior in commensurate_configured_priors()) {
    mixture <- commensurate_prior_mixture(
      commensurate_fast_path_model(prior)
    )

    expect_equal(sum(mixture$weights), 1, tolerance = 1e-12)
    expect_true(all(mixture$weights > 0))
    expect_true(all(is.finite(mixture$sds)))
    expect_true(all(mixture$sds > 0))
    expect_true(all(mixture$power_parameter > 0))
    expect_true(all(mixture$power_parameter <= 1))
  }
})


test_that("inverse-gamma tau quadrature handles small shape parameters", {
  for (alpha in c(1 / 3, 1 / 7, 1 / 1000)) {
    model <- commensurate_fast_path_model(list(
      family = "inverse_gamma",
      alpha = alpha,
      beta = 4
    ))
    rule <- commensurate_tau_quadrature(model, n_nodes = 64L)

    # If tau^2 ~ InvGamma(alpha, beta), 1 / tau is the square root
    # of a Gamma(alpha, rate = beta) variable.
    expected <- gamma(alpha + 0.5) / (gamma(alpha) * sqrt(4))
    expect_equal(sum(rule$weights * rule$inverse_tau), expected,
                 tolerance = 2e-4)
  }
})


test_that("half-normal tau quadrature integrates tau, not only tau squared", {
  for (std_dev in c(1, 5)) {
    model <- commensurate_fast_path_model(list(
      family = "half_normal",
      std_dev = std_dev
    ))
    rule <- commensurate_tau_quadrature(model, n_nodes = 48L)

    expect_equal(sum(rule$weights), 1, tolerance = 1e-12)
    expect_equal(sum(rule$weights * rule$tau^2), std_dev^2, tolerance = 1e-3)

    # The Gauss-Laguerre rule this replaced integrated tau^2 exactly, because
    # tau^2 is linear in the Laguerre variable, but reached only O(1 / n_nodes)
    # on tau itself: 0.43% out at this node count and still 0.05% out at 384.
    # Anything that regresses to it fails this bound by an order of magnitude.
    expect_equal(sum(rule$weights * rule$tau), std_dev * sqrt(2 / pi),
                 tolerance = 5e-4)
  }
})


test_that("commensurate mixture implements the collapsed Stan posterior", {
  model <- commensurate_fast_path_model(list(
    family = "half_normal",
    std_dev = 2
  ))
  prior <- commensurate_prior_mixture(model, n_tau = 12L, n_gamma = 10L)
  estimate <- 0.9
  standard_error <- 0.1

  posterior <- normal_mixture_posterior(
    prior$weights,
    prior$means,
    prior$sds,
    estimate,
    standard_error
  )

  prior_variance <- prior$sds^2
  expected_variance <- 1 / (1 / prior_variance + 1 / standard_error^2)
  expected_mean <- expected_variance *
    (prior$means / prior_variance + estimate / standard_error^2)
  expected_log_weight <- log(prior$weights) + stats::dnorm(
    estimate,
    prior$means,
    sqrt(prior_variance + standard_error^2),
    log = TRUE
  )
  expected_weight <- exp(expected_log_weight - max(expected_log_weight))
  expected_weight <- expected_weight / sum(expected_weight)

  expect_equal(drop(posterior$means), expected_mean)
  expect_equal(drop(posterior$sds), sqrt(expected_variance))
  expect_equal(drop(posterior$weights), expected_weight)
})


test_that("commensurate posterior summaries are converged at default nodes", {
  for (prior in commensurate_configured_priors()) {
    model <- commensurate_fast_path_model(prior)

    expect_equal(
      commensurate_posterior_summary(model, 48L, 24L),
      commensurate_posterior_summary(model, 192L, 48L),
      tolerance = 5e-4
    )
  }
})


test_that("commensurate ELIR sample size is converged at default nodes", {
  elir <- function(model, n_tau) {
    mixture <- commensurate_prior_mixture(model, n_tau, n_gamma = 24L)
    normal_mixture_elir_ess(
      weights = mixture$weights,
      means = mixture$means,
      sds = mixture$sds,
      sigma = 0.1 * sqrt(60)
    )
  }

  # The ELIR integral weights the prior by its own Fisher information, which
  # makes it the most node-hungry output on this path: the half-normal rule was
  # 1.2% out here while every posterior summary above was already converged to
  # five figures.
  for (prior in commensurate_configured_priors()) {
    model <- commensurate_fast_path_model(prior)
    expect_equal(elir(model, 48L), elir(model, 384L), tolerance = 1e-3)
  }
})


test_that("commensurate simulation fast path returns the pipeline contract", {
  model <- commensurate_fast_path_model(list(
    family = "half_normal",
    std_dev = 2
  ))

  result <- commensurate_fast_path_run(
    model,
    to_return = c(
      "test_decision", "posterior_mean", "posterior_median",
      "credible_interval", "posterior_parameters", "ess_moment",
      "ess_precision", "ess_elir", "fit_success", "mcmc_diagnostics"
    )
  )

  expect_length(result$posterior_means, 3)
  expect_equal(dim(result$credible_intervals), c(3L, 2L))
  expect_equal(nrow(result$posterior_parameters), 3)
  expect_equal(result$fit_success, rep("Success", 3))
  expect_true(all(is.finite(result$ess_elir)))
})


test_that("the fast path reports MCMC diagnostics as not applicable", {
  model <- commensurate_fast_path_model(list(
    family = "half_normal",
    std_dev = 2
  ))

  result <- commensurate_fast_path_run(model, to_return = "mcmc_diagnostics")

  # No sampler runs here. Zeros would read as a chain converged beyond
  # perfectly and a sampler that never diverged, so the diagnostics have to say
  # they do not apply instead.
  expect_equal(result$rhat, rep(NA_real_, 3))
  expect_equal(result$mcmc_ess, rep(NA_real_, 3))
  expect_equal(result$n_divergences, rep(NA_real_, 3))
})


test_that("the fast path refuses a critical value the interval is not drawn at", {
  model <- commensurate_fast_path_model(list(
    family = "half_normal",
    std_dev = 2
  ))

  # Model$test_decision() applies this guard to every model that samples, and
  # this one samples whenever it is not on the fast path.
  expect_error(
    commensurate_fast_path_run(model, "test_decision", critical_value = 0.9),
    "97.5 and 2.5 percentiles"
  )
  expect_no_error(
    commensurate_fast_path_run(model, "test_decision", critical_value = 0.975)
  )

  # With no test decision to make the critical value is never consulted, just
  # as in the replicate loop.
  expect_no_error(
    commensurate_fast_path_run(model, "posterior_mean", critical_value = 0.9)
  )
})


test_that("heterogeneity moments that diverge are reported as infinite", {
  parameters_for <- function(prior) {
    commensurate_fast_path_run(
      commensurate_fast_path_model(prior),
      to_return = "posterior_parameters"
    )$posterior_parameters
  }

  # The target marginal likelihood tends to a positive constant as tau grows,
  # so the posterior inherits whichever prior moments diverge: every configured
  # inverse-gamma shape is below 1/2, and a log-Cauchy tau has no moments at
  # all. A finite number here would only report where the rule was truncated.
  divergent <- list(
    list(family = "inverse_gamma", alpha = 1 / 3, beta = 1),
    list(family = "inverse_gamma", alpha = 1 / 7, beta = 1),
    list(family = "inverse_gamma", alpha = 1 / 1000, beta = 1),
    list(family = "cauchy", location = 0, scale = 10)
  )

  for (prior in divergent) {
    parameters <- parameters_for(prior)
    expect_true(all(is.infinite(parameters$heterogeneity_parameter_mean)))
    expect_true(all(is.infinite(parameters$heterogeneity_parameter_std)))
    # The power parameter is bounded by construction, so it stays reportable.
    expect_true(all(is.finite(parameters$power_parameter_mean)))
    expect_true(all(is.finite(parameters$power_parameter_std)))
  }

  # A half-normal tau has moments of every order, so nothing is overridden.
  finite <- parameters_for(list(family = "half_normal", std_dev = 1))
  expect_true(all(is.finite(finite$heterogeneity_parameter_mean)))
  expect_true(all(is.finite(finite$heterogeneity_parameter_std)))
})
