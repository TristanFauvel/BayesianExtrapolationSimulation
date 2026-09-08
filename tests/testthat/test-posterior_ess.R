# The effective sample sizes reported per replicate are defined against a normal
# reference: the moment ESS is the reference variance divided by the posterior
# variance, and the precision ESS is the reference variance divided by the
# variance a normal distribution would need to have the same 95% credible
# interval width. Both are then expressed relative to the target study by
# subtracting its per-arm sample size.
#
# The references below evaluate those definitions on the exact posterior, by a
# numerical route independent of the one the models use: adaptive quadrature
# over the control rate rather than the stratified midpoint rule the
# BinomialConjugate posterior is built on.

beta_difference_cdf <- function(x, shape1_control, shape2_control,
                                shape1_treatment, shape2_treatment) {
  vapply(x, function(effect) {
    stats::integrate(
      function(rate) {
        stats::dbeta(rate, shape1_control, shape2_control) *
          stats::pbeta(effect + rate, shape1_treatment, shape2_treatment)
      },
      lower = 0,
      upper = 1,
      rel.tol = 1e-10
    )$value
  }, numeric(1))
}

beta_difference_quantile <- function(probability, shape1_control, shape2_control,
                                     shape1_treatment, shape2_treatment) {
  vapply(probability, function(p) {
    stats::uniroot(
      function(effect) {
        beta_difference_cdf(
          effect, shape1_control, shape2_control,
          shape1_treatment, shape2_treatment
        ) - p
      },
      interval = c(-1, 1),
      tol = 1e-10
    )$root
  }, numeric(1))
}

reference_moment_ess <- function(reference_scale, posterior_variance,
                                 sample_size_per_arm) {
  reference_scale^2 / posterior_variance - sample_size_per_arm
}

reference_precision_ess <- function(reference_scale, lower, upper,
                                    sample_size_per_arm) {
  implied_sd <- ((upper - lower) / 2) / stats::qnorm(0.975)
  reference_scale^2 / implied_sd^2 - sample_size_per_arm
}

ess_noninformative_prior <- function() {
  list(
    source = list(
      sample_size_control = 60,
      sample_size_treatment = 80,
      control_rate = 0.25,
      treatment_rate = 0.5
    ),
    method_parameters = list(initial_prior = list("noninformative"))
  )
}

# Rates chosen so that sample_size * rate is exact in binary arithmetic, which
# keeps the as.integer() truncation in prepare_data out of these tests.
ess_target_data <- function() {
  list(
    summary_measure_likelihood = "binomial",
    sample_size_per_arm = 40,
    sample_size_control = 40,
    sample_size_treatment = 40,
    sample = list(
      sample_control_rate = 0.25,
      sample_treatment_rate = 0.625,
      standard_deviation = 0.5
    )
  )
}

ess_mcmc_config <- function() {
  list(
    num_chains = 4L,
    parallel_chains = 4L,
    tune = 1000L,
    target_accept = 0.9,
    chain_length = 5000L,
    max_chain_length = 10000L,
    target_ess = 10000L,
    rhat_threshold = 1.1,
    max_divergence_rate = 0.01
  )
}


test_that("BinomialConjugate moment ESS uses the exact posterior variance", {
  model <- BinomialSeparate$new(
    prior = ess_noninformative_prior(),
    mcmc_config = ess_mcmc_config()
  )
  target_data <- ess_target_data()
  model$inference(target_data = target_data)

  # target counts: 10/40 control, 25/40 treatment, with Beta(1, 1) priors
  posterior_variance <- 11 * 31 / (42^2 * 43) + 26 * 16 / (42^2 * 43)

  ess <- model$posterior_ess(target_data = target_data, simulation_config = NULL)

  expect_equal(
    ess$moment,
    reference_moment_ess(0.5, posterior_variance, 40),
    tolerance = 1e-8
  )
})


test_that("BinomialConjugate precision ESS uses the exact credible interval", {
  model <- BinomialSeparate$new(
    prior = ess_noninformative_prior(),
    mcmc_config = ess_mcmc_config()
  )
  target_data <- ess_target_data()
  model$inference(target_data = target_data)

  bounds <- beta_difference_quantile(c(0.025, 0.975), 11, 31, 26, 16)

  ess <- model$posterior_ess(target_data = target_data, simulation_config = NULL)

  # The tolerance is set by the 1024-node stratified rule the class integrates
  # the control rate with, which places the interval bounds to about 4e-5. The
  # route this replaced disagreed with the exact value by more than the whole
  # quantity, so this still pins the definition sharply.
  expect_equal(
    ess$precision,
    reference_precision_ess(0.5, bounds[1], bounds[2], 40),
    tolerance = 1e-3
  )
})


test_that("BinomialConjugate separates the two ESS definitions", {
  # The Monte Carlo route fitted a single-component normal mixture, which forced
  # the moment and precision ESS to coincide. The exact posterior of a
  # difference of Beta variables is not normal, so they must differ.
  model <- BinomialSeparate$new(
    prior = ess_noninformative_prior(),
    mcmc_config = ess_mcmc_config()
  )
  target_data <- ess_target_data()
  model$inference(target_data = target_data)

  ess <- model$posterior_ess(target_data = target_data, simulation_config = NULL)

  expect_false(isTRUE(all.equal(ess$moment, ess$precision)))
})


test_that("BinomialConjugate ESS does not draw from the random number stream", {
  model <- BinomialSeparate$new(
    prior = ess_noninformative_prior(),
    mcmc_config = ess_mcmc_config()
  )
  target_data <- ess_target_data()
  model$inference(target_data = target_data)

  set.seed(1)
  before <- .Random.seed
  model$posterior_ess(target_data = target_data, simulation_config = NULL)

  expect_identical(.Random.seed, before)
})


test_that("TestThenPool reports the ESS of the component it selected", {
  # The two branches condition on different counts, so the ESS has to follow the
  # pooling decision rather than being read off a shared posterior.
  target_data <- ess_target_data()
  prior <- ess_noninformative_prior()
  prior$source$summary_measure_likelihood <- "binomial"
  prior$method_parameters$significance_level <- list(0.1)

  model <- TestThenPoolDifference$new(
    prior = prior,
    mcmc_config = ess_mcmc_config()
  )
  model$separate$posterior_moments(target_data)
  model$pooling$posterior_moments(target_data)

  model$pool <- FALSE
  not_pooled <- model$posterior_ess(
    target_data = target_data, simulation_config = NULL
  )
  model$pool <- TRUE
  pooled <- model$posterior_ess(
    target_data = target_data, simulation_config = NULL
  )

  expect_equal(
    not_pooled,
    model$separate$posterior_ess(
      target_data = target_data, simulation_config = NULL
    )
  )
  expect_equal(
    pooled,
    model$pooling$posterior_ess(
      target_data = target_data, simulation_config = NULL
    )
  )
  expect_false(isTRUE(all.equal(not_pooled$moment, pooled$moment)))
})


test_that("the replicate loop reports the exact ESS for a conjugate binomial model", {
  # Two replicates with different counts, handed to the loop instead of being
  # drawn, so the expected values can be written down from the Beta shapes.
  samples <- data.frame(
    sample_control_rate = c(0.25, 0.5),
    sample_treatment_rate = c(0.625, 0.75),
    sample_size_per_arm = c(40, 40),
    treatment_effect_estimate = c(0.375, 0.25),
    treatment_effect_standard_error = c(0.1, 0.1),
    standard_deviation = c(0.5, 0.6)
  )
  target_data <- list(
    summary_measure_likelihood = "binomial",
    sample_size_per_arm = 40,
    sample_size_control = 40,
    sample_size_treatment = 40,
    sample = NULL,
    generate = function(n_replicates) samples
  )
  model <- BinomialSeparate$new(
    prior = ess_noninformative_prior(),
    mcmc_config = ess_mcmc_config()
  )

  results <- model$simulation_for_given_treatment_effect(
    target_data = target_data,
    n_replicates = 2,
    critical_value = 0.975,
    theta_0 = 0,
    confidence_level = 0.95,
    null_space = "left",
    case_study = "aprepitant",
    method = "separate",
    to_return = c("ess_moment", "ess_precision"),
    n_samples_quantiles_estimation = 10000,
    simulation_config = NULL
  )

  # replicate 1: 10/40 control, 25/40 treatment; replicate 2: 20/40 and 30/40
  variance <- c(
    11 * 31 / (42^2 * 43) + 26 * 16 / (42^2 * 43),
    21 * 21 / (42^2 * 43) + 31 * 11 / (42^2 * 43)
  )

  expect_equal(
    results$ess_moments,
    reference_moment_ess(c(0.5, 0.6), variance, 40),
    tolerance = 1e-8
  )
  expect_false(isTRUE(all.equal(results$ess_moments, results$ess_precisions)))
})


test_that("BinomialPooling ESS accounts for the pooled source counts", {
  model <- BinomialPooling$new(
    prior = ess_noninformative_prior(),
    mcmc_config = ess_mcmc_config()
  )
  target_data <- ess_target_data()
  model$inference(target_data = target_data)

  # target: 10/40 control, 25/40 treatment; source: 15/60 control, 40/80 treatment
  posterior_variance <- 26 * 76 / (102^2 * 103) + 66 * 56 / (122^2 * 123)

  ess <- model$posterior_ess(target_data = target_data, simulation_config = NULL)

  expect_equal(
    ess$moment,
    reference_moment_ess(0.5, posterior_variance, 40),
    tolerance = 1e-8
  )
})
