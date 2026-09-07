# Reference implementation: the posterior density exactly as specified by the
# Stan model block of BinomialSeparate / BinomialPooling, evaluated on a grid in
# the (control_rate, target_treatment_effect) parameterisation Stan samples in.
#
#   control_rate            ~ uniform(0, 1)
#   target_treatment_effect ~ uniform(-control_rate, 1 - control_rate)
#   n_successes_control   ~ binomial(n_control, control_rate)
#   n_successes_treatment ~ binomial(n_treatment, control_rate + target_treatment_effect)
#
# Both prior densities are identically 1 on their support, so the unnormalised
# posterior is the product of the two binomial likelihoods restricted to the
# region where the treatment rate is a probability.
stan_posterior_reference <- function(n_control,
                                     s_control,
                                     n_treatment,
                                     s_treatment,
                                     m = 1201) {
  eps <- 1e-10
  control_rate <- seq(eps, 1 - eps, length.out = m)
  effect <- seq(-1 + eps, 1 - eps, length.out = m)

  density <- outer(control_rate, effect, function(rate, delta) {
    treatment_rate <- rate + delta
    inside <- treatment_rate > 0 & treatment_rate < 1
    ifelse(
      inside,
      dbinom(s_control, n_control, rate) *
        dbinom(s_treatment, n_treatment, pmin(pmax(treatment_rate, 0), 1)),
      0
    )
  })

  d_rate <- control_rate[2] - control_rate[1]
  d_effect <- effect[2] - effect[1]

  marginal <- colSums(density) * d_rate
  marginal <- marginal / (sum(marginal) * d_effect)

  # Midpoint rather than left Riemann sum, so the reference CDF is second order
  # accurate in the grid spacing.
  cdf <- (cumsum(marginal) - 0.5 * marginal) * d_effect
  mean_effect <- sum(effect * marginal) * d_effect

  list(
    mean = mean_effect,
    var = sum((effect - mean_effect)^2 * marginal) * d_effect,
    quantile = function(p) {
      stats::approx(cdf, effect, xout = p, ties = "ordered")$y
    }
  )
}

noninformative_prior <- function() {
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
target_data_fixture <- function() {
  list(
    sample_size_control = 40,
    sample_size_treatment = 40,
    sample = list(
      sample_control_rate = 0.25,
      sample_treatment_rate = 0.625,
      standard_deviation = 0.5
    )
  )
}

mcmc_config_fixture <- function() {
  list(
    num_chains = 4L,
    parallel_chains = 4L,
    threads_per_chain = 1L,
    tune = 1000L,
    target_accept = 0.9,
    chain_length = 5000L,
    max_chain_length = 10000L,
    target_ess = 10000L,
    rhat_threshold = 1.1
  )
}


test_that("BinomialSeparate posterior moments match the Stan model density", {
  model <- BinomialSeparate$new(
    prior = noninformative_prior(),
    mcmc_config = mcmc_config_fixture()
  )
  target_data <- target_data_fixture()

  expect_equal(model$inference(target_data = target_data), "Success")

  reference <- stan_posterior_reference(
    n_control = 40, s_control = 10,
    n_treatment = 40, s_treatment = 25
  )

  expect_equal(model$post_mean, reference$mean, tolerance = 1e-5)
  expect_equal(model$post_var, reference$var, tolerance = 1e-4)
})


test_that("BinomialSeparate credible interval matches the Stan model density", {
  model <- BinomialSeparate$new(
    prior = noninformative_prior(),
    mcmc_config = mcmc_config_fixture()
  )
  target_data <- target_data_fixture()
  model$inference(target_data = target_data)

  reference <- stan_posterior_reference(
    n_control = 40, s_control = 10,
    n_treatment = 40, s_treatment = 25
  )

  expect_equal(
    model$credible_interval(level = 0.95),
    reference$quantile(c(0.025, 0.975)),
    tolerance = 1e-3
  )
  expect_equal(model$post_median, reference$quantile(0.5), tolerance = 1e-3)
})


test_that("BinomialSeparate posterior CDF matches the Stan model density", {
  model <- BinomialSeparate$new(
    prior = noninformative_prior(),
    mcmc_config = mcmc_config_fixture()
  )
  model$inference(target_data = target_data_fixture())

  reference <- stan_posterior_reference(
    n_control = 40, s_control = 10,
    n_treatment = 40, s_treatment = 25
  )
  probabilities <- c(0.05, 0.25, 0.5, 0.75, 0.95)

  expect_equal(
    model$posterior_cdf(reference$quantile(probabilities)),
    probabilities,
    tolerance = 1e-3
  )
})


test_that("BinomialPooling pools the source counts into the target counts", {
  model <- BinomialPooling$new(
    prior = noninformative_prior(),
    mcmc_config = mcmc_config_fixture()
  )
  model$inference(target_data = target_data_fixture())

  # target: 10/40 control, 25/40 treatment; source: 15/60 control, 40/80 treatment
  reference <- stan_posterior_reference(
    n_control = 100, s_control = 25,
    n_treatment = 120, s_treatment = 65
  )

  expect_equal(model$post_mean, reference$mean, tolerance = 1e-5)
  expect_equal(model$post_var, reference$var, tolerance = 1e-4)
})


test_that("BinomialSeparate reports itself as a non-MCMC model", {
  model <- BinomialSeparate$new(
    prior = noninformative_prior(),
    mcmc_config = mcmc_config_fixture()
  )

  expect_false(model$mcmc)
})


test_that("BinomialSeparate draws posterior samples with the right moments", {
  model <- BinomialSeparate$new(
    prior = noninformative_prior(),
    mcmc_config = mcmc_config_fixture()
  )
  model$inference(target_data = target_data_fixture())

  set.seed(1)
  draws <- model$sample_posterior(n_samples = 200000)

  expect_length(draws, 200000)
  expect_equal(mean(draws), model$post_mean, tolerance = 1e-2)
  expect_equal(var(draws), model$post_var, tolerance = 5e-2)
})


test_that("BinomialSeparate rejects incomplete target data", {
  model <- BinomialSeparate$new(
    prior = noninformative_prior(),
    mcmc_config = mcmc_config_fixture()
  )
  target_data <- target_data_fixture()
  target_data$sample_size_control <- NULL

  expect_error(
    model$inference(target_data = target_data),
    "invalid data"
  )
})
