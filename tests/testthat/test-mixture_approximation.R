# The mixture approximations the base Model fits are read on the effect scale:
# the effective sample sizes take a standard deviation and a credible interval
# from them against a reference scale in the units of the treatment effect. A
# Beta mixture lives on the [0, 1] rate scale instead, so it can only serve the
# design prior construction, which rescales its shape parameters itself. These
# tests pin which family each field holds, and that the Beta mixture is fitted
# only when something asks for it.

mixture_prior_fixture <- function() {
  list(
    source = list(
      standard_error = 0.1,
      treatment_effect_estimate = 0.25,
      equivalent_source_sample_size_per_arm = 40,
      sample_size_control = 40,
      sample_size_treatment = 40,
      summary_measure_likelihood = "binomial",
      control_rate = 0.25,
      treatment_rate = 0.5
    ),
    method_parameters = list(initial_prior = list("noninformative"))
  )
}

mixture_target_data <- function() {
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

mixture_mcmc_config <- function() {
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

mixture_simulation_config <- function() {
  list(n_samples_mixture_approx = 200)
}

fitted_binomial_model <- function() {
  model <- BinomialSeparate$new(
    prior = mixture_prior_fixture(),
    mcmc_config = mixture_mcmc_config()
  )
  model$inference(target_data = mixture_target_data())
  model
}


test_that("the binomial prior mixture approximation is a normal mixture", {
  set.seed(1)
  model <- fitted_binomial_model()

  model$prior_to_RBesT(mixture_simulation_config()$n_samples_mixture_approx)

  expect_s3_class(model$RBesT_prior, "normMix")
  expect_s3_class(model$RBesT_prior_normix, "normMix")
})


test_that("prior_to_RBesT hands its caller the normal mixture", {
  # TestThenPool stores what the selected component returns, so the return value
  # is what its ELIR is read from.
  set.seed(1)
  model <- fitted_binomial_model()

  returned <- model$prior_to_RBesT(
    mixture_simulation_config()$n_samples_mixture_approx
  )

  expect_s3_class(returned, "normMix")
})


test_that("binomial TestThenPool reads its ELIR off a normal mixture", {
  set.seed(1)
  prior <- mixture_prior_fixture()
  prior$method_parameters$significance_level <- list(0.1)
  model <- TestThenPoolDifference$new(
    prior = prior,
    mcmc_config = mixture_mcmc_config()
  )
  target_data <- mixture_target_data()
  model$separate$posterior_moments(target_data)
  model$pooling$posterior_moments(target_data)
  model$pool <- FALSE

  model$prior_to_RBesT(mixture_simulation_config()$n_samples_mixture_approx)

  expect_s3_class(model$RBesT_prior_normix, "normMix")
})


test_that("the binomial posterior mixture approximation is a normal mixture", {
  set.seed(1)
  model <- fitted_binomial_model()

  model$posterior_to_RBesT(
    target_data = mixture_target_data(),
    simulation_config = mixture_simulation_config()
  )

  expect_s3_class(model$RBesT_posterior, "normMix")
  expect_s3_class(model$RBesT_posterior_normix, "normMix")
})


test_that("posterior_beta_mixture fits the posterior on the rate scale", {
  # The design prior divides the Beta shape parameters by the mixture ESS, so it
  # needs the fit that lives on [0, 1] rather than the one on the effect scale.
  set.seed(1)
  model <- fitted_binomial_model()

  mixture <- model$posterior_beta_mixture(
    target_data = mixture_target_data(),
    simulation_config = mixture_simulation_config()
  )

  expect_s3_class(mixture, "betaMix")
})


test_that("posterior_beta_mixture rejects a model that is not on a rate scale", {
  set.seed(1)
  model <- fitted_binomial_model()
  model$summary_measure_likelihood <- "normal"

  expect_error(
    model$posterior_beta_mixture(
      target_data = mixture_target_data(),
      simulation_config = mixture_simulation_config()
    ),
    "binomial"
  )
})
