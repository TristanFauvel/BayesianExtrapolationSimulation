# Shared fixtures for the commensurate power prior. Constructing one of these
# models compiles a Stan program, so every test that needs a model has to stub
# the compiler; keeping that in one place stops the stub and the configuration
# from drifting between the Stan-program tests and the fast-path tests.

commensurate_mcmc_config <- function() {
  list(
    num_chains = 1L,
    parallel_chains = 1L,
    tune = 1L,
    target_accept = 0.8,
    chain_length = 1L,
    max_chain_length = 2L,
    target_ess = 1L,
    rhat_threshold = 1.1,
    max_divergence_rate = 0.01
  )
}

commensurate_model <- function(prior, mcmc_config = commensurate_mcmc_config()) {
  model <- testthat::with_mocked_bindings(
    GaussianCommensuratePowerPrior$new(prior = prior, mcmc_config = mcmc_config),
    compile_stan_model = function(...) NULL,
    .package = "BExTE"
  )
  # Model$create() normally installs the prior after constructing the subclass.
  model$prior <- prior
  model
}

# Enough of a prior to read the Stan program off the constructed model.
commensurate_stan_model <- function() {
  commensurate_model(list(
    source = list(),
    method_parameters = list(
      initial_prior = list("noninformative"),
      heterogeneity_prior = list(family = "half_normal", std_dev = 1)
    )
  ))
}

# A fully specified prior, so the quadrature mixture can be built from it.
commensurate_fast_path_model <- function(heterogeneity_prior) {
  commensurate_model(list(
    source = list(
      standard_error = 0.12,
      equivalent_source_sample_size_per_arm = 50,
      treatment_effect_estimate = 0.5
    ),
    method_parameters = list(
      initial_prior = list("noninformative"),
      heterogeneity_prior = heterogeneity_prior
    )
  ))
}

# The heterogeneity priors the shipped configurations run, as listed in
# inst/conf/commensurate_pp_config/methods_config.R.
commensurate_configured_priors <- function() {
  list(
    list(family = "inverse_gamma", alpha = 1 / 3, beta = 1),
    list(family = "inverse_gamma", alpha = 1 / 7, beta = 1),
    list(family = "inverse_gamma", alpha = 1 / 1000, beta = 1),
    list(family = "half_normal", std_dev = 1),
    list(family = "half_normal", std_dev = 5),
    list(family = "cauchy", location = 0, scale = 10)
  )
}

# Posterior mean, standard deviation and quantiles of the treatment effect
# under a mixture built at a given resolution.
commensurate_posterior_summary <- function(model, n_tau, n_gamma,
                                           estimate = 0.9,
                                           standard_error = 0.1) {
  prior <- commensurate_prior_mixture(model, n_tau, n_gamma)
  posterior <- normal_mixture_posterior(
    weights = prior$weights,
    means = prior$means,
    sds = prior$sds,
    estimate = estimate,
    standard_error = standard_error
  )
  summary <- normal_mixture_summary(
    posterior$weights,
    posterior$means,
    posterior$sds
  )

  c(summary$mean, summary$sd, summary$quantiles)
}
