# The robust mixture prior gives the weight w to the informative component:
# that is what the shipped configurations label it, what the vignettes document,
# and what the Gaussian implementations and this class's own prior sampling and
# ELIR both use. These tests pin the Stan program to the same convention.
#
# Sampling means compiling and running the Stan program, so the tests are
# skipped wherever CmdStan is unavailable, as it is in CI.

rmp_stan_mcmc_config <- function() {
  list(
    num_chains = 2L,
    parallel_chains = 1L,
    tune = 1000L,
    target_accept = 0.8,
    chain_length = 2000L,
    max_chain_length = 4000L,
    target_ess = 10L,
    rhat_threshold = 1.1,
    max_divergence_rate = 0.01
  )
}

rmp_stan_model <- function() {
  TruncatedGaussianRMP$new(
    prior = list(
      source = list(
        treatment_effect_estimate = 0.3,
        standard_error = 0.05,
        equivalent_source_sample_size_per_arm = 286
      ),
      vague_mean = 0,
      method_parameters = list(
        prior_weight = list(0.9),
        initial_prior = list("noninformative"),
        empirical_bayes = list(FALSE)
      )
    ),
    mcmc_config = rmp_stan_mcmc_config()
  )
}

skip_without_cmdstan <- function() {
  testthat::skip_if_not(
    !inherits(try(cmdstanr::cmdstan_path(), silent = TRUE), "try-error"),
    "CmdStan is not installed"
  )
}


test_that("the RMP Stan prior gives the prior weight to the informative component", {
  skip_without_cmdstan()

  # An empty treatment arm leaves the treatment effect at its prior, while a
  # large control arm pins the control rate near 0.3, so the prior the draws
  # come from is the mixture truncated to about (-0.3, 0.7).
  data_list <- list(
    n_treatment = 0L,
    n_control = 2000L,
    n_successes_treatment = 0L,
    n_successes_control = 600L,
    w = 0.9,
    vague_mean = 0,
    vague_sd = 0.5,
    info_mean = 0.3,
    info_sd = 0.05
  )

  fit <- rmp_stan_model()$stan_model$sample(
    data = data_list,
    chains = 2,
    parallel_chains = 1,
    iter_warmup = 1000,
    iter_sampling = 2000,
    seed = 20260908,
    refresh = 0,
    show_messages = FALSE
  )

  draws <- as.numeric(fit$draws("target_treatment_effect"))

  # The informative component holds 0.997 of its mass within three of its
  # standard deviations of 0.3, the vague one only 0.307. The share of draws in
  # that window is therefore 0.93 when w weights the informative component and
  # 0.38 when it weights the vague one.
  expect_gt(mean(abs(draws - 0.3) < 0.15), 0.8)
})


test_that("the RMP Stan program is kept on the model, as MCMCModel declares", {
  skip_without_cmdstan()

  expect_match(
    rmp_stan_model()$stan_model_code,
    "log_mix",
    fixed = TRUE
  )
})
