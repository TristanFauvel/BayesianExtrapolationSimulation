# The prior_weight the binomial robust mixture prior reports is the posterior
# probability that the treatment effect came from the informative component,
# which is what GaussianRMP and GaussianRMP_RBesT report for the case studies
# with a normal summary measure. These tests pin the reported quantity to that
# component, and to the counts the Stan program was actually given.
#
# Nothing here samples: the sampler is stubbed, so what runs is the weight
# computation and the wiring around it.

rmp_weight_prior <- function(prior_weight = 0.5) {
  list(
    source = list(
      treatment_effect_estimate = 0.1315456,
      standard_error = 0.03505291,
      equivalent_source_sample_size_per_arm = 286
    ),
    vague_mean = 0,
    method_parameters = list(
      prior_weight = list(prior_weight),
      initial_prior = list("noninformative"),
      empirical_bayes = list(FALSE)
    )
  )
}

rmp_weight_mcmc_config <- function() {
  list(
    num_chains = 4L,
    parallel_chains = 1L,
    tune = 1000L,
    target_accept = 0.8,
    chain_length = 500L,
    max_chain_length = 10000L,
    target_ess = 10L,
    rhat_threshold = 1.1,
    max_divergence_rate = 0.01
  )
}

# Rates that are exact binary fractions of their arm sizes, so that the counts
# prepare_data derives from them do not depend on how the product rounds.
rmp_weight_target_data <- function(sample_treatment_rate = 0.875,
                                   sample_control_rate = 0.75) {
  list(
    sample_size_treatment = 56L,
    sample_size_control = 52L,
    sample_size_per_arm = 54L,
    sample = list(
      sample_treatment_rate = sample_treatment_rate,
      sample_control_rate = sample_control_rate,
      treatment_effect_estimate = sample_treatment_rate - sample_control_rate,
      treatment_effect_standard_error = 0.0777
    )
  )
}

rmp_stub_fit <- function() {
  set.seed(20260908)
  draws <- posterior::as_draws_array(array(
    stats::rnorm(500 * 4, mean = 0.125, sd = 0.05),
    dim = c(500, 4, 1),
    dimnames = list(NULL, NULL, "target_treatment_effect")
  ))

  list(
    draws = function(variables = NULL, ...) {
      if (is.null(variables)) {
        draws
      } else {
        posterior::subset_draws(draws, variable = variables)
      }
    },
    diagnostic_summary = function(...) list(num_divergent = c(0, 0, 0, 0))
  )
}

# Constructing the model normally compiles its Stan program and inference
# normally samples from it; neither is what these tests are about.
rmp_weight_model <- function(prior_weight = 0.5) {
  model <- testthat::with_mocked_bindings(
    TruncatedGaussianRMP$new(
      prior = rmp_weight_prior(prior_weight),
      mcmc_config = rmp_weight_mcmc_config()
    ),
    compile_stan_model = function(...) NULL,
    .package = "RBExT"
  )
  model$stan_model <- list(sample = function(...) rmp_stub_fit())
  model
}


test_that("a target effect matching the source raises the reported prior weight", {
  model <- rmp_weight_model(prior_weight = 0.5)

  # 0.875 against 0.75 is a difference of 0.125, within a standard error of the
  # source estimate of 0.132.
  model$inference(rmp_weight_target_data())

  expect_gt(model$posterior_parameters$prior_weight, 0.5)
})


test_that("a target effect the source rules out lowers the reported prior weight", {
  model <- rmp_weight_model(prior_weight = 0.5)

  # 0.25 against 0.75 is a difference of -0.5, far outside anything the source
  # estimate supports.
  model$inference(rmp_weight_target_data(sample_treatment_rate = 0.25))

  expect_lt(model$posterior_parameters$prior_weight, 0.01)
})


test_that("the reported prior weight is the informative component's, on the counts Stan was given", {
  model <- rmp_weight_model(prior_weight = 0.3)

  model$inference(rmp_weight_target_data())

  expect_equal(
    model$posterior_parameters$prior_weight,
    truncated_normal_mixture_binomial_weights(
      weights = c(0.3, 0.7),
      means = c(0.1315456, 0),
      sds = c(0.03505291, sqrt(0.03505291^2 * 286)),
      n_control = 52L,
      n_successes_control = 39L,
      n_treatment = 56L,
      n_successes_treatment = 49L
    )[1]
  )
})


test_that("the reported prior weight is a probability", {
  model <- rmp_weight_model(prior_weight = 0.5)

  model$inference(rmp_weight_target_data())

  expect_gte(model$posterior_parameters$prior_weight, 0)
  expect_lte(model$posterior_parameters$prior_weight, 1)
})
