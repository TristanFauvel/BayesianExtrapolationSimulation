# The robust mixture prior for a binary endpoint is a two-component normal
# mixture truncated to (-1, 1), and it is an empirical Bayes prior: its vague
# component is scaled by the observed target standard error, so it changes with
# every replicate. These tests pin where its ELIR effective sample size comes
# from, and that the replicate loop asks the model for it rather than refitting
# a mixture to samples.

rmp_prior_fixture <- function(prior_weight = 0.5) {
  list(
    source = list(
      treatment_effect_estimate = 0.1315456,
      standard_error = 0.03505291,
      equivalent_source_sample_size_per_arm = 286,
      summary_measure_likelihood = "binomial"
    ),
    vague_mean = 0,
    method_parameters = list(
      prior_weight = list(prior_weight),
      initial_prior = list("noninformative"),
      empirical_bayes = list(TRUE)
    )
  )
}

rmp_mcmc_config <- function() {
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

# The binary generator ties the two together: the standard error is the sampling
# standard deviation divided by the square root of the per-arm sample size.
rmp_target_data <- function(standard_error = 0.029) {
  list(
    summary_measure_likelihood = "binomial",
    sample_size_per_arm = 286,
    sample = list(
      treatment_effect_standard_error = standard_error,
      standard_deviation = standard_error * sqrt(286)
    )
  )
}

# Building the model normally compiles its Stan program, which the rest of the
# suite avoids. Nothing below samples, so the compilation is stubbed out.
uncompiled_rmp <- function(prior_weight = 0.5) {
  with_mocked_bindings(
    TruncatedGaussianRMP$new(
      prior = rmp_prior_fixture(prior_weight),
      mcmc_config = rmp_mcmc_config()
    ),
    compile_stan_model = function(...) NULL
  )
}


test_that("TruncatedGaussianRMP integrates the ELIR over its truncated prior", {
  model <- uncompiled_rmp()
  target_data <- rmp_target_data()
  model$empirical_bayes_update(target_data)

  result <- model$prior_elir_ess(
    target_data = target_data, simulation_config = NULL
  )

  expect_equal(
    result,
    truncated_normal_mixture_elir(
      weights = c(0.5, 0.5),
      means = c(0, 0.1315456),
      sds = c(0.029 * sqrt(286), 0.03505291),
      lower = -1,
      upper = 1,
      sigma = 0.029 * sqrt(286)
    )
  )
})


test_that("TruncatedGaussianRMP ELIR follows the empirical Bayes update", {
  # The vague component is set from the observed target standard error, so the
  # reported ELIR has to be the one implied by the current replicate rather than
  # a value carried over from an earlier one.
  model <- uncompiled_rmp()
  first <- rmp_target_data(standard_error = 0.01)
  second <- rmp_target_data(standard_error = 0.05)

  model$empirical_bayes_update(first)
  model$prior_elir_ess(target_data = first, simulation_config = NULL)
  model$empirical_bayes_update(second)
  after_update <- model$prior_elir_ess(
    target_data = second, simulation_config = NULL
  )

  expect_equal(
    after_update,
    truncated_normal_mixture_elir(
      weights = c(0.5, 0.5),
      means = c(0, 0.1315456),
      sds = c(0.05 * sqrt(286), 0.03505291),
      lower = -1,
      upper = 1,
      sigma = 0.05 * sqrt(286)
    )
  )
})


test_that("TruncatedGaussianRMP ELIR respects the prior weight", {
  target_data <- rmp_target_data()

  mostly_informative <- uncompiled_rmp(prior_weight = 0.9)
  mostly_informative$empirical_bayes_update(target_data)
  mostly_vague <- uncompiled_rmp(prior_weight = 0.1)
  mostly_vague$empirical_bayes_update(target_data)

  # The informative component is more than an order of magnitude narrower than
  # the vague one, so weighting it more heavily concentrates the prior and
  # raises the information it carries.
  expect_gt(
    mostly_informative$prior_elir_ess(
      target_data = target_data, simulation_config = NULL
    ),
    mostly_vague$prior_elir_ess(
      target_data = target_data, simulation_config = NULL
    )
  )
})


test_that("TruncatedGaussianRMP ELIR does not draw from the random number stream", {
  model <- uncompiled_rmp()
  target_data <- rmp_target_data()
  model$empirical_bayes_update(target_data)

  set.seed(1)
  before <- .Random.seed
  model$prior_elir_ess(target_data = target_data, simulation_config = NULL)

  expect_identical(.Random.seed, before)
})


test_that("the replicate loop takes the ELIR from the model", {
  # Models whose prior has a closed-form ELIR override prior_elir_ess. The loop
  # has to route through it rather than fitting a mixture to prior samples
  # itself, otherwise those overrides never take effect in a simulation.
  RecordingELIRModel <- R6::R6Class(
    "RecordingELIRModel",
    inherit = BinomialSeparate,
    public = list(
      prior_elir_ess = function(target_data, ...) {
        -target_data$sample$standard_deviation
      }
    )
  )

  samples <- data.frame(
    sample_control_rate = c(0.25, 0.5, 0.25),
    sample_treatment_rate = c(0.625, 0.75, 0.5),
    sample_size_per_arm = c(40, 40, 40),
    treatment_effect_estimate = c(0.375, 0.25, 0.25),
    treatment_effect_standard_error = c(0.1, 0.1, 0.1),
    standard_deviation = c(0.5, 0.6, 0.7)
  )
  target_data <- list(
    summary_measure_likelihood = "binomial",
    sample_size_per_arm = 40,
    sample_size_control = 40,
    sample_size_treatment = 40,
    sample = NULL,
    generate = function(n_replicates) samples
  )
  model <- RecordingELIRModel$new(
    prior = list(
      source = list(
        sample_size_control = 60, sample_size_treatment = 80,
        control_rate = 0.25, treatment_rate = 0.5
      ),
      method_parameters = list(initial_prior = list("noninformative"))
    ),
    mcmc_config = NULL
  )

  results <- model$simulation_for_given_treatment_effect(
    target_data = target_data,
    n_replicates = 3,
    critical_value = 0.975,
    theta_0 = 0,
    confidence_level = 0.95,
    null_space = "left",
    case_study = "aprepitant",
    method = "separate",
    to_return = "ess_elir",
    n_samples_quantiles_estimation = 10000,
    simulation_config = list(n_samples_mixture_approx = 1000)
  )

  expect_equal(results$ess_elir, -samples$standard_deviation)
})


# The ELIR is sigma^2 times an expectation taken under the prior, and that
# expectation does not involve sigma. So for a prior that does not move between
# replicates the integral is the same every time and only the reference scale
# changes, which is what these two tests hold the implementation to.

elir_prior_fixture <- function() {
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

elir_target_data <- function(standard_deviation) {
  list(
    summary_measure_likelihood = "binomial",
    sample_size_per_arm = 40,
    sample_size_control = 40,
    sample_size_treatment = 40,
    sample = list(standard_deviation = standard_deviation)
  )
}

elir_model <- function() {
  BinomialSeparate$new(
    prior = elir_prior_fixture(),
    mcmc_config = list(
      num_chains = 4L, parallel_chains = 4L, tune = 1000L, target_accept = 0.9,
      chain_length = 5000L, max_chain_length = 10000L, target_ess = 10000L,
      rhat_threshold = 1.1, max_divergence_rate = 0.01
    )
  )
}


test_that("a prior that does not move has its ELIR integral taken once", {
  set.seed(11)
  model <- elir_model()
  calls <- 0L

  with_mocked_bindings(
    {
      for (standard_deviation in c(0.5, 0.6, 0.7, 0.8)) {
        model$prior_elir_ess(
          target_data = elir_target_data(standard_deviation),
          simulation_config = list(n_samples_mixture_approx = 100)
        )
      }
    },
    prior_ess_elir = function(...) {
      calls <<- calls + 1L
      1
    }
  )

  expect_equal(calls, 1L)
})


test_that("the ELIR follows the reference scale it is quoted against", {
  set.seed(11)
  model <- elir_model()
  config <- list(n_samples_mixture_approx = 100)

  first <- model$prior_elir_ess(
    target_data = elir_target_data(0.5), simulation_config = config
  )
  second <- model$prior_elir_ess(
    target_data = elir_target_data(1.5), simulation_config = config
  )

  # Against the mixture the model actually fitted, so this pins the value and
  # not merely the ratio.
  mixture <- model$RBesT_prior_normix
  RBesT::sigma(mixture) <- 0.5
  expect_equal(first, RBesT::ess(mixture, method = "elir", sigma = 0.5))
  expect_equal(second, first * (1.5 / 0.5)^2)
})
