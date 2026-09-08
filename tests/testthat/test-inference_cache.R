# For a binary endpoint the analysis sees the data only through the number of
# responders in each arm, so replicates that landed on the same counts have the
# same answer. The counts recur often: at 40 patients per arm there are far
# fewer plausible pairs than there are replicates, and the pairs do not depend
# on the drift, which shifts the sampling distribution rather than the analysis.
# These tests pin that repeated data is computed once, that scenarios differing
# only in drift share the work, and that scenarios differing in anything the
# analysis reads do not.

cache_prior_fixture <- function(control_rate = 0.25) {
  list(
    source = list(
      standard_error = 0.1,
      treatment_effect_estimate = 0.25,
      equivalent_source_sample_size_per_arm = 40,
      sample_size_control = 40,
      sample_size_treatment = 40,
      summary_measure_likelihood = "binomial",
      control_rate = control_rate,
      treatment_rate = 0.5
    ),
    method_parameters = list(initial_prior = list("noninformative"))
  )
}

cache_mcmc_config <- function() {
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

# The generator turns counts into a row; reproducing that here keeps the tests
# independent of the random draw while feeding the loop exactly what it sees.
cache_samples <- function(successes_control, successes_treatment, n = 40) {
  control_rate <- successes_control / n
  treatment_rate <- successes_treatment / n
  data.frame(
    sample_control_rate = control_rate,
    sample_treatment_rate = treatment_rate,
    sample_size_per_arm = n,
    treatment_effect_estimate = treatment_rate - control_rate,
    treatment_effect_standard_error = sqrt(
      treatment_rate * (1 - treatment_rate) / n +
        control_rate * (1 - control_rate) / n
    ),
    standard_deviation = sqrt(
      treatment_rate * (1 - treatment_rate) +
        control_rate * (1 - control_rate)
    )
  )
}

cache_target_data <- function(samples, n = 40) {
  list(
    summary_measure_likelihood = "binomial",
    sample_size_per_arm = n,
    sample_size_control = n,
    sample_size_treatment = n,
    sample = samples[1, , drop = FALSE],
    generate = function(n_replicates) samples
  )
}

cache_simulation <- function(model, target_data, to_return = c(
                               "test_decision",
                               "posterior_mean",
                               "credible_interval",
                               "ess_moment",
                               "ess_precision",
                               "ess_elir",
                               "fit_success"
                             )) {
  model$simulation_for_given_treatment_effect(
    target_data = target_data,
    n_replicates = nrow(target_data$generate(0)),
    critical_value = 0.975,
    theta_0 = 0,
    confidence_level = 0.95,
    null_space = "left",
    case_study = "example",
    method = "separate",
    to_return = to_return,
    n_samples_quantiles_estimation = 100,
    simulation_config = list(n_samples_mixture_approx = 100)
  )
}

cache_model <- function(control_rate = 0.25) {
  BinomialSeparate$new(
    prior = cache_prior_fixture(control_rate = control_rate),
    mcmc_config = cache_mcmc_config()
  )
}


test_that("repeated counts are analysed once", {
  inference_cache_reset()
  samples <- cache_samples(
    successes_control    = c(10, 14, 10, 14, 10),
    successes_treatment  = c(25, 22, 25, 22, 25)
  )

  cache_simulation(cache_model(), cache_target_data(samples))

  expect_equal(inference_cache_size(), 2)
})


test_that("caching leaves every reported result unchanged", {
  samples <- cache_samples(
    successes_control    = c(10, 14, 10, 14, 10),
    successes_treatment  = c(25, 22, 25, 22, 25)
  )

  # Both arms are seeded alike: the ELIR reads a mixture fitted to prior draws,
  # so leaving the stream where the other arm left it would move it for reasons
  # that have nothing to do with the cache.
  uncached_model <- cache_model()
  uncached_model$deterministic_inference <- FALSE
  inference_cache_reset()
  set.seed(20240923)
  uncached <- cache_simulation(uncached_model, cache_target_data(samples))

  inference_cache_reset()
  set.seed(20240923)
  cached <- cache_simulation(cache_model(), cache_target_data(samples))

  expect_equal(cached, uncached)
})


test_that("scenarios differing only in drift share the analysis", {
  # Drift moves the sampling distribution of the counts, not the posterior they
  # imply, so the second scenario should add only the counts the first missed.
  inference_cache_reset()
  low_drift <- cache_samples(
    successes_control   = c(10, 14),
    successes_treatment = c(25, 22)
  )
  high_drift <- cache_samples(
    successes_control   = c(10, 18),
    successes_treatment = c(25, 30)
  )

  cache_simulation(cache_model(), cache_target_data(low_drift))
  after_first <- inference_cache_size()
  cache_simulation(cache_model(), cache_target_data(high_drift))

  expect_equal(after_first, 2)
  expect_equal(inference_cache_size(), 3)
})


test_that("a different source study does not read another scenario's results", {
  # A pooled analysis conditions on the source counts as well as the target
  # ones, so the same target counts have a different posterior under a different
  # source study and must not be served from it.
  samples <- cache_samples(successes_control = 10, successes_treatment = 25)
  pooling_model <- function(control_rate) {
    BinomialPooling$new(
      prior = cache_prior_fixture(control_rate = control_rate),
      mcmc_config = cache_mcmc_config()
    )
  }

  inference_cache_reset()
  first <- cache_simulation(pooling_model(0.25), cache_target_data(samples))
  second <- cache_simulation(pooling_model(0.4), cache_target_data(samples))

  expect_equal(inference_cache_size(), 2)
  expect_false(isTRUE(all.equal(first$posterior_means, second$posterior_means)))
})


test_that("two models built from the same source study share the cache", {
  # A model is rebuilt for every scenario, so the scope has to key on what the
  # source study says rather than on the identity of the object saying it.
  # Hashing an R6 object whole would key on its environment and never match.
  case_study_config <- yaml::read_yaml(
    system.file("conf/case_studies/aprepitant.yml", package = "RBExT")
  )
  samples <- cache_samples(successes_control = 10, successes_treatment = 25)
  built <- function() {
    BinomialSeparate$new(
      prior = list(
        source = as.list(SourceData$new(case_study_config, 1)),
        method_parameters = list(initial_prior = list("noninformative"))
      ),
      mcmc_config = cache_mcmc_config()
    )
  }

  inference_cache_reset()
  cache_simulation(built(), cache_target_data(samples))
  cache_simulation(built(), cache_target_data(samples))

  expect_equal(inference_cache_size(), 1)
})


test_that("a model whose inference is not deterministic is not cached", {
  inference_cache_reset()
  samples <- cache_samples(
    successes_control   = c(10, 10),
    successes_treatment = c(25, 25)
  )
  model <- cache_model()
  model$deterministic_inference <- FALSE

  cache_simulation(model, cache_target_data(samples))

  expect_equal(inference_cache_size(), 0)
})


test_that("an empirical Bayes model is refused a cache scope", {
  # Its prior is derived from the replicate's own data by the inference a cache
  # hit skips, so it must stay out of the cache however it is declared.
  model <- cache_model()
  model$empirical_bayes <- TRUE
  target_data <- cache_target_data(
    cache_samples(successes_control = 10, successes_treatment = 25)
  )

  scope <- model$inference_cache_scope(
    target_data = target_data,
    critical_value = 0.975,
    theta_0 = 0,
    confidence_level = 0.95,
    null_space = "left",
    n_samples_quantiles_estimation = 100,
    simulation_config = list(n_samples_mixture_approx = 100)
  )

  expect_null(scope)
})
