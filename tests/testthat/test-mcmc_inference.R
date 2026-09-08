# CmdStan cannot be exercised here, so the sampler itself is stubbed. Everything
# on this side of the process boundary - the summary pass, the moments and the
# diagnostics - runs for real against genuine posterior draws.
posterior_draws_fixture <- function(n_iterations = 500, n_chains = 4) {
  set.seed(20260907)
  posterior::as_draws_array(array(
    stats::rnorm(n_iterations * n_chains, mean = 0.3, sd = 0.1),
    dim = c(n_iterations, n_chains, 1),
    dimnames = list(NULL, NULL, "target_treatment_effect")
  ))
}

stub_fit <- function(draws, num_divergent = c(0, 0, 0, 0)) {
  list(
    draws = function(variables = NULL, ...) {
      if (is.null(variables)) {
        draws
      } else {
        posterior::subset_draws(draws, variable = variables)
      }
    },
    diagnostic_summary = function(...) list(num_divergent = num_divergent)
  )
}

# Records the arguments the model hands to the sampler.
recording_stan_model <- function(draws, calls, num_divergent = c(0, 0, 0, 0)) {
  calls$sample_args_log <- list()
  list(
    sample = function(...) {
      args <- list(...)
      calls$sample_args <- args
      calls$sample_args_log <- c(calls$sample_args_log, list(args))
      stub_fit(draws, num_divergent)
    }
  )
}

StubMCMCModel <- R6::R6Class(
  "StubMCMCModel",
  inherit = MCMCModel,
  public = list(
    prepare_data = function(target_data) list(n_observations = 10L)
  )
)

stub_mcmc_config <- function(target_accept = 0.8) {
  list(
    num_chains = 4L,
    parallel_chains = 1L,
    tune = 1000L,
    target_accept = target_accept,
    chain_length = 500L,
    max_chain_length = 10000L,
    target_ess = 10L,
    rhat_threshold = 1.1,
    max_divergence_rate = 0.01
  )
}

fitted_stub_model <- function(calls, target_accept = 0.8,
                              num_divergent = c(0, 0, 0, 0)) {
  model <- StubMCMCModel$new(
    prior = list(),
    mcmc_config = stub_mcmc_config(target_accept)
  )
  model$stan_model <- recording_stan_model(
    posterior_draws_fixture(), calls, num_divergent
  )
  model
}


test_that("inference passes the configured target acceptance rate to the sampler", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls)

  model$inference(target_data = NULL)

  expect_equal(calls$sample_args$adapt_delta, 0.8)
})


test_that("inference forwards a changed target acceptance rate", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls, target_accept = 0.95)

  model$inference(target_data = NULL)

  expect_equal(calls$sample_args$adapt_delta, 0.95)
})


test_that("inference forwards the rest of the sampler configuration", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls)

  model$inference(target_data = NULL)

  expect_identical(calls$sample_args$chains, 4L)
  expect_identical(calls$sample_args$parallel_chains, 1L)
  expect_identical(calls$sample_args$iter_sampling, 500L)
  expect_identical(calls$sample_args$iter_warmup, 1000L)
})


test_that("inference takes its moments and diagnostics from the posterior summary", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls)
  draws <- posterior_draws_fixture()
  expected <- summarise_posterior_draws(draws)

  expect_equal(model$inference(target_data = NULL), "Success")

  expect_equal(model$post_mean, expected$mean)
  expect_equal(model$post_var, expected$sd^2)
  expect_equal(model$post_median, expected$median)
  expect_equal(model$mcmc_ess, expected$n_eff)
  expect_equal(model$rhat, expected$rhat)
  expect_equal(model$credible_interval(), c(expected$q2.5, expected$q97.5))
})


test_that("a target acceptance rate outside the unit interval is rejected", {
  expect_error(
    StubMCMCModel$new(prior = list(), mcmc_config = stub_mcmc_config(1.2)),
    "target_accept"
  )
  expect_error(
    StubMCMCModel$new(prior = list(), mcmc_config = stub_mcmc_config(0)),
    "target_accept"
  )
})


test_that("a valid target acceptance rate is accepted", {
  expect_no_error(
    StubMCMCModel$new(prior = list(), mcmc_config = stub_mcmc_config(0.99))
  )
})


test_that("inference does not ask the sampler for per-fit console output", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls)

  model$inference(target_data = NULL)

  expect_identical(calls$sample_args$refresh, 0)
  expect_false(calls$sample_args$show_messages)
})


test_that("stan_sampler_seed yields a fresh seed each time", {
  set.seed(1)
  seeds <- replicate(20, stan_sampler_seed())

  expect_length(unique(seeds), 20)
})


test_that("stan_sampler_seed is reproducible from the session seed", {
  set.seed(42)
  first <- replicate(5, stan_sampler_seed())
  set.seed(42)
  second <- replicate(5, stan_sampler_seed())

  expect_identical(first, second)
})


test_that("stan_sampler_seed stays within the range CmdStan accepts", {
  set.seed(1)
  seeds <- replicate(100, stan_sampler_seed())

  expect_true(all(seeds > 0))
  expect_true(all(seeds <= .Machine$integer.max))
  expect_identical(seeds, as.integer(seeds))
})


test_that("inference seeds the sampler", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls)

  set.seed(1)
  model$inference(target_data = NULL)

  expect_true(is.numeric(calls$sample_args$seed))
  expect_length(calls$sample_args$seed, 1)
})


test_that("inference gives every replicate a different sampler seed", {
  # A single fixed seed would make every replicate share one random stream,
  # correlating draws across replicates that are meant to be independent.
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls)

  set.seed(1)
  model$inference(target_data = NULL)
  model$inference(target_data = NULL)

  seeds <- vapply(calls$sample_args_log, function(args) args$seed, numeric(1))
  expect_length(unique(seeds), 2)
})


test_that("the sequence of sampler seeds is reproducible", {
  seeds_for_run <- function() {
    calls <- new.env(parent = emptyenv())
    model <- fitted_stub_model(calls)
    set.seed(20260907)
    model$inference(target_data = NULL)
    model$inference(target_data = NULL)
    vapply(calls$sample_args_log, function(args) args$seed, numeric(1))
  }

  expect_identical(seeds_for_run(), seeds_for_run())
})


test_that("inference reports a warning when divergences exceed the allowed rate", {
  calls <- new.env(parent = emptyenv())
  # 2000 post-warmup draws (4 chains of 500); 100 divergences is a rate of 0.05,
  # above the configured 0.01.
  model <- fitted_stub_model(calls, num_divergent = c(40, 30, 20, 10))

  status <- model$inference(target_data = NULL)

  expect_match(status, "diverge")
  expect_match(status, "0.05", fixed = TRUE)
})


test_that("inference succeeds when divergences are within the allowed rate", {
  calls <- new.env(parent = emptyenv())
  # 10 of 2000 draws is a rate of 0.005, below the configured 0.01.
  model <- fitted_stub_model(calls, num_divergent = c(4, 3, 2, 1))

  expect_identical(model$inference(target_data = NULL), "Success")
})


test_that("inference records the divergence count whether or not it warns", {
  for (divergent in list(c(4, 3, 2, 1), c(40, 30, 20, 10))) {
    calls <- new.env(parent = emptyenv())
    model <- fitted_stub_model(calls, num_divergent = divergent)

    model$inference(target_data = NULL)

    expect_identical(model$n_divergences, sum(divergent))
  }
})


test_that("the existing ESS and rhat warnings still take precedence", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls, num_divergent = c(40, 30, 20, 10))
  model$mcmc_config$target_ess <- 1e9

  status <- model$inference(target_data = NULL)

  expect_match(status, "ESS")
})


test_that("a divergence rate outside the unit interval is rejected", {
  bad <- function(rate) {
    config <- stub_mcmc_config()
    config$max_divergence_rate <- rate
    config
  }

  expect_error(StubMCMCModel$new(prior = list(), mcmc_config = bad(-0.1)),
               "max_divergence_rate")
  expect_error(StubMCMCModel$new(prior = list(), mcmc_config = bad(1.5)),
               "max_divergence_rate")
  expect_no_error(StubMCMCModel$new(prior = list(), mcmc_config = bad(0)))
})


ess_stub_target_data <- function() {
  list(
    summary_measure_likelihood = "binomial",
    sample_size_per_arm = 40,
    sample = list(standard_deviation = 0.5)
  )
}


test_that("posterior_ess takes the moment ESS from the posterior draw summary", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls)
  target_data <- ess_stub_target_data()
  model$inference(target_data = target_data)
  expected <- summarise_posterior_draws(posterior_draws_fixture())

  ess <- model$posterior_ess(
    target_data = target_data,
    simulation_config = list(n_samples_mixture_approx = 1000)
  )

  expect_equal(ess$moment, 0.5^2 / expected$sd^2 - 40)
})


test_that("posterior_ess takes the precision ESS from the posterior draw summary", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls)
  target_data <- ess_stub_target_data()
  model$inference(target_data = target_data)
  expected <- summarise_posterior_draws(posterior_draws_fixture())
  implied_sd <- ((expected$q97.5 - expected$q2.5) / 2) / stats::qnorm(0.975)

  ess <- model$posterior_ess(
    target_data = target_data,
    simulation_config = list(n_samples_mixture_approx = 1000)
  )

  expect_equal(ess$precision, 0.5^2 / implied_sd^2 - 40)
})


test_that("posterior_ess does not resample the posterior it already has draws from", {
  calls <- new.env(parent = emptyenv())
  model <- fitted_stub_model(calls)
  target_data <- ess_stub_target_data()
  model$inference(target_data = target_data)

  set.seed(1)
  before <- .Random.seed
  model$posterior_ess(
    target_data = target_data,
    simulation_config = list(n_samples_mixture_approx = 1000)
  )

  expect_identical(.Random.seed, before)
})
