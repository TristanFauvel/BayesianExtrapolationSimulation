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

stub_fit <- function(draws) {
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

# Records the arguments the model hands to the sampler.
recording_stan_model <- function(draws, calls) {
  calls$sample_args_log <- list()
  list(
    sample = function(...) {
      args <- list(...)
      calls$sample_args <- args
      calls$sample_args_log <- c(calls$sample_args_log, list(args))
      stub_fit(draws)
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
    rhat_threshold = 1.1
  )
}

fitted_stub_model <- function(calls, target_accept = 0.8) {
  model <- StubMCMCModel$new(
    prior = list(),
    mcmc_config = stub_mcmc_config(target_accept)
  )
  model$stan_model <- recording_stan_model(posterior_draws_fixture(), calls)
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
