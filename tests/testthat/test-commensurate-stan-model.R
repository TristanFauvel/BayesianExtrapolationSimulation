commensurate_mcmc_config <- function() {
  list(
    num_chains = 1L,
    parallel_chains = 1L,
    tune = 1L,
    target_accept = 0.8,
    chain_length = 1L,
    max_chain_length = 2L,
    target_ess = 1L,
    rhat_threshold = 1.1
  )
}

commensurate_stan_model <- function() {
  prior <- list(
    source = list(),
    method_parameters = list(
      initial_prior = list("noninformative"),
      heterogeneity_prior = list(family = "half_normal", std_dev = 1)
    )
  )

  testthat::with_mocked_bindings(
    GaussianCommensuratePowerPrior$new(
      prior = prior,
      mcmc_config = commensurate_mcmc_config()
    ),
    compile_stan_model = function(...) NULL,
    .package = "RBExT"
  )
}


test_that("commensurate Stan model updates borrowing parameters with target data", {
  stan_code <- commensurate_stan_model()$stan_model_code

  expect_match(
    stan_code,
    "target_treatment_effect_estimate ~ normal\\s*\\(\\s*source_treatment_effect_estimate",
    perl = TRUE
  )
  expect_match(
    stan_code,
    "target_sampling_variance / NT\\s*\\+ 1 / tau\\s*\\+ prior_variance / \\(power_parameter \\* NS\\)",
    perl = TRUE
  )
})


test_that("commensurate sample sizes must be positive", {
  stan_code <- commensurate_stan_model()$stan_model_code

  expect_match(stan_code, "int<lower=1> NS", fixed = TRUE)
  expect_match(stan_code, "int<lower=1> NT", fixed = TRUE)
})
