autocorrelated_draws <- function(n_iterations = 1000, n_chains = 4, rho = 0.7) {
  set.seed(20260907)
  values <- vapply(seq_len(n_chains), function(chain) {
    innovations <- stats::rnorm(n_iterations)
    as.numeric(stats::filter(innovations, rho, method = "recursive"))
  }, numeric(n_iterations))

  posterior::as_draws_array(array(
    values,
    dim = c(n_iterations, n_chains, 1),
    dimnames = list(NULL, NULL, "target_treatment_effect")
  ))
}


test_that("summarise_posterior_draws reports moments, interval bounds and diagnostics together", {
  draws <- autocorrelated_draws()

  summary <- summarise_posterior_draws(draws)

  expect_identical(summary$variable, "target_treatment_effect")
  expect_true(all(
    c("mean", "median", "sd", "q2.5", "q97.5", "rhat", "n_eff") %in% names(summary)
  ))
})


test_that("summarise_posterior_draws keeps the estimators the simulation reported before", {
  draws <- autocorrelated_draws()

  summary <- summarise_posterior_draws(draws)

  # The previous code read these from bayesplot::rhat() and
  # bayesplot::neff_ratio(), both of which call fit$summary() and so reach
  # posterior::summarise_draws(). summarise_draws() hands each variable to the
  # estimator as an iterations x chains matrix, and rhat()/ess_basic() give
  # different answers on a matrix than on a draws_array, so the matrix form is
  # the reference to preserve.
  iterations_by_chains <- matrix(
    as.numeric(draws),
    nrow = posterior::niterations(draws),
    ncol = posterior::nchains(draws)
  )

  expect_equal(summary$rhat, posterior::rhat(iterations_by_chains), tolerance = 1e-12)
  # bayesplot::neff_ratio() multiplied back up by the number of draws is
  # posterior::ess_basic(), which is not the same estimator as ess_bulk().
  expect_equal(summary$n_eff, posterior::ess_basic(iterations_by_chains), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(
    summary$n_eff,
    posterior::ess_bulk(iterations_by_chains)
  )))
})


test_that("summarise_posterior_draws gives the credible interval bounds the model reads", {
  draws <- autocorrelated_draws()

  summary <- summarise_posterior_draws(draws)
  values <- as.numeric(draws)

  expect_equal(summary$q2.5, unname(posterior::quantile2(values, 0.025)), tolerance = 1e-12)
  expect_equal(summary$q97.5, unname(posterior::quantile2(values, 0.975)), tolerance = 1e-12)
})


test_that("summarise_posterior_draws summarises every variable it is given", {
  draws <- autocorrelated_draws()
  two_variables <- posterior::bind_draws(
    draws,
    posterior::rename_variables(draws, tau = "target_treatment_effect"),
    along = "variable"
  )

  summary <- summarise_posterior_draws(two_variables)

  expect_setequal(summary$variable, c("target_treatment_effect", "tau"))
})
