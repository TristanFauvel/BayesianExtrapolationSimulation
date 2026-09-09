## The RMP prior degenerates to a single component at the ends of the weight
## sweep, and inst/conf/full/methods_config.R sweeps
## seq(0, 1, length.out = 11), so w = 0 and w = 1 are both real configurations
## rather than edge cases nobody runs.
##
## vectorised_prior_components() returns those two cases in different shapes -
## n_replicates x 1 matrices at w = 0, plain scalars at w = 1 - and that
## asymmetry is deliberate: normal_mixture_posterior() dispatches on shape, and
## three non-matrices take shared_normal_mixture_posterior(), which broadcasts
## with outer() and so reads its prior arguments as the *component* axis. A
## scalar there is one component shared by every replicate (what w = 1 wants);
## a per-replicate vector is read as one component per replicate and the update
## goes non-conformable, which is what w = 0 used to do.
##
## What matters either way is the posterior these produce: one component per
## replicate.

rmp_test_case_study_config <- function() {
  list(
    name = "unit_test",
    endpoint = "continuous",
    summary_measure_likelihood = "normal",
    sampling_approximation = TRUE,
    theta_0 = 0,
    null_space = "left",
    source = list(
      control = 100,
      treatment = 100,
      treatment_effect = 0.5,
      standard_error = 0.12
    )
  )
}

#' The RMP model, its target data and one draw of replicates, for a weight.
rmp_fixture <- function(w, n_replicates = 7, empirical_bayes = TRUE) {
  case_study_config <- rmp_test_case_study_config()
  source_data <- SourceData$new(case_study_config)
  target_data <- TargetDataFactory$new()$create(
    source_data = source_data,
    case_study_config = case_study_config,
    target_sample_size_per_arm = 60,
    control_drift = 0,
    treatment_drift = 0,
    summary_measure_likelihood = "normal",
    target_to_source_std_ratio = 1
  )
  model <- Model$new()$create(
    case_study_config = case_study_config,
    method = "RMP",
    method_parameters = list(
      initial_prior = list("noninformative"),
      prior_weight = list(w),
      empirical_bayes = list(empirical_bayes)
    ),
    source_data = source_data
  )
  set.seed(1)
  list(model = model, target_data = target_data,
       samples = target_data$generate(n_replicates))
}

#' The conjugate update the vectorised kernel performs, for one weight.
rmp_posterior_for_weight <- function(w, n_replicates = 7, empirical_bayes = TRUE) {
  f <- rmp_fixture(w, n_replicates, empirical_bayes)
  prior <- f$model$vectorised_prior_components(f$target_data, f$samples)
  normal_mixture_posterior(
    weights = prior$weights,
    means = prior$means,
    sds = prior$sds,
    estimate = f$samples$treatment_effect_estimate,
    standard_error = f$samples$treatment_effect_standard_error
  )
}

test_that("the RMP prior update gives one component per replicate at w = 0", {
  n_replicates <- 7

  posterior <- rmp_posterior_for_weight(0, n_replicates)

  expect_equal(dim(as.matrix(posterior$means)), c(n_replicates, 1))
  expect_equal(dim(as.matrix(posterior$sds)), c(n_replicates, 1))
  expect_equal(dim(as.matrix(posterior$weights)), c(n_replicates, 1))
})

test_that("the RMP prior update gives one component per replicate at w = 1", {
  n_replicates <- 7

  posterior <- rmp_posterior_for_weight(1, n_replicates)

  expect_equal(dim(as.matrix(posterior$means)), c(n_replicates, 1))
  expect_equal(dim(as.matrix(posterior$sds)), c(n_replicates, 1))
  expect_equal(dim(as.matrix(posterior$weights)), c(n_replicates, 1))
})

test_that("w = 0 collapses to one component without an empirical Bayes vague prior", {
  n_replicates <- 7

  posterior <- rmp_posterior_for_weight(0, n_replicates, empirical_bayes = FALSE)

  expect_equal(dim(as.matrix(posterior$means)), c(n_replicates, 1))
})

test_that("an interior weight still gives the two-component mixture", {
  n_replicates <- 7

  posterior <- rmp_posterior_for_weight(0.5, n_replicates)

  expect_equal(dim(as.matrix(posterior$means)), c(n_replicates, 2))
  ## The posterior weights are a genuine mixture, not a collapsed component.
  expect_equal(rowSums(posterior$weights), rep(1, n_replicates))
})
