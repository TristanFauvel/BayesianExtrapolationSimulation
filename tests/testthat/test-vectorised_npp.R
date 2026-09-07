# The normalised power prior puts a Beta prior on the power parameter, so the
# prior on the treatment effect is a continuous mixture of normals:
#   p(theta) = integral over gamma of N(theta; est_S, se_S^2 / gamma) dBeta(gamma)
# Discretising that integral on a quadrature grid turns the whole method into an
# ordinary normal mixture, which the conjugate machinery already handles. These
# tests pin the discretisation against the existing nested-integration code.

npp_test_model <- function(power_parameter_std = 0.2) {
  case_study_config <- list(
    name = "unit_test", endpoint = "continuous",
    summary_measure_likelihood = "normal", sampling_approximation = TRUE,
    theta_0 = 0, null_space = "left",
    source = list(control = 562, treatment = 563,
                  treatment_effect = 0.481, standard_error = 0.1208)
  )
  source_data <- SourceData$new(case_study_config)
  model <- Model$new()$create(
    case_study_config = case_study_config, method = "NPP",
    method_parameters = list(initial_prior = list("noninformative"),
                             power_parameter_mean = list(0.5),
                             power_parameter_std = list(power_parameter_std)),
    source_data = source_data
  )
  list(model = model, source_data = source_data,
       case_study_config = case_study_config)
}

test_that("the vectorised NPP kernel reproduces the scalar kernel", {
  fixture <- npp_test_model()
  target_data <- TargetDataFactory$new()$create(
    source_data = fixture$source_data,
    case_study_config = fixture$case_study_config,
    target_sample_size_per_arm = 46, control_drift = 0, treatment_drift = 0,
    summary_measure_likelihood = "normal", target_to_source_std_ratio = 1
  )

  scalar_class <- R6::R6Class(
    "ScalarOnlyNPP", inherit = Gaussian_NPP,
    public = list(vectorised_replicate_inference = function(...) NULL)
  )
  reference <- scalar_class$new(prior = fixture$model$prior)
  reference$prior <- fixture$model$prior

  # The effective sample sizes are excluded here: the scalar path derives them
  # by fitting a mixture to random draws from the posterior, so they are not
  # reproducible even on identical data. They are checked separately below.
  to_return <- c("test_decision", "posterior_mean", "posterior_median",
                 "credible_interval", "posterior_parameters", "fit_success")

  run <- function(model) {
    set.seed(21)
    model$simulation_for_given_treatment_effect(
      target_data = target_data, n_replicates = 25, critical_value = 0.975,
      theta_0 = 0, confidence_level = 0.95, null_space = "left",
      case_study = "unit_test", method = "NPP", to_return = to_return,
      n_samples_quantiles_estimation = 100
    )
  }

  vectorised <- run(fixture$model)
  scalar <- run(reference)

  expect_equal(vectorised$test_decisions, scalar$test_decisions)
  expect_equal(vectorised$posterior_means, scalar$posterior_means,
               tolerance = 1e-6)
  expect_equal(vectorised$fit_success, scalar$fit_success)

  # uniroot(tol = 0.001) for the median, and HDInterval::inverseCDF over a
  # numerically integrated CDF for the interval, are both looser than the
  # bisection used here.
  expect_equal(vectorised$posterior_medians, scalar$posterior_medians,
               tolerance = 2e-3)
  expect_equal(vectorised$credible_intervals, scalar$credible_intervals,
               tolerance = 2e-3)
  expect_equal(vectorised$posterior_parameters$power_parameter_mean,
               scalar$posterior_parameters$power_parameter_mean,
               tolerance = 1e-6)
  expect_equal(vectorised$posterior_parameters$power_parameter_std,
               scalar$posterior_parameters$power_parameter_std,
               tolerance = 1e-6)
})

test_that("the vectorised NPP effective sample sizes are deterministic", {
  fixture <- npp_test_model()
  target_data <- TargetDataFactory$new()$create(
    source_data = fixture$source_data,
    case_study_config = fixture$case_study_config,
    target_sample_size_per_arm = 46, control_drift = 0, treatment_drift = 0,
    summary_measure_likelihood = "normal", target_to_source_std_ratio = 1
  )

  run <- function(seed) {
    set.seed(7)
    samples <- target_data$generate(5)
    set.seed(seed)
    fixture$model$vectorised_replicate_inference(
      target_data = target_data, samples = samples,
      to_return = c("ess_moment", "ess_precision", "ess_elir"),
      critical_value = 0.975, theta_0 = 0, confidence_level = 0.95,
      null_space = "left"
    )
  }

  # The scalar path fits a mixture to random draws, so its effective sample
  # sizes move by around 20 per cent between runs on identical data. Computing
  # them from the exact mixture removes that noise entirely.
  first <- run(1)
  second <- run(999)

  expect_equal(first$ess_moments, second$ess_moments)
  expect_equal(first$ess_precisions, second$ess_precisions)
  expect_equal(first$ess_elir, second$ess_elir)
  expect_false(any(is.na(first$ess_elir)))
})

test_that("npp_prior_mixture integrates the Beta prior to one", {
  for (std in c(0.1, 0.2, 0.4)) {
    fixture <- npp_test_model(std)
    mixture <- npp_prior_mixture(fixture$model)

    expect_equal(sum(mixture$weights), 1)
    expect_true(all(mixture$power_parameter > 0))
    expect_true(all(mixture$power_parameter < 1))
  }
})

test_that("npp_prior_mixture recovers the Beta prior mean of the power parameter", {
  for (std in c(0.1, 0.2, 0.4)) {
    fixture <- npp_test_model(std)
    mixture <- npp_prior_mixture(fixture$model)

    # Weights are the Beta density integrated by the quadrature rule, so their
    # first two moments must match the Beta distribution they discretise.
    expect_equal(sum(mixture$weights * mixture$power_parameter), 0.5)
    expect_equal(
      sqrt(sum(mixture$weights * mixture$power_parameter^2) - 0.5^2),
      std
    )
  }
})

test_that("the quadrature mixture reproduces the posterior moments of the nested integration", {
  for (std in c(0.1, 0.2, 0.4)) {
    fixture <- npp_test_model(std)
    model <- fixture$model
    target_data <- TargetDataFactory$new()$create(
      source_data = fixture$source_data,
      case_study_config = fixture$case_study_config,
      target_sample_size_per_arm = 46, control_drift = 0, treatment_drift = 0,
      summary_measure_likelihood = "normal", target_to_source_std_ratio = 1
    )

    set.seed(4)
    samples <- target_data$generate(3)

    prior <- npp_prior_mixture(model)
    posterior <- normal_mixture_posterior(
      weights = prior$weights, means = prior$means, sds = prior$sds,
      estimate = samples$treatment_effect_estimate,
      standard_error = samples$treatment_effect_standard_error
    )
    summaries <- normal_mixture_summary(posterior$weights, posterior$means,
                                        posterior$sds)

    for (r in seq_len(nrow(samples))) {
      target_data$sample <- samples[r, , drop = FALSE]
      model$inference(target_data)

      expect_equal(summaries$mean[r], model$post_mean, tolerance = 1e-6)
      expect_equal(summaries$sd[r]^2, model$post_var, tolerance = 1e-6)

      # inference() locates the median with uniroot(tol = 0.001), so it is the
      # looser of the two; the bisection here converges to 1e-12.
      expect_equal(summaries$quantiles[r, 2], model$post_median,
                   tolerance = 2e-3)
    }
  }
})
