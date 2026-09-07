# The vectorised kernel must reproduce the scalar kernel replicate by
# replicate. That comparison is exact rather than distributional because no
# random number is drawn inside the replicate loop for these methods: the data
# are generated up front in a single call, and every remaining step is
# deterministic given that data.

# Generators are looked up by method rather than by class name, because
# Gaussian_Gravestock_EBPP declares classname " Gaussian_gravestock_EBPP",
# which matches neither the object it is bound to nor its own class.
generator_for <- function(method) {
  switch(
    method,
    separate = SeparateGaussian_RBesT,
    pooling = PoolGaussian_RBesT,
    conditional_power_prior = StaticBorrowingGaussian,
    RMP = GaussianRMP_RBesT,
    EB_PP = Gaussian_Gravestock_EBPP,
    p_value_based_PP = p_value_based_PP_Gaussian,
    test_then_pool_equivalence = TestThenPoolEquivalence,
    test_then_pool_difference = TestThenPoolDifference,
    stop("unsupported method in fixture")
  )
}

scalar_only <- function(method, model) {
  # A model that refuses the vectorised path, so the same configuration can be
  # run through the original replicate loop for comparison.
  scalar_class <- R6::R6Class(
    "ScalarOnlyModel",
    inherit = generator_for(method),
    public = list(
      vectorised_replicate_inference = function(...) NULL
    )
  )

  arguments <- list(prior = model$prior)
  if (!is.null(model$null_space)) {
    # The empirical Bayes power priors take the hypothesis space as well.
    arguments$null_space <- model$null_space
    arguments$theta_0 <- model$parameters$theta_0
  }

  reference <- do.call(scalar_class$new, arguments)
  reference$prior <- model$prior
  reference
}

test_case_study_config <- function(null_space = "left") {
  list(
    name = "unit_test",
    endpoint = "continuous",
    summary_measure_likelihood = "normal",
    sampling_approximation = TRUE,
    theta_0 = 0,
    null_space = null_space,
    source = list(
      control = 100,
      treatment = 100,
      treatment_effect = 0.5,
      standard_error = 0.12
    )
  )
}

build_target_data <- function(case_study_config, source_data) {
  TargetDataFactory$new()$create(
    source_data = source_data,
    case_study_config = case_study_config,
    target_sample_size_per_arm = 60,
    control_drift = 0,
    treatment_drift = 0,
    summary_measure_likelihood = "normal",
    target_to_source_std_ratio = 1
  )
}

method_parameters_for <- function(method) {
  base <- list(initial_prior = list("noninformative"))
  switch(
    method,
    separate = base,
    pooling = base,
    conditional_power_prior = c(base, list(power_parameter = list(0.5))),
    RMP = c(base, list(prior_weight = list(0.5), empirical_bayes = list(TRUE))),
    EB_PP = base,
    p_value_based_PP = c(base, list(shape_parameter = list(1),
                                    equivalence_margin = list(0.1))),
    test_then_pool_equivalence = c(base, list(significance_level = list(0.05),
                                              equivalence_margin = list(0.1))),
    test_then_pool_difference = c(base, list(significance_level = list(0.05))),
    stop("unsupported method in fixture")
  )
}

model_for <- function(method, case_study_config, source_data) {
  Model$new()$create(
    case_study_config = case_study_config,
    method = method,
    method_parameters = method_parameters_for(method),
    source_data = source_data
  )
}

run_kernel <- function(model, target_data, seed, to_return) {
  set.seed(seed)
  model$simulation_for_given_treatment_effect(
    target_data = target_data,
    n_replicates = 40,
    critical_value = 0.975,
    theta_0 = 0,
    confidence_level = 0.95,
    null_space = "left",
    case_study = "unit_test",
    method = "unit_test",
    to_return = to_return,
    n_samples_quantiles_estimation = 100
  )
}

vectorised_methods <- c("separate", "pooling", "conditional_power_prior", "RMP",
                        "EB_PP", "p_value_based_PP",
                        "test_then_pool_equivalence", "test_then_pool_difference")

for (method in vectorised_methods) {
  test_that(paste0("vectorised kernel reproduces the scalar kernel for ", method), {
    case_study_config <- test_case_study_config()
    source_data <- SourceData$new(case_study_config)
    target_data <- build_target_data(case_study_config, source_data)

    model <- model_for(method, case_study_config, source_data)
    reference <- scalar_only(method, model)

    to_return <- c("test_decision", "posterior_mean", "posterior_median",
                   "credible_interval", "posterior_parameters",
                   "ess_moment", "ess_precision", "ess_elir", "fit_success")

    # Without this the comparison below would silently run the scalar path
    # twice and pass for the wrong reason.
    set.seed(1)
    expect_false(is.null(model$vectorised_replicate_inference(
      target_data = target_data,
      samples = target_data$generate(5),
      to_return = to_return,
      critical_value = 0.975,
      theta_0 = 0,
      confidence_level = 0.95,
      null_space = "left"
    )))
    expect_null(reference$vectorised_replicate_inference())

    vectorised <- run_kernel(model, target_data, seed = 7, to_return = to_return)
    scalar <- run_kernel(reference, target_data, seed = 7, to_return = to_return)

    expect_equal(vectorised$test_decisions, scalar$test_decisions)
    expect_equal(vectorised$posterior_means, scalar$posterior_means)
    expect_equal(vectorised$fit_success, scalar$fit_success)
    expect_equal(vectorised$ess_moments, scalar$ess_moments, tolerance = 1e-6)
    expect_equal(vectorised$ess_elir, scalar$ess_elir, tolerance = 1e-6)

    # ess_precision is read off the credible interval half-width and is
    # proportional to its inverse square, so it inherits RBesT's uniroot
    # tolerance amplified by a factor of two. Single-component methods use
    # qnorm on both paths and match exactly; only the RMP mixture drifts.
    expect_equal(vectorised$ess_precisions, scalar$ess_precisions,
                 tolerance = 1e-3)
    expect_equal(vectorised$posterior_parameters, scalar$posterior_parameters)

    # Mixture quantiles are resolved by bisection here and by uniroot at a
    # looser tolerance in RBesT, so these agree to RBesT's accuracy, not ours.
    expect_equal(vectorised$posterior_medians, scalar$posterior_medians,
                 tolerance = 1e-4)
    expect_equal(vectorised$credible_intervals, scalar$credible_intervals,
                 tolerance = 1e-4)
  })
}

test_that("vectorised kernel honours to_return and skips unrequested outputs", {
  case_study_config <- test_case_study_config()
  source_data <- SourceData$new(case_study_config)
  target_data <- build_target_data(case_study_config, source_data)

  model <- Model$new()$create(
    case_study_config = case_study_config,
    method = "pooling",
    method_parameters = method_parameters_for("pooling"),
    source_data = source_data
  )

  result <- run_kernel(model, target_data, seed = 3, to_return = "test_decision")

  expect_length(result$test_decisions, 40)
  expect_null(result$posterior_means)
  expect_null(result$posterior_medians)
  expect_null(result$credible_intervals)
  expect_null(result$ess_elir)
  expect_null(result$fit_success)
})
