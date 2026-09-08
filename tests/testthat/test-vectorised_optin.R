# The vectorised path assumes a prior it can describe up front. A subclass that
# re-derives its prior from each replicate must not inherit a fast path built
# for a fixed one, so ConjugateGaussian declines by default and only subclasses
# that genuinely have a fixed prior opt in.

test_that("ConjugateGaussian declines the vectorised path by default", {
  prior <- list(
    source = list(treatment_effect_estimate = 0.5, standard_error = 0.12),
    method_parameters = list(initial_prior = list("noninformative"))
  )
  model <- ConjugateGaussian$new(prior = prior)

  expect_null(model$vectorised_prior_variance(target_data = NULL, samples = NULL))
  expect_null(model$vectorised_replicate_inference(
    target_data = NULL, samples = NULL, to_return = "test_decision",
    critical_value = 0.975, theta_0 = 0, confidence_level = 0.95,
    null_space = "left"
  ))
})

test_that("StaticBorrowingGaussian opts in with its fixed prior variance", {
  prior <- list(
    source = list(treatment_effect_estimate = 0.5, standard_error = 0.12),
    method_parameters = list(initial_prior = list("noninformative"),
                             power_parameter = list(0.5))
  )
  model <- StaticBorrowingGaussian$new(prior = prior)

  expect_equal(
    model$vectorised_prior_variance(target_data = NULL, samples = NULL),
    0.12^2 / 0.5
  )
})

test_that("test-then-pool declines the vectorised path unless both branches are conjugate", {
  # TestThenPoolEquivalence and TestThenPoolDifference are shared between the
  # normal and binomial variants: the same class holds either a pair of
  # ConjugateGaussian models or a pair of MCMC ones. Only the former has the
  # prior variance the vectorised path needs.
  prior <- list(
    source = list(treatment_effect_estimate = 0.5, standard_error = 0.12,
                  summary_measure_likelihood = "normal",
                  equivalent_source_sample_size_per_arm = 100),
    method_parameters = list(initial_prior = list("noninformative"),
                             significance_level = list(0.05))
  )
  model <- TestThenPoolDifference$new(prior = prior)
  model$prior <- prior

  samples <- data.frame(treatment_effect_estimate = c(0.4, 0.6),
                        treatment_effect_standard_error = c(0.3, 0.2),
                        sample_size_per_arm = 50,
                        standard_deviation = c(2.1, 1.4))
  target_data <- list(sample_size_per_arm = 50, summary_measure_likelihood = "normal")

  arguments <- list(target_data = target_data, samples = samples,
                    to_return = "test_decision", critical_value = 0.975,
                    theta_0 = 0, confidence_level = 0.95, null_space = "left")

  # Conjugate branches: the fast path applies.
  expect_false(is.null(do.call(model$vectorised_replicate_inference, arguments)))

  # Swap in a branch that is not a conjugate Gaussian, as the binomial variant
  # has, and the fast path must decline rather than read a missing prior_var.
  model$pooling <- structure(list(), class = c("BinomialPooling", "MCMCModel"))
  expect_null(do.call(model$vectorised_replicate_inference, arguments))
})
