test_that("simulation kernel only computes requested outputs", {
  calls <- new.env(parent = emptyenv())
  calls$median <- 0L
  calls$credible_interval <- 0L

  test_model_class <- R6::R6Class(
    "OutputSelectionTestModel",
    inherit = Model,
    public = list(
      inference = function(target_data) {
        self$post_mean <- target_data$sample$treatment_effect_estimate
        self$post_var <- 1
        self$post_median <- NULL
        "Success"
      },
      posterior_median = function(...) {
        calls$median <- calls$median + 1L
        self$post_mean
      },
      credible_interval = function(...) {
        calls$credible_interval <- calls$credible_interval + 1L
        c(self$post_mean - 1, self$post_mean + 1)
      },
      test_decision = function(...) {
        self$post_mean > 0
      }
    )
  )

  target_data <- list(
    sample = NULL,
    generate = function(n_replicates) {
      data.frame(treatment_effect_estimate = seq_len(n_replicates))
    }
  )

  result <- test_model_class$new()$simulation_for_given_treatment_effect(
    target_data = target_data,
    n_replicates = 3,
    critical_value = 0.975,
    theta_0 = 0,
    confidence_level = 0.95,
    null_space = "left",
    case_study = "unit_test",
    method = "separate",
    to_return = "test_decision",
    n_samples_quantiles_estimation = 100
  )

  expect_equal(result$test_decisions, rep(1, 3))
  expect_null(result$posterior_means)
  expect_null(result$posterior_medians)
  expect_null(result$credible_intervals)
  expect_null(result$posterior_parameters)
  expect_null(result$fit_success)
  expect_equal(calls$median, 0L)
  expect_equal(calls$credible_interval, 0L)
})
