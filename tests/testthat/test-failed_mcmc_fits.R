# A fit that fails an MCMC diagnostic must not be treated the same as one that
# passes: the replicate loop should give it a chance to recover by retrying
# (as it already does for low ESS), and whatever still hasn't recovered once
# retries are exhausted must not be allowed to bias the reported operating
# characteristics.

diagnostics_retry_mcmc_config <- function() {
  list(
    chain_length = 100L,
    max_chain_length = 10000L,
    num_chains = 4L,
    target_ess = 10L,
    rhat_threshold = 1.1,
    max_divergence_rate = 0.01
  )
}

# `inference` is stubbed so the test controls the diagnostics directly,
# independently of whatever string it returns - the replicate loop is meant to
# make its retry decision from `self$rhat` / `self$mcmc_ess` /
# `self$n_divergences`, the same fields `inference()` itself reports.
diagnostics_retry_test_model <- function(rhat_sequence = NULL,
                                         n_divergences_sequence = NULL,
                                         mcmc_ess_sequence = NULL,
                                         calls) {
  n_attempts <- max(
    length(rhat_sequence), length(n_divergences_sequence), length(mcmc_ess_sequence), 1
  )
  rhat_sequence <- rhat_sequence %||% rep(1.0, n_attempts)
  n_divergences_sequence <- n_divergences_sequence %||% rep(0, n_attempts)
  mcmc_ess_sequence <- mcmc_ess_sequence %||% rep(50, n_attempts)

  test_model_class <- R6::R6Class(
    "DiagnosticsRetryTestModel",
    inherit = Model,
    public = list(
      mcmc_config = diagnostics_retry_mcmc_config(),
      rhat = NULL,
      n_divergences = NULL,
      mcmc_ess = NULL,
      initialize = function() {
        self$mcmc <- TRUE
      },
      inference = function(target_data) {
        calls$n <- calls$n + 1L
        attempt <- min(calls$n, length(rhat_sequence))
        self$post_mean <- 1
        self$post_var <- 1
        self$post_median <- 1
        self$rhat <- rhat_sequence[attempt]
        self$n_divergences <- n_divergences_sequence[attempt]
        self$mcmc_ess <- mcmc_ess_sequence[attempt]
        if (self$rhat > self$mcmc_config$rhat_threshold) {
          return(paste0("Large rhat values: ", self$rhat))
        }
        divergence_rate <- self$n_divergences /
          (self$mcmc_config$num_chains * self$mcmc_config$chain_length)
        if (divergence_rate > self$mcmc_config$max_divergence_rate) {
          return("Too many divergent transitions")
        }
        if (self$mcmc_ess < self$mcmc_config$target_ess) {
          return("Target MCMC ESS not reached")
        }
        "Success"
      },
      test_decision = function(...) TRUE,
      credible_interval = function(...) c(0, 2)
    )
  )
  test_model_class$new()
}

`%||%` <- function(x, y) if (is.null(x)) y else x

run_single_replicate <- function(model) {
  target_data <- list(
    sample = NULL,
    generate = function(n_replicates) data.frame(x = seq_len(n_replicates))
  )

  model$simulation_for_given_treatment_effect(
    target_data = target_data,
    n_replicates = 1,
    critical_value = 0.975,
    theta_0 = 0,
    confidence_level = 0.95,
    null_space = "left",
    case_study = "unit_test",
    method = "separate",
    to_return = c("fit_success", "mcmc_diagnostics", "posterior_mean"),
    n_samples_quantiles_estimation = 100
  )
}


test_that("a large rhat is retried rather than only reported", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  model <- diagnostics_retry_test_model(
    rhat_sequence = c(1.5, 1.0),
    calls = calls
  )

  result <- run_single_replicate(model)

  expect_equal(calls$n, 2L)
  expect_equal(result$fit_success, "Success")
})


test_that("an excessive divergence rate is retried rather than only reported", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  model <- diagnostics_retry_test_model(
    n_divergences_sequence = c(50, 0),
    calls = calls
  )

  result <- run_single_replicate(model)

  expect_equal(calls$n, 2L)
  expect_equal(result$fit_success, "Success")
})


test_that("retrying a bad rhat stops once the maximum chain length is reached", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  config <- diagnostics_retry_mcmc_config()
  config$max_chain_length <- config$chain_length
  model <- diagnostics_retry_test_model(
    rhat_sequence = rep(1.5, 10),
    calls = calls
  )
  model$mcmc_config <- config

  result <- run_single_replicate(model)

  expect_equal(calls$n, 1L)
  expect_match(result$fit_success, "rhat")
})


# The replicate loop already tries to rescue a failing fit by retrying (tests
# above); a fit that still hasn't recovered once retries are exhausted must
# not be allowed to bias the reported operating characteristics computed from
# `estimate_frequentist_operating_characteristics`.
canned_replicate_results_model <- function(results) {
  test_model_class <- R6::R6Class(
    "CannedReplicateResultsTestModel",
    inherit = Model,
    public = list(
      simulation_for_given_treatment_effect = function(...) results
    )
  )
  test_model_class$new()
}

four_replicates_one_failed <- list(
  fit_success = c("Success", "Success", "Success", "Large rhat values: 1.5"),
  test_decisions = c(1, 1, 0, 1),
  posterior_means = c(1, 2, 3, 1000),
  posterior_medians = c(1, 2, 3, 1000),
  credible_intervals = matrix(
    c(0, 1, 1, 3, 2, 4, -500, 1500),
    ncol = 2, byrow = TRUE
  ),
  posterior_parameters = NULL,
  ess_moments = c(10, 20, 30, 9999),
  ess_precisions = c(5, 10, 15, 9999),
  ess_elir = c(1, 2, 3, 4),
  rhat = c(1.00, 1.01, 1.02, 1.5),
  mcmc_ess = c(500, 600, 700, 3),
  n_divergences = c(0, 1, 2, 500)
)

run_ocs_with_canned_results <- function(results) {
  canned_replicate_results_model(results)$estimate_frequentist_operating_characteristics(
    theta_0 = 0,
    target_data = list(treatment_effect = 2),
    n_replicates = length(results$fit_success),
    critical_value = 0.975,
    confidence_level = 0.95,
    null_space = "left",
    n_samples_quantiles_estimation = 100,
    case_study = "unit_test",
    method = "separate"
  )
}


test_that("a failed fit's posterior mean and median are excluded from the aggregates", {
  result <- run_ocs_with_canned_results(four_replicates_one_failed)

  expect_equal(result$posterior_mean, mean(c(1, 2, 3)))
  expect_equal(result$posterior_median, mean(c(1, 2, 3)))
  expect_equal(result$bias, mean(c(1, 2, 3) - 2))
  expect_equal(result$mse, mean((c(1, 2, 3) - 2)^2))
})


test_that("a failed fit's decision and credible interval are excluded from the aggregates", {
  result <- run_ocs_with_canned_results(four_replicates_one_failed)

  expect_equal(result$success_proba, mean(c(1, 1, 0)))
  expect_equal(result$coverage, mean(c(FALSE, TRUE, TRUE)))
  expect_equal(result$credible_interval_lower, mean(c(0, 1, 2)))
  expect_equal(result$credible_interval_upper, mean(c(1, 3, 4)))
  expect_equal(result$precision, mean(c(0.5, 1, 1)))
})


test_that("a failed fit's ESS and convergence diagnostics are excluded from the aggregates", {
  result <- run_ocs_with_canned_results(four_replicates_one_failed)

  expect_equal(result$ess_moment, mean(c(10, 20, 30)))
  expect_equal(result$ess_precision, mean(c(5, 10, 15)))
  expect_equal(result$rhat, mean(c(1.00, 1.01, 1.02)))
  expect_equal(result$mcmc_ess, mean(c(500, 600, 700)))
  expect_equal(result$n_divergences, mean(c(0, 1, 2)))
})


test_that("the ELIR ESS is unaffected by target-fit failures, as it does not depend on them", {
  result <- run_ocs_with_canned_results(four_replicates_one_failed)

  expect_equal(result$ess_elir, mean(c(1, 2, 3, 4)))
})


test_that("a failed fit is reported instead of silently absorbed", {
  result <- run_ocs_with_canned_results(four_replicates_one_failed)

  expect_match(result$warning, "1")
  expect_match(result$warning, "4")
  expect_match(result$warning, "Large rhat values: 1.5", fixed = TRUE)
})


test_that("operating characteristics are still reported unchanged when every fit succeeds", {
  all_success <- four_replicates_one_failed
  all_success$fit_success <- rep("Success", 4)

  result <- run_ocs_with_canned_results(all_success)

  expect_equal(result$posterior_mean, mean(c(1, 2, 3, 1000)))
  expect_true(is.na(result$warning))
})


test_that("operating characteristics are NA, not an error, when every fit fails", {
  all_failed <- four_replicates_one_failed
  all_failed$fit_success <- rep("Large rhat values: 1.5", 4)

  result <- run_ocs_with_canned_results(all_failed)

  expect_true(is.na(result$posterior_mean))
  expect_true(is.na(result$success_proba))
  expect_match(result$warning, "4")
})
