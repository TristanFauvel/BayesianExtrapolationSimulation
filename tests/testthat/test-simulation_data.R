# Test standard_error_log_odds_ratio function
test_that("standard_error_log_odds_ratio calculates the standard error correctly", {
  se <- standard_error_log_odds_ratio(10, 20, 30, 40)
  expect_equal(se, 0.449, tolerance = 1e-3)
})

# Test generate_binary_data_from_rate function
test_that("generate_binary_data_from_rate generates binary data correctly", {
  data <- generate_binary_data_from_rate(0.3, 100)
  expect_equal(length(data), 100)
  expect_equal(sum(data), 30)

  data_with_fractional_expected_count <- generate_binary_data_from_rate(0.25, 10)
  expect_equal(length(data_with_fractional_expected_count), 10)
  expect_equal(sum(data_with_fractional_expected_count), round(0.25 * 10))
})

# Test rate_from_drift_logOR function
test_that("rate_from_drift_logOR calculates the success rate correctly", {
  rate <- rate_from_drift_logOR(0.5, 0.2)
  expect_equal(rate, 0.292, tolerance = 1e-3)
})

# Test rate_from_drift_logRR function
test_that("rate_from_drift_logRR calculates the rate correctly", {
  rate <- rate_from_drift_logRR(0.5, 0.2)
  expect_equal(rate, 0.33, tolerance = 1e-3)
})

# Test compute_ORs function
test_that("compute_ORs computes odds ratios correctly", {
  result <- compute_ORs(100, 200, 30, 40)
  expect_equal(result$log_odds_ratio, -0.539, tolerance = 1e-3)
  expect_equal(result$treatment_rate, 0.2)
  expect_equal(result$control_rate, 0.3)
  expect_equal(result$std_err_log_odds_ratio, 0.279, tolerance = 1e-3)
})

# Test compute_log_odds_ratio_from_rates function
test_that("compute_log_odds_ratio_from_rates computes log odds ratio correctly", {
  log_or <- compute_log_odds_ratio_from_rates(0.2, 0.3)
  expect_equal(log_or, -0.539, tolerance = 1e-3)
})

# Test compute_log_odds_ratio_from_counts function
test_that("compute_log_odds_ratio_from_counts computes log odds ratio correctly", {
  log_or <- compute_log_odds_ratio_from_counts(30, 40, 70, 160, TRUE)
  expect_equal(log_or, -0.539, tolerance = 1e-3)
})

# Test sample_log_odds_ratios function
test_that("sample_log_odds_ratios samples log odds ratios correctly", {
  result <- sample_log_odds_ratios(100, 200, 0.3, 0.2, 10)
  expect_equal(length(result$log_odds_ratio), 10)
  expect_equal(length(result$treatment_rate), 10)
  expect_equal(length(result$control_rate), 10)
  expect_equal(length(result$std_err_log_odds_ratio), 10)
})

# Test sample_rate_ratios function
test_that("sample_rate_ratios samples rate ratios correctly", {
  set.seed(123)
  result <- sample_rate_ratios(0.3, 0.2, 10, 100, 200)

  set.seed(123)
  expected <- sample_aggregate_binary_data(0.2, 200, 10) /
    sample_aggregate_binary_data(0.3, 100, 10)

  expect_equal(length(result), 10)
  expect_equal(result, expected)
})

# Test sample_aggregate_normal_data function
test_that("sample_aggregate_normal_data samples aggregate normal data correctly", {
  result <- sample_aggregate_normal_data(10, 5, 10, 100)
  expect_equal(length(result$treatment_effect_estimate), 10)
  expect_equal(length(result$treatment_effect_standard_error), 10)
})

# Test sample_aggregate_binary_data function
test_that("sample_aggregate_binary_data samples aggregate binary data correctly", {
  result <- sample_aggregate_binary_data(0.3, 100, 10)
  expect_equal(length(result), 10)
})

test_that("SourceData preserves the observed binary control rate by default", {
  case_study_config <- list(
    endpoint = "binary",
    summary_measure_likelihood = "normal",
    source = list(
      control = 100,
      treatment = 100,
      responses = list(control = 20, treatment = 30)
    )
  )

  source_data <- SourceData$new(case_study_config)

  expect_equal(source_data$control_rate, 0.2)
})

test_that("ObservedSourceData reads recurrent-event rates from source config", {
  case_study_config <- list(
    endpoint = "recurrent_event",
    summary_measure_likelihood = "normal",
    source = list(
      control = 100,
      treatment = 120,
      treatment_effect = -0.2,
      standard_error = 0.1,
      control_rate = 0.8,
      treatment_rate = 0.65
    ),
    source_metadata = list(description = "prevents partial matching")
  )

  source_data <- ObservedSourceData$new(case_study_config)

  expect_equal(source_data$control_rate, 0.8)
  expect_equal(source_data$treatment_rate, 0.65)
})

test_that("recurrent-event size parameter is used consistently", {
  case_study_config <- list(
    endpoint = "recurrent_event",
    summary_measure_likelihood = "normal",
    sampling_approximation = TRUE,
    source = list(
      control = 100,
      treatment = 120,
      treatment_effect = log(0.5),
      standard_error = 0.1,
      control_rate = 2,
      treatment_rate = 1
    )
  )
  source_data <- SourceData$new(case_study_config)

  source_variance_treatment <- 1 + 1 ^ 2 / 0.8
  source_variance_control <- 2 + 2 ^ 2 / 0.8
  expected_source_se <- sqrt(
    source_variance_treatment / (120 * 1 ^ 2) +
      source_variance_control / (100 * 2 ^ 2)
  )
  expect_equal(source_data$standard_error, expected_source_se)

  target_data <- RecurrentEventTargetData$new(
    source_data = source_data,
    sampling_approximation = TRUE,
    target_sample_size_per_arm = 50,
    control_drift = 0,
    treatment_drift = 0,
    summary_measure_likelihood = "normal",
    k_treatment = 0.4,
    k_control = 0.6
  )
  expected_target_sd <- sqrt(
    (1 + 1 ^ 2 / 0.4) / 1 ^ 2 +
      (2 + 2 ^ 2 / 0.6) / 2 ^ 2
  )

  expect_equal(target_data$k_treatment, 0.4)
  expect_equal(target_data$k_control, 0.6)
  expect_equal(target_data$standard_deviation, expected_target_sd)

  set.seed(5821)
  expected_samples <- stats::rnbinom(n = 20, size = 0.4, mu = 1)
  set.seed(5821)
  observed_samples <- RBExT:::sample_negative_binomial(n = 20, mu = 1, k = 0.4)
  expect_identical(observed_samples, expected_samples)
})

test_that("recurrent-event size parameters must be positive", {
  case_study_config <- list(
    endpoint = "recurrent_event",
    summary_measure_likelihood = "normal",
    sampling_approximation = TRUE,
    source = list(
      control = 100,
      treatment = 100,
      treatment_effect = 0,
      standard_error = 0.1,
      control_rate = 1,
      treatment_rate = 1
    )
  )
  source_data <- SourceData$new(case_study_config)

  expect_error(
    RecurrentEventTargetData$new(
      source_data = source_data,
      sampling_approximation = TRUE,
      target_sample_size_per_arm = 50,
      treatment_drift = 0,
      summary_measure_likelihood = "normal",
      k_treatment = 0
    ),
    "size parameters must be positive"
  )
})

# Test sample_exponential_arm_statistics function
test_that("sample_exponential_arm_statistics returns coherent sufficient statistics", {
  n_subjects <- 40
  max_follow_up_time <- 2
  result <- RBExT:::sample_exponential_arm_statistics(
    n_subjects = n_subjects,
    rate = 0.534,
    max_follow_up_time = max_follow_up_time,
    n_replicates = 500
  )

  expect_length(result$n_events, 500)
  expect_length(result$exposure, 500)
  expect_true(all(result$n_events >= 0 & result$n_events <= n_subjects))
  expect_true(all(result$exposure > 0))
  expect_true(all(result$exposure <= n_subjects * max_follow_up_time))
})

test_that("sample_exponential_arm_statistics censors every patient when no event can occur", {
  n_subjects <- 5
  max_follow_up_time <- 2
  result <- RBExT:::sample_exponential_arm_statistics(
    n_subjects = n_subjects,
    rate = 1e-12,
    max_follow_up_time = max_follow_up_time,
    n_replicates = 4
  )

  expect_equal(result$n_events, rep(0L, 4))
  expect_equal(result$exposure, rep(n_subjects * max_follow_up_time, 4))
})

test_that("sample_exponential_arm_statistics matches patient-level simulation", {
  set.seed(4242)
  n_subjects <- 57
  rate <- 0.534
  max_follow_up_time <- 2
  n_replicates <- 20000

  # Simulate the individual survival times the closed form replaces
  patient_level <- replicate(n_replicates, {
    times <- stats::rexp(n_subjects, rate = rate)
    c(sum(times <= max_follow_up_time), sum(pmin(times, max_follow_up_time)))
  })

  result <- RBExT:::sample_exponential_arm_statistics(
    n_subjects = n_subjects,
    rate = rate,
    max_follow_up_time = max_follow_up_time,
    n_replicates = n_replicates
  )

  # Both are unbiased for the same quantities
  expected_events <- n_subjects * stats::pexp(max_follow_up_time, rate = rate)
  expected_exposure <- n_subjects * (1 - exp(-rate * max_follow_up_time)) / rate
  expect_equal(mean(result$n_events), expected_events, tolerance = 0.01)
  expect_equal(mean(result$exposure), expected_exposure, tolerance = 0.01)

  # ... and have the same sampling distribution as the patient-level version
  expect_gt(suppressWarnings(
    stats::ks.test(patient_level[1, ], result$n_events)$p.value
  ), 0.001)
  expect_gt(suppressWarnings(
    stats::ks.test(patient_level[2, ], result$exposure)$p.value
  ), 0.001)
})

# Test negative_binomial_regression / negative_binomial_inverse_dispersion
test_that("negative_binomial_regression estimates the rate by the sample mean", {
  set.seed(515)
  counts <- stats::rnbinom(60, size = 0.8, mu = 1.74)
  result <- negative_binomial_regression(counts)

  # The intercept-only maximum likelihood estimate of the mean is exactly the
  # sample mean, whatever the dispersion
  expect_identical(result$rate_estimate, mean(counts))
  expect_equal(result$se_rate, result$se_log_rate * result$rate_estimate)
  expect_true(is.finite(result$se_log_rate) && result$se_log_rate > 0)
})

test_that("negative_binomial_inverse_dispersion falls back to the Poisson limit", {
  # Underdispersed data: the profile likelihood is maximised at 1 / theta = 0
  counts <- c(2, 2, 2, 3, 2, 2, 3, 2, 2, 2)
  expect_true(sum((counts - mean(counts))^2) <= length(counts) * mean(counts))
  expect_equal(
    RBExT:::negative_binomial_inverse_dispersion(counts, mean(counts)),
    0
  )

  # ... so the standard error reduces to the Poisson one
  result <- negative_binomial_regression(counts)
  expect_equal(
    result$se_log_rate,
    sqrt(1 / (length(counts) * mean(counts)))
  )
})

test_that("negative_binomial_inverse_dispersion maximises the profile likelihood", {
  profile_loglikelihood <- function(counts, theta) {
    n_observations <- length(counts)
    rate <- mean(counts)
    sum(lgamma(counts + theta) - lgamma(theta)) +
      n_observations * theta * log(theta / (theta + rate)) +
      sum(counts) * log(rate / (rate + theta))
  }

  set.seed(616)
  for (n_observations in c(9, 25, 100)) {
    counts <- stats::rnbinom(n_observations, size = 0.8, mu = 1.74)
    inverse_dispersion <- RBExT:::negative_binomial_inverse_dispersion(
      counts, mean(counts)
    )
    if (inverse_dispersion == 0) next

    theta <- 1 / inverse_dispersion
    best <- profile_loglikelihood(counts, theta)

    # No neighbouring theta, and not the Poisson boundary, does better
    neighbours <- theta * c(0.5, 0.9, 0.99, 1.01, 1.1, 2)
    expect_true(all(vapply(
      neighbours,
      function(candidate) profile_loglikelihood(counts, candidate),
      numeric(1)
    ) <= best + 1e-8))
    expect_gt(best, profile_loglikelihood(counts, 1e10))
  }
})

test_that("negative_binomial_regression handles all-zero data", {
  expect_warning(
    result <- negative_binomial_regression(rep(0, 20)),
    "All values are zero"
  )
  expect_true(is.finite(result$rate_estimate))
  expect_true(is.finite(result$se_log_rate))
})
