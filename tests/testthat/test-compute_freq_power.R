target_data <- list(
  sample_size_per_arm = 30,
  treatment_effect = 1,
  standard_deviation = 1,
  summary_measure_likelihood = "normal",
  endpoint = "continuous"
)

compute_test_power <- function(frequentist_test, null_space) {
  compute_freq_power(
    alpha = 0.05,
    target_data = target_data,
    frequentist_test = frequentist_test,
    theta_0 = 0,
    null_space = null_space,
    simulation_config = list()
  )
}

test_that("compute_freq_power works for t-test with left null space", {
  result <- compute_test_power("t-test", "left")
  expected_power <- pwr::pwr.t.test(
    d = 1,
    n = 30,
    sig.level = 0.05,
    type = "one.sample",
    alternative = "greater"
  )$power

  expect_equal(result$power, expected_power, tolerance = 1e-6)
  expect_equal(result$conf_int_power, rep(expected_power, 2))
})

test_that("compute_freq_power works for t-test with right null space", {
  result <- compute_test_power("t-test", "right")
  expected_power <- pwr::pwr.t.test(
    d = 1,
    n = 30,
    sig.level = 0.05,
    type = "one.sample",
    alternative = "less"
  )$power

  expect_equal(result$power, expected_power, tolerance = 1e-6)
  expect_equal(result$conf_int_power, rep(expected_power, 2))
})

test_that("compute_freq_power works for z-test with left null space", {
  result <- compute_test_power("z-test", "left")
  expected_power <- pwr::pwr.norm.test(
    d = 1,
    n = 30,
    sig.level = 0.05,
    alternative = "greater"
  )$power

  expect_equal(result$power, expected_power, tolerance = 1e-6)
  expect_equal(result$conf_int_power, rep(expected_power, 2))
})

test_that("compute_freq_power works for z-test with right null space", {
  result <- compute_test_power("z-test", "right")
  expected_power <- pwr::pwr.norm.test(
    d = 1,
    n = 30,
    sig.level = 0.05,
    alternative = "less"
  )$power

  expect_equal(result$power, expected_power, tolerance = 1e-6)
  expect_equal(result$conf_int_power, rep(expected_power, 2))
})

test_that("compute_freq_power rejects an invalid null space", {
  expect_error(
    compute_test_power("t-test", "middle"),
    "Null space must be either 'left' or 'right'"
  )
})

test_that("compute_freq_power preserves its return structure for missing alpha", {
  result <- compute_freq_power(
    alpha = NA_real_,
    target_data = target_data,
    frequentist_test = "t-test",
    theta_0 = 0,
    null_space = "left",
    simulation_config = list()
  )

  expect_named(result, c("power", "conf_int_power"))
  expect_true(is.na(result$power))
  expect_equal(result$conf_int_power, rep(NA_real_, 2))
})
