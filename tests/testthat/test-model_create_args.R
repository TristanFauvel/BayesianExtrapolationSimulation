valid_case_study_config <- function() {
  list(
    null_space = "left",
    summary_measure_likelihood = "normal",
    theta_0 = 0
  )
}


test_that("Model$create rejects a case study config that is not a list", {
  model <- Model$new()

  expect_error(
    model$create("botox", "conjugate", list(), list()),
    "case_study_config"
  )
})


test_that("Model$create rejects a case study config missing null_space", {
  model <- Model$new()
  config <- valid_case_study_config()
  config$null_space <- NULL

  expect_error(
    model$create(config, "conjugate", list(), list()),
    "null_space"
  )
})


test_that("Model$create rejects a case study config missing summary_measure_likelihood", {
  model <- Model$new()
  config <- valid_case_study_config()
  config$summary_measure_likelihood <- NULL

  expect_error(
    model$create(config, "conjugate", list(), list()),
    "summary_measure_likelihood"
  )
})


test_that("Model$create rejects a method that is not a single string", {
  model <- Model$new()

  expect_error(
    model$create(valid_case_study_config(), c("a", "b"), list(), list()),
    "method"
  )
})


test_that("Model$create rejects method parameters that are not a list", {
  model <- Model$new()

  expect_error(
    model$create(valid_case_study_config(), "conjugate", "noninformative", list()),
    "method_parameters"
  )
})


test_that("Model$create rejects an mcmc config that is neither NULL nor a list", {
  model <- Model$new()

  expect_error(
    model$create(valid_case_study_config(), "conjugate", list(), list(), mcmc_config = 4),
    "mcmc_config"
  )
})
