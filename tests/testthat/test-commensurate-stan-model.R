test_that("commensurate Stan model updates borrowing parameters with target data", {
  stan_code <- commensurate_stan_model()$stan_model_code

  expect_match(
    stan_code,
    "target_treatment_effect_estimate ~ normal\\s*\\(\\s*source_treatment_effect_estimate",
    perl = TRUE
  )
  expect_match(
    stan_code,
    "target_sampling_variance / NT\\s*\\+ 1 / tau\\s*\\+ prior_variance / \\(power_parameter \\* NS\\)",
    perl = TRUE
  )
})


test_that("commensurate sample sizes must be positive", {
  stan_code <- commensurate_stan_model()$stan_model_code

  expect_match(stan_code, "int<lower=1> NS", fixed = TRUE)
  expect_match(stan_code, "int<lower=1> NT", fixed = TRUE)
})
