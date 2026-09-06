source_data <- list(
  summary_measure_likelihood = "normal",
  standard_error = 0.2,
  equivalent_source_sample_size_per_arm = 25,
  treatment_effect_estimate = 0.5
)

create_unit_information_prior <- function() {
  UnitInformationDesignPrior$new(
    source_data = source_data,
    case_study_config = list(),
    case_study = "example",
    simulation_config = list(),
    mcmc_config = NULL
  )
}

test_that("DesignPrior creates a unit-information prior", {
  design_prior <- DesignPrior$new()$create(
    design_prior_type = "ui_design_prior",
    model = NULL,
    source_data = source_data,
    case_study_config = list(),
    simulation_config = list(),
    mcmc_config = NULL,
    case_study = "example"
  )

  expect_s3_class(design_prior, "UnitInformationDesignPrior")
  expect_equal(design_prior$design_prior_type, "ui_design_prior")
  expect_equal(design_prior$parameters$mean, 0.5)
  expect_equal(design_prior$parameters$sd, 1)
})

test_that("UnitInformationDesignPrior samples the requested number of values", {
  design_prior <- create_unit_information_prior()

  set.seed(123)
  samples <- design_prior$sample(100)

  expect_length(samples, 100)
  expect_true(is.numeric(samples))
})

test_that("UnitInformationDesignPrior evaluates its normal CDF", {
  design_prior <- create_unit_information_prior()

  expect_equal(design_prior$cdf(0.5), 0.5)
})
