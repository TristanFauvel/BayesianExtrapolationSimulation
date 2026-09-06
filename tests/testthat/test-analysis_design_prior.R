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

test_that("UnitInformationDesignPrior keeps binomial mixtures on the effect scale", {
  design_prior <- create_unit_information_prior()
  design_prior$summary_measure_likelihood <- "binomial"
  design_prior$RBesT_model <- RBesT::mixbeta(c(1, 2, 5))

  set.seed(123)
  samples <- design_prior$sample(10000)

  expect_true(all(samples >= -1 & samples <= 1))
  expect_equal(mean(samples), 2 * (2 / 7) - 1, tolerance = 0.02)
  expect_equal(design_prior$cdf(c(-1, 0, 1)), c(0, stats::pbeta(0.5, 2, 5), 1))
  expect_equal(design_prior$pdf(c(-1, 0, 1)), c(0, stats::dbeta(0.5, 2, 5) / 2, 0))
})

test_that("SourcePosteriorDesignPrior samples its binomial risk difference", {
  binomial_source_data <- list(
    summary_measure_likelihood = "binomial",
    standard_error = 0.2,
    treatment_effect_estimate = 0.2,
    control_rate = 0.2,
    treatment_rate = 0.4,
    sample_size_control = 10,
    sample_size_treatment = 10
  )
  design_prior <- SourcePosteriorDesignPrior$new(
    source_data = binomial_source_data,
    case_study_config = list(),
    simulation_config = list()
  )

  expect_equal(design_prior$n_successes_control, 2L)
  expect_equal(design_prior$n_successes_treatment, 4L)

  set.seed(123)
  samples <- design_prior$sample(20000)
  expected_mean <- (1 + 4) / (2 + 10) - (1 + 2) / (2 + 10)

  expect_length(samples, 20000)
  expect_true(all(samples >= -1 & samples <= 1))
  expect_equal(mean(samples), expected_mean, tolerance = 0.02)
  expect_equal(design_prior$cdf(1), 1, tolerance = 1e-6)
})
