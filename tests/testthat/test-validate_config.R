test_that("validate_config accepts a config matching its schema", {
  schema <- list(seed = "count", critical_value = "probability")

  expect_silent(
    validate_config(list(seed = 42, critical_value = 0.975), schema, "test_config")
  )
})


test_that("validate_config reports a missing key and names the config", {
  schema <- list(seed = "count", critical_value = "probability")

  expect_error(
    validate_config(list(seed = 42), schema, "simulation_config.yml"),
    "simulation_config.yml.*critical_value"
  )
})


test_that("validate_config reports a probability outside the unit interval", {
  schema <- list(critical_value = "probability")

  expect_error(
    validate_config(list(critical_value = 1.5), schema, "test_config"),
    "critical_value"
  )
})


test_that("validate_config reports a count that is not a positive whole number", {
  schema <- list(n_replicates = "count")

  expect_error(
    validate_config(list(n_replicates = 0), schema, "test_config"),
    "n_replicates"
  )
  expect_error(
    validate_config(list(n_replicates = 10.5), schema, "test_config"),
    "n_replicates"
  )
})


test_that("validate_config reports a flag that is not a single logical", {
  schema <- list(delete_old_results = "flag")

  expect_error(
    validate_config(list(delete_old_results = "TRUE"), schema, "test_config"),
    "delete_old_results"
  )
})


test_that("validate_config accepts YAML list-of-scalars as a numeric vector", {
  schema <- list(sample_size_factors = "numeric_vector")

  expect_silent(
    validate_config(list(sample_size_factors = list(1, 2, 4)), schema, "test_config")
  )
})


test_that("validate_config rejects a numeric vector containing a string", {
  schema <- list(sample_size_factors = "numeric_vector")

  expect_error(
    validate_config(list(sample_size_factors = list(1, "two")), schema, "test_config"),
    "sample_size_factors"
  )
})


test_that("validate_config reports every offending key at once", {
  schema <- list(seed = "count", critical_value = "probability")

  message <- tryCatch(
    validate_config(list(seed = -1, critical_value = 2), schema, "test_config"),
    error = function(e) conditionMessage(e)
  )

  expect_match(message, "seed")
  expect_match(message, "critical_value")
})


test_that("validate_config ignores keys the schema does not describe", {
  schema <- list(seed = "count")

  expect_silent(
    validate_config(list(seed = 42, undocumented_extra = "anything"), schema, "test_config")
  )
})


test_that("the shipped simulation and mcmc configs satisfy their schemas", {
  simulation_config <- yaml::read_yaml(
    system.file("conf/simulation_config.yml", package = "RBExT")
  )
  expect_silent(
    validate_config(simulation_config, simulation_config_schema, "simulation_config.yml")
  )
})


test_that("validate_config accepts a config omitting a key marked optional", {
  schema <- list(seed = "count", parallelization = "flag?")

  expect_silent(validate_config(list(seed = 42), schema, "test_config"))
})


test_that("validate_config still type checks an optional key that is present", {
  schema <- list(parallelization = "flag?")

  expect_error(
    validate_config(list(parallelization = "yes"), schema, "test_config"),
    "parallelization"
  )
})


test_that("validate_config accepts zero for a non-negative count", {
  schema <- list(ndrift = "nonnegative_count")

  expect_silent(validate_config(list(ndrift = 0), schema, "test_config"))
})


test_that("validate_config rejects a negative non-negative count", {
  schema <- list(ndrift = "nonnegative_count")

  expect_error(validate_config(list(ndrift = -1), schema, "test_config"), "ndrift")
})


test_that("every shipped config satisfies its schema", {
  conf_dir <- system.file("conf", package = "RBExT")

  scenarios <- list.files(conf_dir, pattern = "^scenarios_config\\.yml$",
                          recursive = TRUE, full.names = TRUE)
  expect_gt(length(scenarios), 0)
  for (file in scenarios) {
    expect_silent(
      validate_config(yaml::read_yaml(file), scenarios_config_schema, file)
    )
  }

  mcmc <- list.files(conf_dir, pattern = "^mcmc_config\\.yml$",
                     recursive = TRUE, full.names = TRUE)
  expect_gt(length(mcmc), 0)
  for (file in mcmc) {
    expect_silent(validate_config(yaml::read_yaml(file), mcmc_config_schema, file))
  }
})


test_that("read_config returns the parsed config when it is valid", {
  path <- system.file("conf/simulation_config.yml", package = "RBExT")

  config <- read_config(path, simulation_config_schema)

  expect_identical(config$seed, 42L)
  expect_identical(config$critical_value, 0.975)
})


test_that("read_config rejects an invalid config and names the file", {
  path <- file.path(tempdir(), "bad_simulation_config.yml")
  yaml::write_yaml(list(critical_value = 3), path)

  expect_error(
    read_config(path, simulation_config_schema),
    "bad_simulation_config.yml"
  )
})
