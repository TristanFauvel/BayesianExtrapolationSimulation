test_that("get_parallel_worker_count always returns a valid cluster size", {
  expect_identical(
    get_parallel_worker_count(detected_cores = NA_integer_),
    1L
  )
  expect_identical(
    get_parallel_worker_count(detected_cores = 1L),
    1L
  )
  expect_identical(
    get_parallel_worker_count(detected_cores = 8L),
    7L
  )
  expect_identical(
    get_parallel_worker_count(max_workers = 8L, detected_cores = 32L),
    8L
  )
})

test_that("limit_mcmc_chain_parallelism runs chains sequentially when scenarios are parallel", {
  mcmc_config <- list(num_chains = 4L, parallel_chains = 4L, chain_length = 5000L)

  adjusted <- limit_mcmc_chain_parallelism(mcmc_config, parallelization = TRUE)

  expect_identical(adjusted$parallel_chains, 1L)
  # Every other setting is left alone.
  expect_identical(adjusted$num_chains, 4L)
  expect_identical(adjusted$chain_length, 5000L)
})

test_that("limit_mcmc_chain_parallelism leaves the configuration alone when scenarios are sequential", {
  mcmc_config <- list(num_chains = 4L, parallel_chains = 4L, chain_length = 5000L)

  expect_identical(
    limit_mcmc_chain_parallelism(mcmc_config, parallelization = FALSE),
    mcmc_config
  )
})
