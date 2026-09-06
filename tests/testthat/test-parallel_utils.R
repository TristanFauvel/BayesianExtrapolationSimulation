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
