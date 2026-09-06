test_that("sweet_spot_determination finds one bounded interval", {
  result <- sweet_spot_determination(
    x_values = 1:10,
    y_values = c(3, 5, 7, 9, 11, 8, 6, 4, 2, 0),
    reference_value = 5,
    larger_is_better = TRUE
  )

  expected <- data.frame(
    start = 2,
    end = 7.5,
    width = 5.5,
    total_width = 5.5
  )
  expect_equal(result, expected)
})

test_that("sweet_spot_determination returns multiple intervals", {
  expect_warning(
    result <- sweet_spot_determination(
      x_values = 1:10,
      y_values = c(3, 5, 7, 9, 11, 8, 6, 4, 6, 8),
      reference_value = 6,
      larger_is_better = TRUE
    ),
    "More than one sweet spot"
  )

  expected <- data.frame(
    start = c(2.5, 9),
    end = c(7, 10),
    width = c(4.5, 1),
    total_width = c(5.5, 5.5)
  )
  expect_equal(result, expected)
})

test_that("sweet_spot_determination handles values below the reference", {
  result <- sweet_spot_determination(
    x_values = 1:10,
    y_values = c(3, 5, 7, 9, 11, 8, 6, 4, 2, 0),
    reference_value = 12,
    larger_is_better = TRUE
  )

  expected <- data.frame(start = NA, end = NA, width = 0, total_width = 0)
  expect_equal(result, expected)
})

test_that("sweet_spot_determination handles empty values", {
  expect_warning(
    result <- sweet_spot_determination(
      x_values = 1:10,
      y_values = numeric(),
      reference_value = 5,
      larger_is_better = TRUE
    ),
    "Input vectors must have non-zero length"
  )

  expected <- data.frame(start = NA, end = NA, width = NA)
  expect_equal(result, expected)
})

test_that("sweet_spot_determination treats equality as favorable", {
  result <- sweet_spot_determination(
    x_values = 1:10,
    y_values = rep(5, 10),
    reference_value = 5,
    larger_is_better = TRUE
  )

  expected <- data.frame(start = 1, end = 10, width = 9, total_width = 9)
  expect_equal(result, expected)
})
