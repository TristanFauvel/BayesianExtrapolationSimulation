test_that("check_probability_value warns when clamping invalid values", {
  expect_warning(
    expect_equal(BExTE:::check_probability_value(-0.1), 0),
    "Rounding the value to 0"
  )

  expect_warning(
    expect_equal(BExTE:::check_probability_value(1.1), 1),
    "Rounding the value to 1"
  )
})
