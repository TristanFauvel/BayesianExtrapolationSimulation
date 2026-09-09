## The data objects in R/simulation_data.R are built once per scenario and once
## per result row, and assertions::assert_number() costs ~190us a call because
## it deparses its own call and dispatches over a list of validators - half of
## load_data() was argument checking. assert_single_number() accepts exactly
## the same values for ~0.4us.

test_that("assert_single_number accepts what assert_number accepts", {
  expect_silent(assert_single_number(1.5))
  expect_silent(assert_single_number(3L))
  expect_silent(assert_single_number(NA_real_))
  expect_silent(assert_single_number(Inf))
  expect_invisible(assert_single_number(2))
})

test_that("assert_single_number rejects what assert_number rejects", {
  expect_error(assert_single_number("a"), "not a number")
  expect_error(assert_single_number(c(1, 2)), "not a number")
  expect_error(assert_single_number(NULL), "not a number")
  expect_error(assert_single_number(list(1)), "not a number")
})

test_that("the two agree on every case, so swapping one for the other is safe", {
  cases <- list(1.5, 3L, NA_real_, Inf, -2.5, "a", c(1, 2), NULL, list(1), character(0), TRUE)
  passes <- function(f) vapply(cases, function(x) {
    isTRUE(tryCatch(
      {
        f(x)
        TRUE
      },
      error = function(e) FALSE
    ))
  }, logical(1))

  expect_identical(passes(assert_single_number), passes(assertions::assert_number))
})

test_that("the failure message still names the value and says what was wrong", {
  bad_value <- "not numeric"
  expect_error(assert_single_number(bad_value), "bad_value", fixed = TRUE)
  expect_error(assert_single_number(bad_value), "class is character, not numeric", fixed = TRUE)
  expect_error(assert_single_number(c(1, 2, 3)), "length is 3, not 1", fixed = TRUE)
})
