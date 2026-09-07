#' Two-sample t-test from summary statistics, across replicates
#'
#' @description Vectorised equivalent of [BSDA::tsum.test()] with unequal
#' variances, which the empirical Bayes power priors and the test-then-pool
#' methods call once per replicate. `x` is the source study, whose summary
#' statistics are fixed; `y` is the target study, which varies by replicate.
#'
#' @param mean_x,sd_x,n_x Mean, standard deviation and per-arm sample size of
#'   the first sample.
#' @param mean_y,sd_y,n_y Mean, standard deviation and per-arm sample size of
#'   the second sample. `mean_y` and `sd_y` may be vectors.
#' @param mu Difference in means under the null hypothesis.
#' @param alternative One of `"two.sided"`, `"less"` or `"greater"`.
#' @return Vector of p-values.
#' @export
summary_t_test_p_value <- function(mean_x, sd_x, n_x, mean_y, sd_y, n_y,
                                   mu = 0, alternative = "two.sided") {
  variance_x <- sd_x^2 / n_x
  variance_y <- sd_y^2 / n_y
  standard_error <- sqrt(variance_x + variance_y)

  # Welch-Satterthwaite degrees of freedom, as used by tsum.test.
  degrees_of_freedom <- (variance_x + variance_y)^2 /
    (variance_x^2 / (n_x - 1) + variance_y^2 / (n_y - 1))

  statistic <- (mean_x - mean_y - mu) / standard_error

  tail_probabilities(statistic, alternative, function(q, lower) {
    stats::pt(q, df = degrees_of_freedom, lower.tail = lower)
  })
}

#' Two-sample z-test from summary statistics, across replicates
#'
#' @description Vectorised equivalent of [BSDA::zsum.test()].
#'
#' @param mean_x,sd_x,n_x Mean, known standard deviation and per-arm sample size
#'   of the first sample.
#' @param mean_y,sd_y,n_y Mean, known standard deviation and per-arm sample size
#'   of the second sample. `mean_y` and `sd_y` may be vectors.
#' @param mu Difference in means under the null hypothesis.
#' @param alternative One of `"two.sided"`, `"less"` or `"greater"`.
#' @return Vector of p-values.
#' @export
summary_z_test_p_value <- function(mean_x, sd_x, n_x, mean_y, sd_y, n_y,
                                   mu = 0, alternative = "two.sided") {
  standard_error <- sqrt(sd_x^2 / n_x + sd_y^2 / n_y)
  statistic <- (mean_x - mean_y - mu) / standard_error

  tail_probabilities(statistic, alternative, function(q, lower) {
    stats::pnorm(q, lower.tail = lower)
  })
}

#' Convert test statistics to p-values for a given alternative
#'
#' @param statistic Vector of test statistics.
#' @param alternative One of `"two.sided"`, `"less"` or `"greater"`.
#' @param distribution Function of `(q, lower)` giving the null distribution.
#' @return Vector of p-values.
#' @keywords internal
tail_probabilities <- function(statistic, alternative, distribution) {
  switch(
    alternative,
    less = distribution(statistic, TRUE),
    greater = distribution(statistic, FALSE),
    two.sided = 2 * distribution(-abs(statistic), TRUE),
    stop("alternative must be \"two.sided\", \"less\" or \"greater\"")
  )
}
