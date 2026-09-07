#' Type I error of the adaptive power prior, computed exactly
#'
#' @description
#' The adaptive power prior of Nikolakopoulos et al. (2018) discounts the source
#' data by a weight that depends on how far the target estimate falls from the
#' source estimate. For a given calibration parameter the decision rule is a
#' deterministic function of the target treatment effect estimate, which under
#' the null is a single normal variate. The type I error is therefore the normal
#' measure of a union of intervals, and can be computed exactly rather than
#' estimated by simulation.
#'
#' The rejection region is found piecewise. Between the two cut-offs the source
#' data are borrowed in full, the decision statistic is linear in the estimate,
#' and its boundary is explicit. Outside them the borrowing weight varies, and
#' clearing its denominator turns the boundary condition into a degree-six
#' polynomial, so [base::polyroot()] locates every crossing at once.
#'
#' @param calibration_parameter The calibration parameter, written Z_1-c/2 in
#'   the manuscript. Must be positive.
#' @param source_sample_size_per_arm Sample size per arm in the source study
#' @param target_sample_size_per_arm Sample size per arm in the target study
#' @param source_treatment_effect_estimate Treatment effect estimate in the source study
#' @param significance_level Significance level for hypothesis testing
#' @param target_data_sampling_variance Sampling variance of the target study data
#' @param source_data_sampling_variance Sampling variance of the source study data
#' @param theta_0 Value of the treatment effect under the null hypothesis
#' @return The type I error rate, a single number in [0, 1].
#' @export
adaptive_power_prior_type_I_error <- function(calibration_parameter,
                                              source_sample_size_per_arm,
                                              target_sample_size_per_arm,
                                              source_treatment_effect_estimate,
                                              significance_level,
                                              target_data_sampling_variance,
                                              source_data_sampling_variance,
                                              theta_0 = 0) {
  if (!is.finite(calibration_parameter) || calibration_parameter <= 0) {
    stop(
      "The calibration parameter must be positive, but it is ",
      format(calibration_parameter),
      ". The tail probability 2 * pnorm(-Z) is not a probability otherwise, ",
      "so the borrowing weight is undefined."
    )
  }

  n <- target_sample_size_per_arm
  tau2 <- target_data_sampling_variance
  equivalent_target_sample_size <- n * tau2 / source_data_sampling_variance
  prior_variance <- tau2 / source_sample_size_per_arm
  std_predictive_dist <- sqrt(tau2 / equivalent_target_sample_size + tau2 / n)
  sampling_sd <- sqrt(tau2 / n)
  z_alpha <- qnorm(significance_level)

  # The original code wrote the cut-off as qnorm(1 - B / 2) with
  # B = 2 * pnorm(-Z). That round trip is the identity, so the cut-off is Z.
  cutoff <- std_predictive_dist * calibration_parameter

  borrowing_weight <- function(x) {
    deviation <- x - source_treatment_effect_estimate
    ifelse(
      abs(deviation) > cutoff,
      prior_variance /
        ((deviation / calibration_parameter)^2 - tau2 / n),
      1
    )
  }

  # The decision rule rejects when the posterior quantile exceeds zero. Clearing
  # the (always positive) posterior precision denominator leaves this, which has
  # the same sign and no removable singularity.
  decision_statistic <- function(x) {
    effective_source_size <- borrowing_weight(x) * equivalent_target_sample_size
    effective_source_size * source_treatment_effect_estimate + x * n +
      z_alpha * sqrt(tau2 * (effective_source_size + n))
  }

  lower_cutoff <- source_treatment_effect_estimate - cutoff
  upper_cutoff <- source_treatment_effect_estimate + cutoff

  # Between the cut-offs the weight is one, so the statistic is linear in the
  # estimate and its single zero is explicit.
  full_borrowing_boundary <-
    (-z_alpha * sqrt(tau2 * (equivalent_target_sample_size + n)) -
       equivalent_target_sample_size * source_treatment_effect_estimate) / n

  # Outside them the weight is prior_variance / D(x), with D quadratic in the
  # estimate. Clearing that denominator in
  #   (g * theta_s + x * n)^2 = z_alpha^2 * tau2 * (g + n),   g = K / D(x)
  # leaves a degree-six polynomial whose roots include every boundary of the
  # rejection region on both outer branches. Squaring introduces spurious roots
  # and some roots fall between the cut-offs where the formula does not hold,
  # but neither has to be filtered out: the rejection region is decided below by
  # evaluating the statistic between consecutive breakpoints, so a breakpoint
  # that is not a real crossing merely splits an interval in two.
  #
  # This replaces scanning for sign changes on a grid, which could step over the
  # narrow slivers of rejection region that sit immediately outside each cut-off.
  quadratic <- c(
    source_treatment_effect_estimate^2 / calibration_parameter^2 - tau2 / n,
    -2 * source_treatment_effect_estimate / calibration_parameter^2,
    1 / calibration_parameter^2
  )
  scaled_source_size <- equivalent_target_sample_size * prior_variance
  cubic <- c(scaled_source_size * source_treatment_effect_estimate, n * quadratic)

  pad <- function(coefficients, degree) {
    c(coefficients, numeric(degree + 1L - length(coefficients)))
  }
  multiply <- function(a, b) {
    product <- numeric(length(a) + length(b) - 1L)
    for (i in seq_along(a)) {
      at <- i + seq_along(b) - 1L
      product[at] <- product[at] + a[i] * b
    }
    product
  }

  polynomial <- pad(multiply(cubic, cubic), 6L) -
    z_alpha^2 * tau2 * pad(
      pad(scaled_source_size * quadratic, 4L) +
        n * multiply(quadratic, quadratic), 6L
    )
  roots <- polyroot(polynomial)
  real_roots <- Re(roots)[abs(Im(roots)) < 1e-6 * pmax(1, abs(Re(roots)))]

  breakpoints <- sort(unique(c(
    lower_cutoff, upper_cutoff, full_borrowing_boundary, real_roots
  )))
  edges <- c(-Inf, breakpoints, Inf)

  # The statistic has constant sign between consecutive breakpoints, so one
  # interior evaluation classifies each interval.
  probability <- 0
  for (i in seq_len(length(edges) - 1L)) {
    from <- edges[i]
    to <- edges[i + 1L]
    interior <- if (is.infinite(from)) {
      to - 1
    } else if (is.infinite(to)) {
      from + 1
    } else {
      (from + to) / 2
    }
    if (decision_statistic(interior) > 0) {
      probability <- probability +
        pnorm(to, theta_0, sampling_sd) - pnorm(from, theta_0, sampling_sd)
    }
  }
  probability
}

