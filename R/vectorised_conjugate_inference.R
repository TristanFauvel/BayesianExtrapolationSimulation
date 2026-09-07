#' Run a whole normal-mixture simulation without looping over replicates
#'
#' @description Shared implementation behind the vectorised fast path. Given the
#' prior mixture for every replicate, this reproduces exactly what the replicate
#' loop would have produced: posterior moments, medians, credible intervals,
#' test decisions and effective sample sizes.
#'
#' @param weights Prior component weights, shared across replicates (a vector)
#'   or one row per replicate (a matrix).
#' @param means Prior component means, shaped like `weights`.
#' @param sds Prior component standard deviations, shaped like `weights`.
#' @param samples Data frame of generated replicates, with columns
#'   `treatment_effect_estimate`, `treatment_effect_standard_error` and
#'   `standard_deviation`.
#' @param target_data Target data object, used for its sample size per arm.
#' @param to_return Character vector of requested outputs.
#' @param critical_value Critical value for the test decision.
#' @param theta_0 Null hypothesis value.
#' @param confidence_level Credible interval level.
#' @param null_space Either `"left"` or `"right"`.
#' @param decision_rule `"credible_interval"` to decide from the 95% credible
#'   interval, as the RBesT-backed models do, or `"posterior_cdf"` to compare
#'   the posterior tail probability against `critical_value`, as the plain
#'   conjugate models do.
#' @param posterior_parameters Optional data frame of per-replicate posterior
#'   parameters to report.
#' @return A list shaped like the return value of
#'   [Model$simulation_for_given_treatment_effect()].
#' @keywords internal
vectorised_normal_mixture_simulation <- function(weights, means, sds,
                                                 samples, target_data,
                                                 to_return,
                                                 critical_value, theta_0,
                                                 confidence_level, null_space,
                                                 decision_rule,
                                                 posterior_parameters = NULL) {
  requested <- function(output) output %in% to_return

  estimate <- samples$treatment_effect_estimate
  standard_error <- samples$treatment_effect_standard_error
  n_replicates <- length(estimate)

  posterior <- normal_mixture_posterior(
    weights = weights,
    means = means,
    sds = sds,
    estimate = estimate,
    standard_error = standard_error
  )

  alpha <- (1 - confidence_level) / 2
  summaries <- normal_mixture_summary(
    posterior$weights, posterior$means, posterior$sds,
    probs = c(alpha, 0.5, 1 - alpha)
  )

  credible_intervals <- summaries$quantiles[, c(1, 3), drop = FALSE]

  test_decisions <- if (requested("test_decision")) {
    as.numeric(vectorised_test_decision(
      lower = credible_intervals[, 1],
      upper = credible_intervals[, 2],
      posterior_weights = posterior$weights,
      posterior_means = posterior$means,
      posterior_sds = posterior$sds,
      critical_value = critical_value,
      theta_0 = theta_0,
      null_space = null_space,
      decision_rule = decision_rule
    ))
  } else {
    NULL
  }

  # The reference scale for effective sample sizes is the per-replicate
  # sampling standard deviation, exactly as the replicate loop sets it.
  reference_scale <- samples$standard_deviation

  ess_moments <- if (requested("ess_moment")) {
    reference_scale^2 / summaries$sd^2 - target_data$sample_size_per_arm
  } else {
    NULL
  }

  ess_precisions <- if (requested("ess_precision")) {
    half_width <- (credible_intervals[, 2] - credible_intervals[, 1]) / 2
    implied_sd <- half_width / stats::qnorm(0.975)
    reference_scale^2 / implied_sd^2 - target_data$sample_size_per_arm
  } else {
    NULL
  }

  ess_elir <- if (requested("ess_elir")) {
    # Passed through unrecycled: a prior shared by every replicate only needs
    # the ELIR integral evaluating once.
    normal_mixture_elir_ess(
      weights = weights,
      means = means,
      sds = sds,
      sigma = reference_scale
    )
  } else {
    NULL
  }

  list(
    test_decisions = test_decisions,
    posterior_means = if (requested("posterior_mean")) summaries$mean else NULL,
    posterior_medians = if (requested("posterior_median")) summaries$quantiles[, 2] else NULL,
    credible_intervals = if (requested("credible_interval")) credible_intervals else NULL,
    posterior_parameters = if (requested("posterior_parameters")) posterior_parameters else NULL,
    ess_moments = ess_moments,
    ess_precisions = ess_precisions,
    ess_elir = ess_elir,
    fit_success = if (requested("fit_success")) rep("Success", n_replicates) else NULL,
    # No method with a closed-form posterior samples, but the replicate loop
    # still returns zero-filled diagnostics when they are asked for, and
    # estimate_frequentist_operating_characteristics() averages them into
    # required result columns. Returning NULL instead would turn those columns
    # into NA.
    mcmc_ess = if (requested("mcmc_diagnostics")) numeric(n_replicates) else NULL,
    rhat = if (requested("mcmc_diagnostics")) numeric(n_replicates) else NULL,
    n_divergences = if (requested("mcmc_diagnostics")) numeric(n_replicates) else NULL
  )
}

#' Test decision for every replicate at once
#'
#' @description Reproduces the two decision rules already in use.
#' [Model_RBesT] populates a posterior summary, so `Model$test_decision()` takes
#' the credible interval branch and ignores `critical_value`; [ConjugateGaussian]
#' leaves that summary empty and takes the posterior CDF branch, which honours
#' it. The fast path keeps each family on its own rule rather than unifying
#' them.
#'
#' @param lower Lower credible interval bounds.
#' @param upper Upper credible interval bounds.
#' @param posterior_weights `n_replicates x n_components` posterior weights.
#' @param posterior_means `n_replicates x n_components` posterior means.
#' @param posterior_sds `n_replicates x n_components` posterior standard
#'   deviations.
#' @param critical_value Critical value for the posterior CDF rule.
#' @param theta_0 Null hypothesis value.
#' @param null_space Either `"left"` or `"right"`.
#' @param decision_rule `"credible_interval"` or `"posterior_cdf"`.
#' @return Logical vector of decisions.
#' @keywords internal
vectorised_test_decision <- function(lower, upper,
                                     posterior_weights, posterior_means,
                                     posterior_sds,
                                     critical_value, theta_0, null_space,
                                     decision_rule) {
  if (decision_rule == "credible_interval") {
    if (null_space == "left") {
      return(lower > theta_0)
    }
    return(upper < theta_0)
  }

  cdf_at_theta_0 <- rowSums(
    posterior_weights * stats::pnorm(theta_0, posterior_means, posterior_sds)
  )

  if (null_space == "left") {
    return(1 - cdf_at_theta_0 > critical_value)
  }
  cdf_at_theta_0 > critical_value
}

#' Conjugate update of a normal mixture prior across replicates
#'
#' @description Applies the normal-normal conjugate update to every replicate at
#' once. This is the vectorised equivalent of calling [RBesT::postmix()] once per
#' replicate: for a prior component with weight \eqn{w_k}, mean \eqn{\mu_k} and
#' standard deviation \eqn{s_k}, and an observation \eqn{(m_i, se_i)}, the
#' posterior component has variance \eqn{1/(1/s_k^2 + 1/se_i^2)}, mean
#' \eqn{v_{ik}(\mu_k/s_k^2 + m_i/se_i^2)} and weight proportional to
#' \eqn{w_k \, N(m_i; \mu_k, s_k^2 + se_i^2)}.
#'
#' @param weights Prior component weights. Either a vector of length
#'   `n_components` (a prior shared by every replicate) or a
#'   `n_replicates x n_components` matrix (one prior per replicate, as needed by
#'   empirical Bayes methods).
#' @param means Prior component means, shaped like `weights`.
#' @param sds Prior component standard deviations, shaped like `weights`.
#' @param estimate Vector of per-replicate treatment effect estimates.
#' @param standard_error Vector of per-replicate standard errors.
#' @return A list of three `n_replicates x n_components` matrices: `weights`,
#'   `means` and `sds` of the posterior mixture.
#' @export
normal_mixture_posterior <- function(weights, means, sds, estimate, standard_error) {
  n_replicates <- length(estimate)

  weights <- recycle_prior_component(weights, n_replicates)
  means <- recycle_prior_component(means, n_replicates)
  sds <- recycle_prior_component(sds, n_replicates)

  prior_precision <- 1 / sds^2
  data_precision <- 1 / standard_error^2

  posterior_variance <- 1 / (prior_precision + data_precision)
  posterior_mean <- posterior_variance *
    (means * prior_precision + estimate * data_precision)

  # Marginal likelihood of the observation under each component, in log space so
  # that a component with negligible weight cannot underflow to an NaN weight.
  log_weight <- log(weights) +
    stats::dnorm(estimate, means, sqrt(sds^2 + standard_error^2), log = TRUE)
  log_weight <- log_weight - apply(log_weight, 1, max)
  posterior_weight <- exp(log_weight)
  posterior_weight <- posterior_weight / rowSums(posterior_weight)

  list(
    weights = posterior_weight,
    means = posterior_mean,
    sds = sqrt(posterior_variance)
  )
}

#' Mean, standard deviation and quantiles of a normal mixture, per replicate
#'
#' @description Vectorised equivalent of `summary()` on an [RBesT::mixnorm()]
#' object. The mean and standard deviation are closed form. The quantiles invert
#' the mixture CDF by bisection, which runs on every replicate simultaneously;
#' RBesT instead calls `uniroot` once per mixture, at a looser tolerance.
#'
#' @param weights `n_replicates x n_components` matrix of mixture weights.
#' @param means `n_replicates x n_components` matrix of component means.
#' @param sds `n_replicates x n_components` matrix of component standard
#'   deviations.
#' @param probs Probabilities at which to evaluate the quantile function.
#' @return A list with `mean` and `sd` vectors of length `n_replicates`, and a
#'   `n_replicates x length(probs)` matrix of `quantiles`.
#' @export
normal_mixture_summary <- function(weights, means, sds, probs = c(0.025, 0.5, 0.975)) {
  mixture_mean <- rowSums(weights * means)
  # Law of total variance: E[X^2] - E[X]^2 over the mixture components.
  mixture_variance <- rowSums(weights * (sds^2 + means^2)) - mixture_mean^2
  mixture_sd <- sqrt(mixture_variance)

  quantiles <- vapply(
    probs,
    function(p) normal_mixture_quantile(weights, means, sds, p, mixture_mean, mixture_sd),
    numeric(nrow(weights))
  )
  # vapply drops to a vector when there is a single replicate.
  quantiles <- matrix(quantiles, nrow = nrow(weights), ncol = length(probs))

  list(mean = mixture_mean, sd = mixture_sd, quantiles = quantiles)
}

#' Invert a normal mixture CDF for every replicate at once
#'
#' @description Bisection on the mixture CDF, which is strictly increasing. The
#' bracket starts at the mixture mean plus or minus a multiple of its standard
#' deviation, widened until it straddles the root, then halved to convergence.
#'
#' @param weights `n_replicates x n_components` matrix of mixture weights.
#' @param means `n_replicates x n_components` matrix of component means.
#' @param sds `n_replicates x n_components` matrix of component standard
#'   deviations.
#' @param p Single probability at which to evaluate the quantile function.
#' @param mixture_mean Vector of mixture means, used to seed the bracket.
#' @param mixture_sd Vector of mixture standard deviations, used to seed the
#'   bracket.
#' @param tolerance Absolute width at which bisection stops.
#' @return Vector of quantiles, one per replicate.
#' @keywords internal
normal_mixture_quantile <- function(weights, means, sds, p,
                                    mixture_mean, mixture_sd,
                                    tolerance = 1e-12) {
  cdf <- function(x) rowSums(weights * stats::pnorm(x, means, sds))

  # A normal mixture is bounded by its most extreme component, so widening the
  # bracket geometrically is guaranteed to bracket the root in a few steps.
  half_width <- 10 * mixture_sd
  lower <- mixture_mean - half_width
  upper <- mixture_mean + half_width
  for (i in seq_len(50)) {
    too_high <- cdf(lower) > p
    too_low <- cdf(upper) < p
    if (!any(too_high) && !any(too_low)) {
      break
    }
    half_width <- half_width * 2
    lower <- ifelse(too_high, mixture_mean - half_width, lower)
    upper <- ifelse(too_low, mixture_mean + half_width, upper)
  }

  # log2(20 * sd / tolerance) steps suffice; 200 covers any realistic scale.
  for (i in seq_len(200)) {
    middle <- (lower + upper) / 2
    below <- cdf(middle) < p
    lower <- ifelse(below, middle, lower)
    upper <- ifelse(below, upper, middle)
    if (max(upper - lower) < tolerance) {
      break
    }
  }

  (lower + upper) / 2
}

#' ELIR effective sample size of a normal mixture, per replicate
#'
#' @description Vectorised equivalent of `RBesT::ess(mix, method = "elir")`. The
#' ELIR effective sample size is
#' \eqn{\sigma^2 \int p(\theta) \, i(\theta) \, d\theta}, where
#' \eqn{i(\theta) = -\partial^2_\theta \log p(\theta)} is the Fisher information
#' of the prior.
#'
#' RBesT evaluates that integral with Gauss-Hermite quadrature centred on each
#' mixture component, doubling the node count until successive estimates agree
#' and falling back to adaptive quadrature if they never do. That fallback is
#' the usual outcome for a robust mixture prior, whose informative and vague
#' components differ in scale by a factor of around twenty: nodes spaced for the
#' vague component step over the sharp peak the informative one puts in
#' \eqn{i(\theta)}, so the estimate never settles.
#'
#' This function instead integrates \eqn{p\,i} directly, over panels split at
#' every component's centre and tails, with Gauss-Legendre quadrature on each
#' panel. Every component then gets panels matched to its own width, whatever
#' the spread of scales, and the result agrees with RBesT to around 1e-10 while
#' running on all replicates at once.
#'
#' Two cases short-circuit the quadrature entirely. A single-component mixture
#' has constant Fisher information, so its ELIR is exactly
#' \eqn{\sigma^2/\mathrm{variance}}. A prior shared by every replicate is
#' integrated once and rescaled, since ELIR is proportional to \eqn{\sigma^2}.
#'
#' @param weights Mixture weights: a vector when the prior is shared by every
#'   replicate, or a `n_replicates x n_components` matrix.
#' @param means Component means, shaped like `weights`.
#' @param sds Component standard deviations, shaped like `weights`.
#' @param sigma Reference scale, either a single value or one value per
#'   replicate.
#' @param n_nodes Number of Gauss-Legendre nodes per panel.
#' @param spread How many standard deviations each component's panels reach.
#' @return Vector of ELIR effective sample sizes, one per replicate.
#' @export
normal_mixture_elir_ess <- function(weights, means, sds, sigma,
                                    n_nodes = 40L, spread = 9) {
  if (!is.matrix(weights)) {
    # The prior is shared by every replicate, so the integral only has to be
    # evaluated once: ELIR is proportional to the squared reference scale.
    unit_scale <- normal_mixture_elir_ess(
      weights = matrix(weights, nrow = 1),
      means = matrix(means, nrow = 1),
      sds = matrix(sds, nrow = 1),
      sigma = 1,
      n_nodes = n_nodes, spread = spread
    )
    return(sigma^2 * unit_scale)
  }

  n_replicates <- nrow(weights)
  sigma <- rep_len(sigma, n_replicates)

  if (ncol(weights) == 1L) {
    return(sigma^2 / sds[, 1]^2)
  }

  breakpoints <- mixture_support_breakpoints(means, sds, spread)
  rule <- statmod::gauss.quad(n_nodes, kind = "legendre")

  expected_information <- numeric(n_replicates)
  for (panel in seq_len(ncol(breakpoints) - 1L)) {
    lower <- breakpoints[, panel]
    upper <- breakpoints[, panel + 1L]
    half_width <- (upper - lower) / 2
    midpoint <- (upper + lower) / 2

    nodes <- midpoint + outer(half_width, rule$nodes)
    integrand <- normal_mixture_density_information(weights, means, sds, nodes)
    expected_information <- expected_information +
      half_width * drop(integrand %*% rule$weights)
  }

  sigma^2 * expected_information
}

#' Density times Fisher information of a normal mixture at a grid of points
#'
#' @description Returns \eqn{p(x)\,i(x)}, the integrand of the ELIR expectation,
#' with \eqn{i(x) = -\partial^2_x \log p(x)}. Both factors come out of the same
#' log-sum-exp pass, which keeps a component with negligible responsibility from
#' underflowing.
#'
#' @param weights `n_replicates x n_components` matrix of mixture weights.
#' @param means `n_replicates x n_components` matrix of component means.
#' @param sds `n_replicates x n_components` matrix of component standard
#'   deviations.
#' @param x `n_replicates x n_points` matrix of evaluation points.
#' @return A `n_replicates x n_points` matrix of \eqn{p(x)\,i(x)}.
#' @keywords internal
normal_mixture_density_information <- function(weights, means, sds, x) {
  n_components <- ncol(weights)
  dims <- dim(x)

  # Component log densities, stacked as (replicate x point) slices.
  log_density <- vector("list", n_components)
  for (k in seq_len(n_components)) {
    log_density[[k]] <- log(weights[, k]) +
      stats::dnorm(x, means[, k], sds[, k], log = TRUE)
  }

  largest <- Reduce(pmax, log_density)
  scaled <- lapply(log_density, function(l) exp(l - largest))
  normaliser <- Reduce(`+`, scaled)

  # score = d/dx log p, curvature = sum_k omega_k (phi_k'' / phi_k)
  score <- matrix(0, dims[1], dims[2])
  curvature <- matrix(0, dims[1], dims[2])
  for (k in seq_len(n_components)) {
    omega <- scaled[[k]] / normaliser
    standardised <- (x - means[, k]) / sds[, k]^2
    score <- score - omega * standardised
    curvature <- curvature + omega * (standardised^2 - 1 / sds[, k]^2)
  }

  density <- exp(largest) * normaliser
  density * (score^2 - curvature)
}

#' Integration breakpoints covering every component's own scale
#'
#' @description A mixture whose components differ widely in scale cannot be
#' integrated on a single grid: a rule fine enough for the broad component steps
#' straight over the narrow one. Splitting the range at each component's centre
#' and tails gives every component at least one panel matched to its own width.
#'
#' @param means `n_replicates x n_components` matrix of component means.
#' @param sds `n_replicates x n_components` matrix of component standard
#'   deviations.
#' @param spread How many standard deviations each component should reach.
#' @return A `n_replicates x (3 * n_components)` matrix of breakpoints, sorted
#'   within each row. Repeated values give empty panels, which contribute
#'   nothing.
#' @keywords internal
mixture_support_breakpoints <- function(means, sds, spread = 9) {
  breakpoints <- cbind(means - spread * sds, means, means + spread * sds)
  matrix(t(apply(breakpoints, 1, sort)), nrow = nrow(means))
}

#' Expand a prior specification to one row per replicate
#'
#' @param x A vector of component values shared by every replicate, or a matrix
#'   with one row per replicate.
#' @param n_replicates Number of replicates.
#' @return A `n_replicates x n_components` matrix.
#' @keywords internal
recycle_prior_component <- function(x, n_replicates) {
  if (is.matrix(x)) {
    return(x)
  }
  matrix(x, nrow = n_replicates, ncol = length(x), byrow = TRUE)
}
