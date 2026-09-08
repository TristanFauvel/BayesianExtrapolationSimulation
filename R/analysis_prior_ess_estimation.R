#' Calculate the moment-based Effective Sample Size (ESS) for a Gaussian mixture.
#'
#' @description This function calculates the moment-based ESS for a Gaussian mixture distribution.
#' It takes a mixture object and an optional prior reference scale as input and returns the ESS.
#'
#' @param mix A mixture object.
#' @return The moment-based ESS for the Gaussian mixture.
#' @export
gaussian_mix_moment_ess <- function(mix) {
  sigma <- RBesT::sigma(mix)
  if (is.null(sigma)) {
    stop("Reference scale is NULL, must be a number for ESS estimation")
  }

  ## simple and conservative moment matching
  smix <- summary(mix)
  res <- sigma ^ 2 / smix["sd"] ^ 2
  return(unname(res))
}

#' Calculate the precision-based Effective Sample Size (ESS) for a Gaussian mixture.
#'
#' @description This function calculates the precision-based ESS for a Gaussian mixture distribution.
#' It takes a mixture object and an optional prior reference scale as input and returns the ESS.
#'
#' @param mix A mixture object.
#' @return The precision-based ESS for the Gaussian mixture.
#' @export
gaussian_mix_precision_ess <- function(mix) {
  # Precision-based matching method for ESS estimation, for Gaussian mixtures only.
  sigma <- RBesT::sigma(mix)
  if (is.null(sigma)) {
    stop("Reference scale is NULL, must be a number of ESS estimation")
  }

  smix <- summary(mix)

  # Find the indices corresponding to "97.5%" and "2.5%"
  index_97.5 <- grep("97.5%", names(smix))
  index_2.5 <- grep("2.5%", names(smix))
  # Calculate half-width of 95% confidence interval
  half_width <- as.numeric((smix[index_97.5] - smix[index_2.5]) / 2)

  alpha <- 0.05
  sd <- half_width / stats::qnorm(1 - alpha / 2)

  res <- sigma ^ 2 / sd ^ 2
  return(unname(res))
}

#' Calculate the prior moment-based Effective Sample Size (ESS) for a Bayesian model.
#'
#' @description This function calculates the prior moment-based ESS for a Bayesian model using the RBesT package.
#' It takes a RBesT model object and target data as input and returns the prior moment-based ESS.
#'
#' @param rbest_model A RBesT model object.
#' @param target_data Target data containing sample information.
#' @return The prior moment-based ESS for the Bayesian model.
#' @export
prior_moment_ess <- function(rbest_model, target_data) {
  if (is.null(rbest_model)) {
    stop("The posterior is NULL")
  }

  posterior_moment_ess <- RBesT::ess(rbest_model,
                                     method = "moment",
                                     sigma = RBesT::sigma(rbest_model))

  return(posterior_moment_ess - target_data$sample_size_per_arm)
}

#' Calculate the prior precision-based Effective Sample Size (ESS) for a Bayesian model.
#'
#' @description This function calculates the prior precision-based ESS for a Bayesian model using the RBesT package.
#' It takes a RBesT model object and target data as input and returns the prior precision-based ESS.
#'
#' @param rbest_model A RBesT model object.
#' @param target_data Target data containing sample information.
#' @return The prior precision-based ESS for the Bayesian model.
#' @export
prior_precision_ess <- function(rbest_model, target_data) {
  if (target_data$summary_measure_likelihood == "normal") {
    posterior_precision_ess <- gaussian_mix_precision_ess(rbest_model)
  } else if (target_data$summary_measure_likelihood == "binomial") {
    posterior_precision_ess <- gaussian_mix_precision_ess(rbest_model)
  }
  return(posterior_precision_ess - target_data$sample_size_per_arm)
}


#' Calculate the priorEffective Sample Size (ESS) based on ELIR for a Bayesian model.
#'
#' @description This function calculates the ELIR for a Bayesian model using the RBesT package.
#' It takes a RBesT model object and target data as input and returns the prior moment-based ESS.
#'
#' @param rbest_model A RBesT model object.
#' @param target_data Target data containing sample information.
#' @return The prior moment-based ESS for the Bayesian model.
#' @export
prior_ess_elir <- function(rbest_model, target_data) {
  if (is.null(rbest_model)) {
    stop("The prior is NULL")
  }

  tryCatch({
    elir <- RBesT::ess(rbest_model, method = "elir", sigma = RBesT::sigma(rbest_model))
    return(elir)
  }, error = function(e) {
    model_summary <- summary(rbest_model)
    if (model_summary['sd'] == "Inf"){
      return(0)
    } else {
      # When an error occurs, print the error and return NA
      message("An error occurred: ", e$message)
      return(NA)
    }
  })
}


#' Effective sample sizes of a posterior summarised against a normal reference
#'
#' @description Both effective sample sizes reported per replicate compare the
#' posterior against a normal reference of known scale. The moment version uses
#' the posterior variance directly; the precision version uses the variance a
#' normal distribution would need in order to have the same 95% credible
#' interval width. Both are expressed relative to the target study, by
#' subtracting its per-arm sample size.
#'
#' Models whose posterior summary is available exactly, or already computed,
#' evaluate these definitions directly rather than fitting a mixture to samples
#' drawn from the posterior.
#'
#' @param reference_scale Reference scale, i.e. the sampling standard deviation
#'   of the target study.
#' @param posterior_sd Standard deviation of the posterior treatment effect.
#' @param lower Lower bound of the 95% credible interval.
#' @param upper Upper bound of the 95% credible interval.
#' @param sample_size_per_arm Per-arm sample size of the target study.
#' @return A list with the `moment` and `precision` effective sample sizes.
#' @export
normal_reference_ess <- function(reference_scale,
                                 posterior_sd,
                                 lower,
                                 upper,
                                 sample_size_per_arm) {
  implied_sd <- ((upper - lower) / 2) / stats::qnorm(0.975)

  return(list(
    moment = reference_scale^2 / posterior_sd^2 - sample_size_per_arm,
    precision = reference_scale^2 / implied_sd^2 - sample_size_per_arm
  ))
}


#' Expected local information ratio ESS of a truncated normal mixture
#'
#' @description The ELIR effective sample size of a prior \eqn{\pi} against a
#' normal likelihood of known scale \eqn{\sigma} is
#' \deqn{\sigma^2 \, E_\pi[-(\log \pi)''(\theta)],}
#' which is what [RBesT::ess()] evaluates for an untruncated normal mixture.
#'
#' A normal mixture truncated to an interval has no representation as an
#' untruncated mixture, so the alternative is to fit one to a sample drawn from
#' it. That costs a mixture fit per replicate and leaves both Monte Carlo noise
#' and an upward bias in the result. This function integrates the definition
#' directly instead.
#'
#' Each component is truncated and renormalised on its own, matching how the
#' robust mixture prior is drawn from. Inside the interval the truncation is a
#' constant factor on the density, so it does not contribute to the second
#' derivative of the log density; it enters only through the normalisers and the
#' domain of integration.
#'
#' @param weights Component weights of the mixture, summing to one.
#' @param means Component means.
#' @param sds Component standard deviations.
#' @param lower Lower truncation point.
#' @param upper Upper truncation point.
#' @param sigma Reference scale of the normal likelihood.
#' @return The ELIR effective sample size.
#' @export
truncated_normal_mixture_elir <- function(weights, means, sds, lower, upper,
                                          sigma) {
  assertions::assert_number(lower)
  assertions::assert_number(upper)
  if (lower >= upper) {
    stop("The truncation interval must be non-empty.", call. = FALSE)
  }

  normalisers <- stats::pnorm(upper, means, sds) -
    stats::pnorm(lower, means, sds)

  # Simpson's rule on a grid fine enough to resolve the narrowest component,
  # which for the robust mixture prior is the informative one and can be orders
  # of magnitude narrower than the truncation interval.
  n_nodes <- max(2001, ceiling(40 * (upper - lower) / min(sds)))
  n_nodes <- min(n_nodes, 400001)
  if (n_nodes %% 2 == 0) {
    n_nodes <- n_nodes + 1
  }

  # The endpoints are stepped away from: a component whose truncation point sits
  # far into its tail contributes no density there, and the log density is not
  # defined where the density underflows to zero.
  margin <- (upper - lower) * 1e-9
  nodes <- seq(lower + margin, upper - margin, length.out = n_nodes)
  spacing <- nodes[2] - nodes[1]

  component <- outer(nodes, seq_along(weights), function(theta, k) {
    weights[k] * stats::dnorm(theta, means[k], sds[k]) / normalisers[k]
  })
  standardised <- outer(nodes, seq_along(weights), function(theta, k) {
    (theta - means[k]) / sds[k]^2
  })
  precision <- matrix(rep(1 / sds^2, each = n_nodes), nrow = n_nodes)

  density <- rowSums(component)
  first_derivative <- rowSums(component * -standardised)
  second_derivative <- rowSums(component * (standardised^2 - precision))

  # -(log f)'' = (f'/f)^2 - f''/f, multiplied by the density it is integrated
  # against, so one factor of the density cancels and the integrand stays finite
  # wherever the density underflows to zero.
  integrand <- first_derivative^2 / density - second_derivative
  integrand[density == 0] <- 0

  simpson_weights <- c(1, rep(c(4, 2), length.out = n_nodes - 2), 1)

  return(sigma^2 * sum(simpson_weights * integrand) * spacing / 3)
}
