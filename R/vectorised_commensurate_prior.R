#' Discretise the commensurate power prior as a normal mixture
#'
#' @description Conditional on the commensurability precision `tau` and power
#' parameter `gamma`, the treatment-effect prior is normal. Quadrature over
#' those two parameters therefore turns the continuous prior into a finite
#' normal mixture. The shared conjugate-mixture kernel can update that mixture
#' for every simulated target estimate at once, avoiding a separate Stan fit
#' for each replicate.
#'
#' All three families are integrated the same way, through their quantile
#' representation on a Gauss-Legendre rule over the probability scale. For the
#' inverse-gamma family this remains stable for the configured shapes as small
#' as 0.001, for which direct density quadrature is dominated by an endpoint
#' singularity.
#'
#' @param model A [GaussianCommensuratePowerPrior] object.
#' @param n_tau Number of quadrature nodes for the commensurability parameter.
#'   At the default every configured prior family agrees with a 1536-node rule
#'   to four significant figures, including the `inverse_gamma(0.001, 1)` prior,
#'   whose quantile function is the steepest of them.
#' @param n_gamma Number of conditional power-parameter nodes per `tau` node.
#' @return A list containing normal-mixture `weights`, `means` and `sds`, plus
#'   the `tau` and `power_parameter` value represented by each component.
#' @keywords internal
commensurate_prior_mixture <- function(model, n_tau = 48L, n_gamma = 24L) {
  tau_rule <- commensurate_tau_quadrature(model, n_tau)
  gamma_rule <- statmod::gauss.quad(n_gamma, kind = "legendre")
  uniform_node <- (gamma_rule$nodes + 1) / 2
  uniform_weight <- gamma_rule$weights / 2

  beta_shape <- g_function(tau_rule$log_tau)
  power_parameter <- as.vector(outer(
    uniform_node,
    1 / beta_shape,
    function(probability, inverse_shape) {
      exp(log(probability) * inverse_shape)
    }
  ))

  tau <- rep(tau_rule$tau, each = n_gamma)
  inverse_tau <- rep(tau_rule$inverse_tau, each = n_gamma)
  weights <- as.vector(outer(uniform_weight, tau_rule$weights))
  weights <- weights / sum(weights)

  source_standard_error <- model$prior$source$standard_error
  component_variance <- inverse_tau +
    source_standard_error^2 / power_parameter

  list(
    weights = weights,
    means = rep(
      model$prior$source$treatment_effect_estimate,
      length(weights)
    ),
    sds = sqrt(component_variance),
    tau = tau,
    power_parameter = power_parameter
  )
}


#' Quadrature rule for the commensurability parameter
#'
#' @param model A [GaussianCommensuratePowerPrior] object.
#' @param n_nodes Number of nodes.
#' @return A list with `tau`, `inverse_tau`, `log_tau` and `weights`.
#' @keywords internal
commensurate_tau_quadrature <- function(model, n_nodes) {
  prior <- model$prior$method_parameters$heterogeneity_prior

  if (model$heterogeneity_prior_family == "inverse_gamma") {
    alpha <- prior$alpha
    beta <- prior$beta

    # 1 / tau^2 ~ Gamma(alpha, rate = beta). Integrating its quantile
    # representation avoids evaluating the sharply singular density at zero.
    rule <- statmod::gauss.quad(n_nodes, kind = "legendre")
    probability <- (rule$nodes + 1) / 2
    inverse_tau_squared <- stats::qgamma(
      probability,
      shape = alpha,
      rate = beta
    )
    inverse_tau <- sqrt(inverse_tau_squared)
    log_tau <- -0.5 * log(inverse_tau_squared)
    log_limit <- log(.Machine$double.xmax) / 2

    return(list(
      tau = exp(pmin(log_tau, log_limit)),
      inverse_tau = inverse_tau,
      log_tau = log_tau,
      weights = rule$weights / 2
    ))
  }

  if (model$heterogeneity_prior_family == "half_normal") {
    # The quantile representation, as for the other two families. The
    # equivalent Gauss-Laguerre rule on tau^2 / (2 * sigma^2) ~ Gamma(1/2, 1)
    # integrates tau^2 exactly, because tau^2 is linear in the Laguerre
    # variable, but only reaches O(1 / n_nodes) on tau itself, whose square
    # root has an unbounded derivative at the origin. Neither more nodes nor a
    # longer range repairs that: it left the ELIR effective sample size 1.2%
    # out at 48 nodes and still 0.3% out at 384.
    rule <- statmod::gauss.quad(n_nodes, kind = "legendre")
    probability <- (rule$nodes + 1) / 2
    tau <- prior$std_dev * stats::qnorm((1 + probability) / 2)

    return(list(
      tau = tau,
      inverse_tau = 1 / tau,
      log_tau = log(tau),
      weights = rule$weights / 2
    ))
  }

  if (model$heterogeneity_prior_family == "cauchy") {
    rule <- statmod::gauss.quad(n_nodes, kind = "legendre")
    probability <- (rule$nodes + 1) / 2
    log_tau <- stats::qcauchy(
      probability,
      location = prior$location,
      scale = prior$scale
    )

    # The configured scale of 10 reaches beyond the floating-point range at
    # the outer quadrature nodes. Those values are limiting zero/infinite
    # precisions; clipping only their representation keeps the likelihood and
    # normal-mixture calculations finite.
    log_limit <- log(.Machine$double.xmax) / 2

    return(list(
      tau = exp(pmin(log_tau, log_limit)),
      inverse_tau = exp(pmin(-log_tau, log_limit)),
      log_tau = log_tau,
      weights = rule$weights / 2
    ))
  }

  stop(
    "Heterogeneity prior not implemented: ",
    model$heterogeneity_prior_family,
    call. = FALSE
  )
}


#' Summarise borrowing parameters from quadrature weights
#'
#' @param posterior_weights Matrix with one posterior mixture per row.
#' @param mixture Output from [commensurate_prior_mixture()].
#' @param heterogeneity_prior_family Name of the prior family.
#' @param heterogeneity_prior Prior parameters.
#' @return A data frame of posterior means and standard deviations.
#' @keywords internal
commensurate_parameter_summary <- function(posterior_weights, mixture,
                                           heterogeneity_prior_family,
                                           heterogeneity_prior) {
  weighted_summary <- function(values) {
    mean_value <- drop(posterior_weights %*% values)
    second_moment <- drop(posterior_weights %*% values^2)
    list(
      mean = mean_value,
      sd = sqrt(pmax(second_moment - mean_value^2, 0))
    )
  }

  power <- weighted_summary(mixture$power_parameter)
  tau <- weighted_summary(mixture$tau)

  # The target marginal likelihood approaches a positive constant as tau goes
  # to infinity, so it does not repair divergent positive moments in these
  # priors. Report the mathematical moments rather than a finite, sampler-run-
  # dependent truncation of them.
  if (heterogeneity_prior_family == "cauchy") {
    tau$mean[] <- Inf
    tau$sd[] <- Inf
  } else if (heterogeneity_prior_family == "inverse_gamma") {
    if (heterogeneity_prior$alpha <= 0.5) {
      tau$mean[] <- Inf
    }
    if (heterogeneity_prior$alpha <= 1) {
      tau$sd[] <- Inf
    }
  }

  data.frame(
    heterogeneity_parameter_mean = tau$mean,
    heterogeneity_parameter_std = tau$sd,
    power_parameter_mean = power$mean,
    power_parameter_std = power$sd
  )
}
