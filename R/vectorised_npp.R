#' Discretise the normalised power prior as a normal mixture
#'
#' @description The normalised power prior places a `Beta(p, q)` prior on the
#' power parameter, which makes the prior on the treatment effect a continuous
#' mixture of normals,
#' \deqn{p(\theta) = \int_0^1 N(\theta; \hat\theta_S, \sigma_S^2/\gamma)
#'   \, \mathrm{Beta}(\gamma; p, q) \, d\gamma.}
#' Replacing that integral by a quadrature rule turns the method into an
#' ordinary finite normal mixture, after which the conjugate update, the
#' posterior summaries and the effective sample sizes all follow from the shared
#' normal-mixture code. The nested numerical integration in `posterior_pdf()`
#' and `posterior_cdf()` is then unnecessary.
#'
#' The rule substitutes \eqn{\gamma = u^2} before applying Gauss-Jacobi
#' quadrature. Gauss-Jacobi handles the `Beta` density's endpoint singularities
#' exactly, which matter because the configured settings include shape
#' parameters below one; the substitution removes a square-root term in the
#' remaining factor that would otherwise slow convergence. Together they reach
#' machine precision at a few dozen nodes for every configured setting.
#'
#' The prior does not depend on the replicate, so this is computed once per
#' simulation rather than once per replicate.
#'
#' @param model A `Gaussian_NPP` object.
#' @param n_nodes Number of quadrature nodes.
#' @return A list with `weights`, `means` and `sds` describing the prior
#'   mixture, and `power_parameter`, the quadrature nodes themselves, which are
#'   the power parameter values each component corresponds to.
#' @export
npp_prior_mixture <- function(model, n_nodes = 60L) {
  p <- model$p
  q <- model$q

  rule <- statmod::gauss.quad(n_nodes, kind = "jacobi",
                              alpha = q - 1, beta = 2 * p - 1)

  u <- (1 + rule$nodes) / 2
  power_parameter <- u^2

  # Weight of the Beta density under the substitution gamma = u^2. The constant
  # normalises the Jacobi weight function to the Beta density; it cancels in the
  # rescaling below but is kept so the weights are the Beta measure itself.
  weights <- rule$weights * (1 + u)^(q - 1) *
    2^(1 - q - 2 * p) * 2 / beta(p, q)
  weights <- weights / sum(weights)

  list(
    weights = weights,
    means = rep(model$prior$source$treatment_effect_estimate, n_nodes),
    sds = model$prior$source$standard_error / sqrt(power_parameter),
    power_parameter = power_parameter
  )
}

#' Posterior summaries of the power parameter, per replicate
#'
#' @description Once the prior is discretised, the posterior of the power
#' parameter is the discrete distribution given by the updated mixture weights,
#' so its mean and standard deviation are weighted sums over the quadrature
#' nodes.
#'
#' @param posterior_weights `n_replicates x n_nodes` matrix of posterior mixture
#'   weights.
#' @param power_parameter Quadrature nodes, one per mixture component.
#' @return A data frame with `power_parameter_mean` and `power_parameter_std`.
#' @keywords internal
npp_power_parameter_summary <- function(posterior_weights, power_parameter) {
  mean_value <- drop(posterior_weights %*% power_parameter)
  second_moment <- drop(posterior_weights %*% power_parameter^2)

  data.frame(
    power_parameter_mean = mean_value,
    power_parameter_std = sqrt(pmax(second_moment - mean_value^2, 0))
  )
}
