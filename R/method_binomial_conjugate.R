#' BinomialConjugate class
#'
#' @description Base class for the binomial models whose posterior is available
#' in closed form.
#'
#' Both the separate-analysis and the pooled-analysis models place a uniform
#' prior on the control rate and, conditionally on it, a uniform prior on the
#' treatment effect over `(-control_rate, 1 - control_rate)`. That interval has
#' width 1 whatever the control rate, so the joint prior density is constant and
#' the pair `(control_rate, treatment_rate)` is uniform on the unit square, i.e.
#' independent `Beta(1, 1)` priors on the two arm response rates. The binomial
#' likelihood factorises over arms, so the posterior is a product of two
#' independent Beta distributions:
#'
#' \deqn{p_c \mid D \sim Beta(s_c + 1, n_c - s_c + 1), \quad
#'       p_t \mid D \sim Beta(s_t + 1, n_t - s_t + 1)}
#'
#' and the treatment effect is their difference. Moments are available
#' analytically; the distribution function of the difference is obtained by
#' one-dimensional quadrature and inverted numerically for quantiles, so no
#' Monte Carlo error enters the operating characteristics.
#'
#' @field control_shape1 First shape parameter of the control rate posterior.
#' @field control_shape2 Second shape parameter of the control rate posterior.
#' @field treatment_shape1 First shape parameter of the treatment rate posterior.
#' @field treatment_shape2 Second shape parameter of the treatment rate posterior.
#' @field n_quadrature_nodes Number of nodes used to integrate over the control rate.
#' @field quadrature_control_rates Quadrature nodes on the control rate posterior.
#' @export
BinomialConjugate <- R6::R6Class(
  "BinomialConjugate",
  inherit = Model,
  public = list(
    control_shape1 = NULL,
    control_shape2 = NULL,
    treatment_shape1 = NULL,
    treatment_shape2 = NULL,
    n_quadrature_nodes = 1024L,
    quadrature_control_rates = NULL,

    #' @description Initialize the BinomialConjugate object
    #' @param prior The prior object
    #' @param mcmc_config Unused, kept so that the model factory can build every
    #'   binomial model with the same call.
    initialize = function(prior, mcmc_config = NULL) {
      if (!(prior$method_parameters$initial_prior[[1]] == "noninformative")) {
        stop("Only implemented for a noninformative initial prior")
      }
      super$initialize()

      self$prior <- prior
      self$summary_measure_likelihood <- "binomial"
      self$mcmc <- FALSE
    },

    #' @description Assemble the event counts the posterior conditions on.
    #' Subclasses must implement the 'prepare_data' method.
    #' @param target_data The target data for inference
    prepare_data = function(target_data) {
      stop("Subclasses must implement the 'prepare_data' method.", call. = FALSE)
    },

    #' @description Compute the posterior moments in closed form
    #' @param target_data Target study data
    posterior_moments = function(target_data) {
      counts <- self$prepare_data(target_data)

      self$check_data(counts)

      # A uniform prior on a response rate is Beta(1, 1), so the posterior shape
      # parameters are the successes and failures each incremented by one.
      self$control_shape1 <- counts$n_successes_control + 1
      self$control_shape2 <- counts$n_control - counts$n_successes_control + 1
      self$treatment_shape1 <- counts$n_successes_treatment + 1
      self$treatment_shape2 <- counts$n_treatment - counts$n_successes_treatment + 1

      control_mean <- self$control_shape1 / (self$control_shape1 + self$control_shape2)
      treatment_mean <- self$treatment_shape1 / (self$treatment_shape1 + self$treatment_shape2)

      self$post_mean <- treatment_mean - control_mean
      # The two arms are independent a posteriori, so the variances add.
      self$post_var <- beta_variance(self$treatment_shape1, self$treatment_shape2) +
        beta_variance(self$control_shape1, self$control_shape2)

      # Stratified nodes on the control rate posterior. Integrating a smooth
      # function of the control rate against these is a midpoint rule in
      # probability space, which handles the tails of the Beta without tuning.
      node_probabilities <- (seq_len(self$n_quadrature_nodes) - 0.5) / self$n_quadrature_nodes
      self$quadrature_control_rates <- stats::qbeta(
        node_probabilities,
        self$control_shape1,
        self$control_shape2
      )

      self$post_median <- self$posterior_quantile(0.5)

      invisible(NULL)
    },

    #' @description Posterior CDF of the treatment effect
    #' @param target_treatment_effect Point at which to evaluate the posterior CDF
    posterior_cdf = function(target_treatment_effect) {
      vapply(target_treatment_effect, function(effect) {
        mean(stats::pbeta(
          effect + self$quadrature_control_rates,
          self$treatment_shape1,
          self$treatment_shape2
        ))
      }, numeric(1))
    },

    #' @description Posterior PDF of the treatment effect
    #' @param target_treatment_effect Point at which to evaluate the posterior PDF
    posterior_pdf = function(target_treatment_effect) {
      vapply(target_treatment_effect, function(effect) {
        mean(stats::dbeta(
          effect + self$quadrature_control_rates,
          self$treatment_shape1,
          self$treatment_shape2
        ))
      }, numeric(1))
    },

    #' @description Quantiles of the posterior treatment effect
    #' @param probability Probabilities at which to evaluate the quantile function
    posterior_quantile = function(probability) {
      vapply(probability, function(p) {
        stats::uniroot(
          function(effect) self$posterior_cdf(effect) - p,
          interval = c(-1, 1),
          tol = .Machine$double.eps^0.5
        )$root
      }, numeric(1))
    },

    #' @description Posterior median
    #' @param ... Additional argument
    #' @return The posterior median
    posterior_median = function(...) {
      return(self$post_median)
    },

    #' @description Credible interval on the treatment effect
    #' @param level Level of the credible interval
    credible_interval = function(level = 0.95) {
      alpha <- (1 - level) / 2
      return(self$posterior_quantile(c(alpha, 1 - alpha)))
    },

    #' @description Draw independent samples from the posterior
    #' @param n_samples Number of samples to draw
    sample_posterior = function(n_samples) {
      stats::rbeta(n_samples, self$treatment_shape1, self$treatment_shape2) -
        stats::rbeta(n_samples, self$control_shape1, self$control_shape2)
    },

    #' @description Draw samples from the prior
    #' @param n_samples Number of samples from the prior
    sample_prior = function(n_samples) {
      control_rate <- stats::runif(n = n_samples, min = 0, max = 1)

      target_treatment_effect <- stats::runif(
        n = n_samples,
        min = -control_rate,
        max = 1 - control_rate
      )

      return(target_treatment_effect)
    },

    #' @description Prior PDF
    #' @param target_treatment_effect Point at which to evaluate the prior PDF
    prior_pdf = function(target_treatment_effect) {
      t <- target_treatment_effect

      result <- ifelse(t < -1 | t > 1, 0, ifelse(t >= -1 &
                                                   t <= 0, t + 1, ifelse(t > 0 & t <= 1, 1 - t, 0)))

      return(result)
    },

    #' @description Prior CDF
    #' @param target_treatment_effect Point at which to evaluate the prior CDF
    prior_cdf = function(target_treatment_effect) {
      t <- target_treatment_effect
      result <- ifelse(t < -1, 0, ifelse(t >= -1 &
                                           t <= 0, (t ^ 2) / 2 + t + 1 / 2, ifelse(t > 0 &
                                                                                     t <= 1, t - (t ^ 2) / 2 + 1 / 2, 1)))
      return(result)
    }
  )
)


#' Variance of a Beta distribution
#'
#' @param shape1 First shape parameter.
#' @param shape2 Second shape parameter.
#'
#' @return The variance of the Beta distribution.
#' @noRd
beta_variance <- function(shape1, shape2) {
  total <- shape1 + shape2
  return(shape1 * shape2 / (total ^ 2 * (total + 1)))
}
