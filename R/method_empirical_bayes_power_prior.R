#' Find Calibration Parameter
#'
#' @description This function finds the calibration parameter for type I error control in empirical Bayes power prior methods.
#' It is based on the code in Nikolakopoulos et al, 2018, "Dynamic borrowing through adaptive power priors that control type I error". We just renamed the variables to be more explicit.
#' The function finds the calibration parameter by the same bisection search as
#' the original, but evaluates the type I error at each step exactly rather than
#' by simulation. The result is deterministic.
#'
#' @param n_iter Formerly the number of simulated target estimates used to
#'   estimate the type I error. The type I error is now integrated exactly by
#'   [adaptive_power_prior_type_I_error()], so this argument is ignored. It is
#'   retained because existing method configurations still supply it.
#' @param source_sample_size_per_arm Sample size per arm in the source study
#' @param target_sample_size_per_arm Sample size per arm in the target study
#' @param source_treatment_effect_estimate Treatment effect estimate in the source study
#' @param desired_tie Desired type I error rate
#' @param significance_level Significance level for hypothesis testing
#' @param target_data_sampling_variance Sampling variance of the target study data
#' @param source_data_sampling_variance Sampling variance of the source study data
#' @param tolerance Tolerance for convergence of the estimation
#' @param theta_0 True mean for type I error computation
#' @return A matrix containing the calibration parameter and other related values
#' @export
findCalibrationParameter <- function(n_iter = 1e6,
                                     source_sample_size_per_arm,
                                     target_sample_size_per_arm,
                                     source_treatment_effect_estimate,
                                     desired_tie = 0.065,
                                     significance_level = 0.05,
                                     target_data_sampling_variance,
                                     source_data_sampling_variance,
                                     tolerance = 0.0001,
                                     theta_0 = 0) {
  true_mean <- theta_0 # True mean is theta_0 for type I error computation

  # Prior sample size, equation (6) of Nikolakopoulos et al (2018). It is the
  # source's sample size rescaled to units of target observations, so that
  # target_data_sampling_variance / n0 is the source's squared standard error.
  n0 <- source_sample_size_per_arm * target_data_sampling_variance / source_data_sampling_variance

  std_predictive_dist <- sqrt(
    target_data_sampling_variance / n0 + target_data_sampling_variance / target_sample_size_per_arm
  )

  maxZ_1_m_c2 <- -(((
    -(
      sqrt(target_data_sampling_variance) * sqrt(n0 + target_sample_size_per_arm) * qnorm(significance_level) + n0 * source_treatment_effect_estimate
    )
  ) / (
    target_sample_size_per_arm * std_predictive_dist
  )) - source_treatment_effect_estimate / std_predictive_dist)

  min_prior_mean <- -(
    sqrt(target_data_sampling_variance) * sqrt(n0 + target_sample_size_per_arm) / (n0 + target_sample_size_per_arm)
  ) * qnorm(significance_level) # significance level was hard coded as 0.05 here in the original code

  # Type I error attained by a given calibration parameter. This used to be
  # estimated from n_iter simulated target estimates; it is now integrated
  # exactly, which is both faster and free of the Monte Carlo noise that the
  # bisection below could not resolve (its tolerance is finer than the standard
  # error of a 1e6-draw estimate).
  attained_type_I_error <- function(calibration_parameter) {
    adaptive_power_prior_type_I_error(
      calibration_parameter = calibration_parameter,
      source_sample_size_per_arm = source_sample_size_per_arm,
      target_sample_size_per_arm = target_sample_size_per_arm,
      source_treatment_effect_estimate = source_treatment_effect_estimate,
      significance_level = significance_level,
      target_data_sampling_variance = target_data_sampling_variance,
      source_data_sampling_variance = source_data_sampling_variance,
      theta_0 = true_mean
    )
  }

  upper_limit <- min(maxZ_1_m_c2, 1)
  lower_limit <- 0
  Z_1_m_c2 <- min(maxZ_1_m_c2, 1) # Z_1-c/2 : as defined in the manuscript, calibration parameter for type I error control
  # Type I error of full borrowing: equation (5) of Nikolakopoulos et al (2018),
  #   Phi((sigma * sqrt(n0 + n1) * z_{1-eta} + n0 * mu_0) / (sqrt(n1) * sigma)).
  # Named because the search below compares against it in two places, and the
  # reference script wrote the second one without its leading sigma. That is
  # harmless there, where sigma^2 is fixed at 1, but not here.
  full_borrowing_type_I_error <- pnorm((
    sqrt(target_data_sampling_variance) * sqrt(n0 + target_sample_size_per_arm) * qnorm(significance_level) + n0 * source_treatment_effect_estimate
  ) / sqrt(target_data_sampling_variance * target_sample_size_per_arm)
  )
  t1 <- ex.t1 <- full_borrowing_type_I_error
  t2 <- 0
  if (t1 < desired_tie) {
    Z_1_m_c2 <- maxZ_1_m_c2
  } else {
    while ((abs(t1 - t2) > tolerance) &
           (abs(upper_limit - lower_limit) > tolerance)) {
      if (t2 != 0) {
        t1 <- t2
      }
      while ((t1 > desired_tie) &
             (abs(upper_limit - lower_limit) > tolerance)) {
        upper_limit <- Z_1_m_c2
        Z_1_m_c2 <- mean(c(lower_limit, upper_limit))
        t1 <- attained_type_I_error(Z_1_m_c2)

        if (is.na(t1)) {
          stop("t1 is NA")
        }
      }

      if (t1 < full_borrowing_type_I_error) {
        t2 <- t1
      }
      while ((t2 < desired_tie) &
             (abs(upper_limit - lower_limit) > tolerance)) {
        lower_limit <- Z_1_m_c2
        Z_1_m_c2 <- mean(c(lower_limit, upper_limit))
        t2 <- attained_type_I_error(Z_1_m_c2)
      }
    }
  }

  values <- c(Z_1_m_c2,
              std_predictive_dist,
              maxZ_1_m_c2,
              min_prior_mean,
              ex.t1)
  labels <- c("Z_1-c/2",
              "sigma pred",
              "maximum Z_1-c/2",
              "min_prior_mean",
              "type I er with A =1")

  return(cbind(labels, values))
}


#' Gaussian_empirical_Bayes_PP class
#'
#' @description This is a parent class for variants of empirical Bayes PP methods for normally distributed summary measure of the treatment effect.
#'
#' @format R6Class object.
#' @field power_parameter The power parameter.
#' @field summary_measure_likelihood The summary measure distribution.
#' @field null_space Null hypothesis space.
#' @field empirical_bayes Boolean indicating if empirical Bayes is used.
#' @export
Gaussian_empirical_Bayes_PP <- R6::R6Class(
  classname = "Gaussian_empirical_Bayes_PP",
  inherit = StaticBorrowingGaussian,
  public = list(
    power_parameter = NULL,
    summary_measure_likelihood = NULL,
    null_space = NULL,
    empirical_bayes = TRUE,

    #' @description Initialize the Gaussian_empirical_Bayes_PP object.
    #'
    #' @param prior The prior object.
    #' @param null_space Null space
    #' @param theta_0 The theta_0 value.
    #' @return NULL
    initialize = function(prior, null_space, theta_0) {
      super$initialize(prior = prior)
      self$summary_measure_likelihood <- "normal"
      self$null_space <- null_space
      self$parameters <- list(theta_0 = theta_0)
      self$prior_var <- NULL
    },

    #' @description Transform the hypothesis space.
    #' @param target_data The target data.
    #' @return A list containing transformed treatment effect estimates.
    hypothesis_space_transformation = function(target_data) {
      # To handle the cases where null_space == "right", we apply the following transformations to put ourselves back in a situation equivalent to null_space == "left":

      assertions::assert_number(self$parameters$theta_0)

      if (self$null_space == "right") {
        source_treatment_effect_estimate <- -self$prior$source$treatment_effect_estimate
        target_treatment_effect_estimate <- -target_data$sample$treatment_effect_estimate
        # Negate locally. Assigning back to self$parameters$theta_0 flipped its
        # sign on every call, so consecutive replicates were transformed
        # differently.
        theta_0 <- -self$parameters$theta_0
      } else if (self$null_space == "left") {
        source_treatment_effect_estimate <- self$prior$source$treatment_effect_estimate
        target_treatment_effect_estimate <- target_data$sample$treatment_effect_estimate
        theta_0 <- self$parameters$theta_0
      } else {
        stop("The null space must be either on the left side or on the right side of theta_0")
      }
      # To handle the cases where theta_0 != 0, we apply the following translations to put ourselves back in a situation equivalent to theta_0 == 0
      source_treatment_effect_estimate <- source_treatment_effect_estimate - theta_0
      target_treatment_effect_estimate <- target_treatment_effect_estimate - theta_0

      return(
        list(
          source_treatment_effect_estimate = source_treatment_effect_estimate,
          target_treatment_effect_estimate = target_treatment_effect_estimate
        )
      )
    },

    #' @description Empirical Bayes update
    #' @param target_data Target study data
    #' @return NULL
    empirical_bayes_update = function(target_data) {
      if (target_data$sample$treatment_effect_standard_error == Inf) {
        stop("The standard error on the treatment effect is Inf")
      }

      self$posterior_parameters$power_parameter <- self$power_parameter_estimation(target_data = target_data)

      self$prior_var <- (self$prior$source$standard_error ^ 2) / self$posterior_parameters$power_parameter

      if (self$posterior_parameters$power_parameter != 0) {
        self$prior_var <- (self$prior$source$standard_error ^ 2) / self$posterior_parameters$power_parameter # In the Gaussian case, the power prior is equivalent to having a Gaussian prior with variance prior_var
      } else {
        self$prior_var <- 1000 # Vague prior
      }

      if (is.numeric(self$prior_var) &&
          length(self$prior_var) == 0) {
        stop("self$prior_var is numeric(0)")
      }
    },

    #' Perform inference using the Gaussian_empirical_Bayes_PP method.
    #'
    #' @param target_data The target data.
    #' @return The inference result.
    inference = function(target_data) {
      self$empirical_bayes_update(target_data)

      return(super$inference(target_data))
    },

    #' Estimate the power parameter.
    #'
    #' @return The estimated power parameter.
    power_parameter_estimation = function() {
      stop("Subclass must implement a power_parameter estimation method.")
    },

    #' @description Estimate the power parameter for every replicate at once
    #'
    #' Subclasses whose estimator is closed form override this. Returning `NULL`
    #' means "no fast path", which keeps subclasses with an iterative estimator
    #' (PDCCPP calibrates by search) on the replicate loop.
    #'
    #' @param target_data Target study data.
    #' @param samples Data frame of generated replicates.
    #' @return A vector of power parameters, or `NULL`.
    vectorised_power_parameter = function(target_data, samples) {
      NULL
    },

    #' @description Transform the hypothesis space for every replicate at once
    #'
    #' Vectorised counterpart of `hypothesis_space_transformation()`.
    #'
    #' @param samples Data frame of generated replicates.
    #' @return A list of transformed source and target treatment effect
    #'   estimates.
    vectorised_hypothesis_space_transformation = function(samples) {
      if (self$null_space == "right") {
        source_estimate <- -self$prior$source$treatment_effect_estimate
        target_estimate <- -samples$treatment_effect_estimate
        theta_0 <- -self$parameters$theta_0
      } else if (self$null_space == "left") {
        source_estimate <- self$prior$source$treatment_effect_estimate
        target_estimate <- samples$treatment_effect_estimate
        theta_0 <- self$parameters$theta_0
      } else {
        stop("The null space must be either on the left side or on the right side of theta_0")
      }

      list(
        source_treatment_effect_estimate = source_estimate - theta_0,
        target_treatment_effect_estimate = target_estimate - theta_0
      )
    },

    #' @description Prior variance for each replicate
    #'
    #' Mirrors `empirical_bayes_update()`: the power prior is equivalent to a
    #' Gaussian prior with variance `source standard error^2 / power parameter`,
    #' and a power parameter of zero means a vague prior.
    #'
    #' @param target_data Target study data.
    #' @param samples Data frame of generated replicates.
    #' @return A vector of prior variances, or `NULL`.
    vectorised_prior_variance = function(target_data, samples) {
      power_parameter <- self$vectorised_power_parameter(target_data, samples)
      if (is.null(power_parameter)) {
        return(NULL)
      }

      # Cached so that vectorised_posterior_parameters() reports the same values
      # without estimating them a second time.
      private$last_power_parameter <- power_parameter

      ifelse(
        power_parameter != 0,
        self$prior$source$standard_error^2 / power_parameter,
        1000
      )
    },

    #' @description Posterior parameters reported by the vectorised path
    #' @param prior_variance Per-replicate prior variance.
    #' @return A data frame with one `power_parameter` column.
    vectorised_posterior_parameters = function(prior_variance) {
      data.frame(power_parameter = private$last_power_parameter)
    },

    #' @description
    #' Calculate the prior probability density function (PDF) for a given target treatment effect.
    #' @param target_treatment_effect The target treatment effect.
    #' @return The prior PDF.
    prior_pdf = function(target_treatment_effect) {
      if (is.null(self$prior_var)) {
        stop(
          "Prior variance is not defined. Run model$empirical_bayes_update(target_data) befor calling model$prior_pdf()."
        )
      }
      return(dnorm(
        target_treatment_effect,
        mean = self$prior_mean,
        sd = sqrt(self$prior_var)
      ))
    },

    #' @description
    #' Plot the power parameter as a function of drift in treatment effect
    #' @param source_treatment_effect_estimate Treatment effect estimate in the source study
    #' @param target_data Target study data
    #' @param min_drift Minimum drift value
    #' @param max_drift Maximum drift value
    #' @param resolution Number of points on the drift grid.
    #' @return The prior PDF.
    plot_power_parameter_vs_drift = function(source_treatment_effect_estimate,
                                             target_data,
                                             min_drift,
                                             max_drift,
                                             resolution) {
      # Generate a sequence of differences
      drifts <- seq(min_drift, max_drift, length.out = resolution)

      # Calculate power parameters for each difference
      power_parameters <- sapply(drifts, function(drift) {
        target_data_drift <- duplicate(target_data)
        target_data_drift$sample$treatment_effect_estimate <- source_treatment_effect_estimate + drift
        self$power_parameter_estimation(target_data)
      })


      # Create a data frame for plotting
      plot_data <- data.frame(Drift = drifts, PowerParameter = power_parameters)

      # Plot using ggplot2
      ggplot(plot_data, aes(x = Drift, y = PowerParameter)) +
        geom_line(color = "blue") +
        labs(title = "Power Parameter as a Function of Drift", x = "Drift", y = "Power Parameter") +
        theme_minimal()
    }
  ),
  private = list(
    # Power parameters from the most recent vectorised_prior_variance() call,
    # so that vectorised_posterior_parameters() can report them unchanged.
    last_power_parameter = NULL
  )
)


#' Gaussian_Gravestock_EBPP class
#'
#' @description This class inherits from Gaussian_empirical_Bayes_PP and implements the Gravestock's EBPP method.
#'
#' @format R6Class object.
#' @field method Method name
#' @export
Gaussian_Gravestock_EBPP <- R6::R6Class(
  classname = " Gaussian_gravestock_EBPP",
  inherit = Gaussian_empirical_Bayes_PP,
  public = list(
    method = "EBPP",
    #' @description Initialize the Gaussian_Gravestock_EBPP object.
    #'
    #' @param prior The prior object.
    #' @param null_space Side of the null hypothesis space
    #' @param theta_0 Boundary of the null hypothesis space
    #' @return NULL
    initialize = function(prior, null_space, theta_0) {
      super$initialize(prior = prior,
                       null_space = null_space,
                       theta_0 = theta_0)
    },

    #' @description Estimate the power parameter using the Gravestock's EBPP method.
    #'
    #' @param target_data The target data.
    #' @param source_treatment_effect_estimate Treatment effect estimate in the source study
    #' @param target_treatment_effect_estimate Treatment effect estimate in the target study
    #' @return The estimated power parameter.
    power_parameter_estimation = function(target_data) {
      # We reuse the code from the PDCCPP, but leverage the fact that PDCCPP is equivalent to Gravestock's EBPP with the calibration parameter set as:

      transformed_treatment_effects <- self$hypothesis_space_transformation(target_data = target_data)
      source_treatment_effect_estimate <- transformed_treatment_effects$source_treatment_effect_estimate
      target_treatment_effect_estimate <- transformed_treatment_effects$target_treatment_effect_estimate


      calibration_parameter <- 2 * (1 - pnorm(1)) # See Nikolakopoulos et al, 2018

      target_data_sampling_variance <- target_data$sample$treatment_effect_standard_error ^
        2 * target_data$sample_size_per_arm

      # We compute the sampling variance for the corresponding Gaussian, the goal is to put ourselves in a case equivalent to sampling from a Gaussian (single arm)
      source_data_sampling_variance <- self$prior$source$standard_error ^
        2 * self$prior$source$equivalent_source_sample_size_per_arm

      # Prior sample size, equation (6) of Nikolakopoulos et al (2018): the
      # source's sample size rescaled to units of target observations, so that
      # target_data_sampling_variance / n0 is the source's squared standard
      # error. Equation (9) of that paper is this estimator at z_{1-c/2} = 1,
      # so it takes the same n0 as PDCCPP.
      n0 <- self$prior$source$equivalent_source_sample_size_per_arm *
        target_data_sampling_variance / source_data_sampling_variance

      standard_deviation_predictive <- sqrt(
        target_data_sampling_variance / n0 + target_data_sampling_variance / target_data$sample_size_per_arm
      )

      power_parameter <- ifelse(((
        target_treatment_effect_estimate > (
          source_treatment_effect_estimate + standard_deviation_predictive * qnorm(1 - calibration_parameter / 2)
        )
      )) |
        ((
          target_treatment_effect_estimate < (
            source_treatment_effect_estimate + standard_deviation_predictive * qnorm(calibration_parameter / 2)
          )
        )), ((target_data_sampling_variance / n0) / (((
          target_treatment_effect_estimate - source_treatment_effect_estimate
        ) / qnorm(1 - calibration_parameter / 2)
        ) ^ 2 - target_data_sampling_variance / target_data$sample_size_per_arm
        )
        ), 1)
      assertions::assert_number(power_parameter)
      return(power_parameter)
    },

    #' @description Estimate the power parameter for every replicate at once.
    #'
    #' Same closed form as `power_parameter_estimation()`, evaluated on vectors.
    #'
    #' @param target_data The target data.
    #' @param samples Data frame of generated replicates.
    #' @return A vector of power parameters.
    vectorised_power_parameter = function(target_data, samples) {
      transformed <- self$vectorised_hypothesis_space_transformation(samples)
      source_estimate <- transformed$source_treatment_effect_estimate
      target_estimate <- transformed$target_treatment_effect_estimate

      calibration_parameter <- 2 * (1 - pnorm(1)) # See Nikolakopoulos et al, 2018

      target_sampling_variance <- samples$treatment_effect_standard_error^2 *
        target_data$sample_size_per_arm
      source_sampling_variance <- self$prior$source$standard_error^2 *
        self$prior$source$equivalent_source_sample_size_per_arm

      n0 <- self$prior$source$equivalent_source_sample_size_per_arm *
        target_sampling_variance / source_sampling_variance

      standard_deviation_predictive <- sqrt(
        target_sampling_variance / n0 +
          target_sampling_variance / target_data$sample_size_per_arm
      )

      upper <- source_estimate +
        standard_deviation_predictive * qnorm(1 - calibration_parameter / 2)
      lower <- source_estimate +
        standard_deviation_predictive * qnorm(calibration_parameter / 2)

      ifelse(
        target_estimate > upper | target_estimate < lower,
        (target_sampling_variance / n0) /
          (((target_estimate - source_estimate) / qnorm(1 - calibration_parameter / 2))^2 -
             target_sampling_variance / target_data$sample_size_per_arm),
        1
      )
    }
  )
)

#' PDCCPP class
#'
#' @description This class inherits from Gaussian_empirical_Bayes_PP and implements the PDCCPP method.
#'
#' @format R6Class object.
#' @field null_space Side of the null hypothesis space
#' @field method Method name
#' @export
PDCCPP <- R6::R6Class(
  classname = "PDCCPP",
  inherit = Gaussian_empirical_Bayes_PP,
  public = list(
    null_space = NULL,
    method = "PDCCPP",
    #' @description Initialize the PDCCPP object.
    #'
    #' @param prior The prior object.
    #' @param theta_0 The theta_0 value.
    #' @param null_space Null space
    #' @return NULL
    initialize = function(prior, theta_0, null_space) {
      super$initialize(prior = prior,
                       theta_0 = theta_0,
                       null_space = null_space)
      self$parameters <- c(
        self$parameters,
        list(
          desired_tie = prior$method_parameters$desired_tie[[1]],
          significance_level = prior$method_parameters$significance_level[[1]],
          tolerance = prior$method_parameters$tolerance[[1]],
          n_iter = prior$method_parameters$n_iter[[1]]
        )
      )

      self$null_space <- null_space
    },

    #' Estimate the power parameter using the PDCCPP method.
    #'
    #' @param target_data The target data.
    #' @param source_treatment_effect_estimate The source treatment effect estimate.
    #' @param target_treatment_effect_estimate The target treatment effect estimate.
    #' @return The estimated power parameter.
    power_parameter_estimation = function(target_data) {
      transformed_treatment_effects <- self$hypothesis_space_transformation(target_data = target_data)
      source_treatment_effect_estimate <- transformed_treatment_effects$source_treatment_effect_estimate
      target_treatment_effect_estimate <- transformed_treatment_effects$target_treatment_effect_estimate


      target_data_sampling_variance <- target_data$sample$treatment_effect_standard_error ^
        2 * target_data$sample_size_per_arm

      equivalent_source_sample_size_per_arm <- self$prior$source$equivalent_source_sample_size_per_arm

      source_data_sampling_variance <- equivalent_source_sample_size_per_arm * self$prior$source$standard_error ^ 2

      # Equation (5) is written in terms of z_{1-eta}, where eta is the
      # posterior probability threshold the analysis decides at. Calibrating
      # against any other threshold tunes a different test than the one that is
      # run. The configured significance_level is the fallback for callers that
      # invoke the estimator outside a simulation.
      significance_level <- if (is.null(self$analysis_critical_value)) {
        self$parameters$significance_level
      } else {
        1 - self$analysis_critical_value
      }

      calibration <- findCalibrationParameter(
        n_iter = self$parameters$n_iter,
        source_sample_size_per_arm = equivalent_source_sample_size_per_arm,
        target_sample_size_per_arm = target_data$sample_size_per_arm,
        source_treatment_effect_estimate = source_treatment_effect_estimate,
        desired_tie = self$parameters$desired_tie,
        significance_level = significance_level,
        target_data_sampling_variance = target_data_sampling_variance,
        source_data_sampling_variance = source_data_sampling_variance,
        tolerance = self$parameters$tolerance,
        theta_0 = self$parameters$theta_0
      )

      calibration_parameter <- as.numeric(calibration[1, 2])
      assertions::assert_number(calibration_parameter)

      if (calibration_parameter > 1 || calibration_parameter < 0) {
        stop("Calibration parameter must be in [0,1].")
      }
      # Same prior sample size the calibration used, so that the cut-off is
      # measured against the same predictive standard deviation.
      n0 <- equivalent_source_sample_size_per_arm * target_data_sampling_variance / source_data_sampling_variance

      standard_deviation_predictive <- sqrt(
        target_data_sampling_variance / n0 + target_data_sampling_variance / target_data$sample_size_per_arm
      )

      # findCalibrationParameter reports z_{1-c/2}, which equation (9) of
      # Nikolakopoulos et al (2018) uses as the number of predictive standard
      # deviations that X-bar may deviate from mu_0 before borrowing is
      # discounted. The formula below is shared with Gaussian_Gravestock_EBPP,
      # which parameterises the same cut-off by the tail probability c and
      # recovers z_{1-c/2} as qnorm(1 - c / 2). Sending a z-score through that
      # conversion a second time applied a wider cut-off than the one the
      # search had just calibrated.
      power_parameter <- ifelse(((
        target_treatment_effect_estimate > (
          source_treatment_effect_estimate + standard_deviation_predictive * calibration_parameter
        )
      )) |
        ((
          target_treatment_effect_estimate < (
            source_treatment_effect_estimate - standard_deviation_predictive * calibration_parameter
          )
        )), ((target_data_sampling_variance / n0) / (((
          target_treatment_effect_estimate - source_treatment_effect_estimate
        ) / calibration_parameter
        ) ^ 2 - target_data_sampling_variance / target_data$sample_size_per_arm
        )
        ), 1)

      if (is.null(power_parameter) || is.na(power_parameter)) {
        stop("Variable is NULL or NA. Execution stopped.")
      }

      return(power_parameter)
    }
  )
)


#' p_value_based_PP_Gaussian class
#'
#' @description This class represents a p-value based power prior method.
#' It inherits from the Gaussian_empirical_Bayes_PP class.
#'
#' @field shape_parameter The shape parameter for the method.
#' @field equivalence_margin The equivalence margin for the method.
#' @field method Method name
#' @export
p_value_based_PP_Gaussian <- R6::R6Class(
  classname = "p_value_based_PP",
  inherit = Gaussian_empirical_Bayes_PP,
  public = list(
    shape_parameter = NULL,
    equivalence_margin = NULL,
    method = "p_value_based_PP",
    #' @description Initialize the p_value_based_PP object.
    #'
    #' @param prior The prior object.
    #' @param theta_0 Boundary of the null hypothesis space
    #' @param null_space Null space.
    #' @return None
    initialize = function(prior, theta_0, null_space) {
      super$initialize(prior = prior,
                       theta_0 = theta_0,
                       null_space = null_space)
      self$parameters <- c(
        self$parameters,
        list(
          shape_parameter = prior$method_parameters$shape_parameter[[1]],
          equivalence_margin = prior$method_parameters$equivalence_margin[[1]]
        )
      )
    },

    #' Test method
    #'
    #' @description This method performs the test for the given target data.
    #'
    #' @param target_data The target data object.
    #' @param target_treatment_effect_estimate The target treatment effect estimate.
    #' @param source_treatment_effect_estimate The source treatment effect estimate.
    #' @param test_type Type of frequentist test used
    #' @return The p-value.
    test = function(target_data,
                    source_treatment_effect_estimate,
                    target_treatment_effect_estimate,
                    test_type = "t-test") {
      if (test_type == "z-test") {
        # H0a : θS - θT > λ
        left <- BSDA::zsum.test(
          mean.x = source_treatment_effect_estimate,
          mean.y = target_treatment_effect_estimate,
          mu = self$parameters$equivalence_margin,
          alternative = "less",
          sigma.x = self$prior$source$standard_error * sqrt(
            self$prior$source$equivalent_source_sample_size_per_arm
          ),
          sigma.y = target_data$sample$standard_deviation,
          n.x = self$prior$source$equivalent_source_sample_size_per_arm,
          n.y = target_data$sample_size_per_arm
        )

        # H0b : θS - θT < -λ
        right <- BSDA::zsum.test(
          mean.x = source_treatment_effect_estimate,
          mean.y = target_treatment_effect_estimate,
          mu = -self$parameters$equivalence_margin,
          alternative = "greater",
          sigma.x = self$prior$source$standard_error * sqrt(
            self$prior$source$equivalent_source_sample_size_per_arm
          ),
          sigma.y = target_data$sample$standard_deviation,
          n.x = self$prior$source$equivalent_source_sample_size_per_arm,
          n.y = target_data$sample_size_per_arm
        )
      } else if (test_type == "t-test") {
        # H0a : θS - θT > λ
        left <- BSDA::tsum.test(
          mean.x = source_treatment_effect_estimate,
          mean.y = target_treatment_effect_estimate,
          mu = self$parameters$equivalence_margin,
          alternative = "less",
          s.x = self$prior$source$standard_error * sqrt(
            self$prior$source$equivalent_source_sample_size_per_arm
          ),
          s.y = target_data$sample$standard_deviation,
          n.x = self$prior$source$equivalent_source_sample_size_per_arm,
          n.y = target_data$sample_size_per_arm
        )

        # H0b : θS - θT < -λ
        right <- BSDA::tsum.test(
          mean.x = source_treatment_effect_estimate,
          mean.y = target_treatment_effect_estimate,
          mu = -self$parameters$equivalence_margin,
          alternative = "greater",
          s.x = self$prior$source$standard_error * sqrt(
            self$prior$source$equivalent_source_sample_size_per_arm
          ),
          s.y = target_data$sample$standard_deviation,
          n.x = self$prior$source$equivalent_source_sample_size_per_arm,
          n.y = target_data$sample_size_per_arm
        )
      } else {
        stop("Not implemented for this test. Must be either 't-test' or 'z-test'.")
      }

      # Take the maximum p_value of the two tests
      p_value <- pmax(left$p.value, right$p.value)


      assertions::assert_number(p_value)
      return(p_value)
    },

    #' Power parameter estimation method
    #'
    #' @description This method estimates the power parameter for the given target data.
    #'
    #' @param target_data The target data object.
    #' @return The power parameter.
    power_parameter_estimation = function(target_data) {
      transformed_treatment_effects <- self$hypothesis_space_transformation(target_data = target_data)
      source_treatment_effect_estimate <- transformed_treatment_effects$source_treatment_effect_estimate
      target_treatment_effect_estimate <- transformed_treatment_effects$target_treatment_effect_estimate

      p_value <- self$test(
        target_data = target_data,
        target_treatment_effect_estimate = target_treatment_effect_estimate,
        source_treatment_effect_estimate = source_treatment_effect_estimate
      )
      power_parameter <- exp((self$parameters$shape_parameter / (1 - p_value)) * log(1 - p_value))
      if (is.null(power_parameter) || is.na(power_parameter)) {
        stop("Variable is NULL or NA. Execution stopped.")
      }
      return(power_parameter)
    },

    #' @description Estimate the power parameter for every replicate at once.
    #'
    #' Reproduces `test()` followed by `power_parameter_estimation()`. The two
    #' one-sided equivalence tests are the same summary-statistic t-tests that
    #' `test()` runs through BSDA, evaluated on vectors.
    #'
    #' @param target_data The target data.
    #' @param samples Data frame of generated replicates.
    #' @return A vector of power parameters.
    vectorised_power_parameter = function(target_data, samples) {
      transformed <- self$vectorised_hypothesis_space_transformation(samples)

      source_sd <- self$prior$source$standard_error *
        sqrt(self$prior$source$equivalent_source_sample_size_per_arm)
      source_n <- self$prior$source$equivalent_source_sample_size_per_arm
      margin <- self$parameters$equivalence_margin

      one_sided <- function(mu, alternative) {
        summary_t_test_p_value(
          mean_x = transformed$source_treatment_effect_estimate,
          sd_x = source_sd,
          n_x = source_n,
          mean_y = transformed$target_treatment_effect_estimate,
          sd_y = samples$standard_deviation,
          n_y = target_data$sample_size_per_arm,
          mu = mu,
          alternative = alternative
        )
      }

      # H0a: theta_S - theta_T > margin, and H0b: theta_S - theta_T < -margin.
      p_value <- pmax(one_sided(margin, "less"), one_sided(-margin, "greater"))

      exp((self$parameters$shape_parameter / (1 - p_value)) * log(1 - p_value))
    }
  )
)




#' Gaussian_empirical_Bayes_PP class
#'
#' @description This is a parent class for variants of empirical Bayes PP methods for normally distributed summary measure of the treatment effect.
#'
#' @format R6Class object.
#' @field power_parameter The power parameter.
#' @field summary_measure_likelihood The summary measure distribution.
#' @field null_space Null hypothesis space.
#' @field empirical_bayes Boolean indicating if empirical Bayes is used.
#' @field shape_parameter Shape parameter
#' @field equivalence_margin Equivalence margin for the test
#' @field method Method name
#' @field prior_var Prior variance
#' @field mcmc_config MCMC configuration
#' @export
p_value_based_PP_Binomial <- R6::R6Class(
  classname = "p_value_based_PP_Binomial",
  inherit = BinomialCPP,
  public = list(
    power_parameter = NULL,
    summary_measure_likelihood = NULL,
    null_space = NULL,
    empirical_bayes = TRUE,
    shape_parameter = NULL,
    equivalence_margin = NULL,
    method = "p_value_based_PP",
    prior_var = NULL,
    mcmc_config = NULL,
    #' @description Initialize the p_value_based_PP object.
    #'
    #' @param prior The prior object.
    #' @param theta_0 Boundary of the null hypothesis space
    #' @param null_space Null space.
    #' @param mcmc_config MCMC configuration.
    #' @return None
    initialize = function(prior, theta_0, null_space, mcmc_config) {
      super$initialize(prior = prior, mcmc_config = mcmc_config)
      self$parameters <- c(
        self$parameters,
        list(
          shape_parameter = prior$method_parameters$shape_parameter[[1]],
          equivalence_margin = prior$method_parameters$equivalence_margin[[1]],
          theta_0 = theta_0
        )
      )
      self$summary_measure_likelihood <- "binomial"
      self$null_space <- null_space
    },

    #' @description Transform the hypothesis space.
    #' @param target_data The target data.
    #' @return A list containing transformed treatment effect estimates.
    hypothesis_space_transformation = function(target_data) {
      # To handle the cases where null_space == "right", we apply the following transformations to put ourselves back in a situation equivalent to null_space == "left":

      assertions::assert_number(self$parameters$theta_0)

      if (self$null_space == "right") {
        source_treatment_effect_estimate <- -self$prior$source$treatment_effect_estimate
        target_treatment_effect_estimate <- -target_data$sample$treatment_effect_estimate
        # Negate locally. Assigning back to self$parameters$theta_0 flipped its
        # sign on every call, so consecutive replicates were transformed
        # differently.
        theta_0 <- -self$parameters$theta_0
      } else if (self$null_space == "left") {
        source_treatment_effect_estimate <- self$prior$source$treatment_effect_estimate
        target_treatment_effect_estimate <- target_data$sample$treatment_effect_estimate
        theta_0 <- self$parameters$theta_0
      } else {
        stop("The null space must be either on the left side or on the right side of theta_0")
      }
      # To handle the cases where theta_0 != 0, we apply the following translations to put ourselves back in a situation equivalent to theta_0 == 0
      source_treatment_effect_estimate <- source_treatment_effect_estimate - theta_0
      target_treatment_effect_estimate <- target_treatment_effect_estimate - theta_0

      return(
        list(
          source_treatment_effect_estimate = source_treatment_effect_estimate,
          target_treatment_effect_estimate = target_treatment_effect_estimate
        )
      )
    },

    #' @description Empirical Bayes update
    #' @param target_data Target study data
    #' @return NULL
    empirical_bayes_update = function(target_data) {
      if (target_data$sample$treatment_effect_standard_error == Inf) {
        stop("The standard error on the treatment effect is Inf")
      }

      self$posterior_parameters$power_parameter <- self$power_parameter_estimation(target_data = target_data)
      self$power_parameter <- self$posterior_parameters$power_parameter
    },

    #' Perform inference using the Gaussian_empirical_Bayes_PP method.
    #'
    #' @param target_data The target data.
    #' @return The inference result.
    inference = function(target_data) {
      self$empirical_bayes_update(target_data)

      return(super$inference(target_data))
    },

    #' Test method
    #'
    #' @description This method performs the test for the given target data.
    #'
    #' @param target_data The target data object.
    #' @param target_treatment_effect_estimate The target treatment effect estimate.
    #' @param source_treatment_effect_estimate The source treatment effect estimate.
    #' @return The p-value.
    test = function(target_data,
                    source_treatment_effect_estimate,
                    target_treatment_effect_estimate) {
      # # Standard error of the difference between the means:
      # se <- sqrt(
      #   target_data$sample$treatment_effect_standard_error ^ 2 + self$prior$source$standard_error ^
      #     2
      # )
      #
      # # H0a : θS - θT > λ
      # z_left <- (
      #   source_treatment_effect_estimate - target_treatment_effect_estimate - self$parameters$equivalence_margin
      # ) / se
      # p_left <- pnorm(z_left, lower.tail = TRUE)
      #
      # # H0b : θS - θT < -λ
      # z_right <- (
      #    source_treatment_effect_estimate - target_treatment_effect_estimate + self$parameters$equivalence_margin
      # ) / se
      # p_right <- pnorm(z_right, lower.tail = FALSE)
      #
      # p_value <- pmax(p_left, p_right)

      # H0a : θS - θT > λ
      left <- BSDA::zsum.test(
        mean.x = source_treatment_effect_estimate,
        mean.y = target_treatment_effect_estimate,
        mu = self$parameters$equivalence_margin,
        alternative = "less",
        sigma.x = self$prior$source$standard_error * sqrt(self$prior$source$equivalent_source_sample_size_per_arm),
        sigma.y = target_data$sample$standard_deviation,
        n.x = self$prior$source$equivalent_source_sample_size_per_arm,
        n.y = target_data$sample_size_per_arm
      )

      # H0b : θS - θT < -λ
      right <- BSDA::zsum.test(
        mean.x = source_treatment_effect_estimate,
        mean.y = target_treatment_effect_estimate,
        mu = -self$parameters$equivalence_margin,
        alternative = "greater",
        sigma.x = self$prior$source$standard_error * sqrt(self$prior$source$equivalent_source_sample_size_per_arm),
        sigma.y = target_data$sample$standard_deviation,
        n.x = self$prior$source$equivalent_source_sample_size_per_arm,
        n.y = target_data$sample_size_per_arm
      )

      # Take the maximum p_value of the two tests
      p_value <- pmax(left$p.value, right$p.value)


      assertions::assert_number(p_value)
      return(p_value)
    },

    #' Power parameter estimation method
    #'
    #' @description This method estimates the power parameter for the given target data.
    #'
    #' @param target_data The target data object.
    #' @param source_treatment_effect_estimate The source treatment effect estimate.
    #' @param target_treatment_effect_estimate The target treatment effect estimate.
    #' @return The power parameter.
    power_parameter_estimation = function(target_data) {
      transformed_treatment_effects <- self$hypothesis_space_transformation(target_data = target_data)
      source_treatment_effect_estimate <- transformed_treatment_effects$source_treatment_effect_estimate
      target_treatment_effect_estimate <- transformed_treatment_effects$target_treatment_effect_estimate

      p_value <- self$test(
        target_data = target_data,
        target_treatment_effect_estimate = target_treatment_effect_estimate,
        source_treatment_effect_estimate = source_treatment_effect_estimate
      )
      power_parameter <- exp((self$parameters$shape_parameter / (1 - p_value)) * log(1 - p_value))
      if (is.null(power_parameter) || is.na(power_parameter)) {
        stop("Variable is NULL or NA. Execution stopped.")
      }
      return(power_parameter)
    },

    #' @description
    #' Calculate the prior probability density function (PDF) for a given target treatment effect.
    #' @param target_treatment_effect The target treatment effect.
    #' @return The prior PDF.
    prior_pdf = function(target_treatment_effect) {
      if (is.null(self$prior_var)) {
        stop(
          "Prior variance is not defined. Run model$empirical_bayes_update(target_data) befor calling model$prior_pdf()."
        )
      }
      return(dnorm(
        target_treatment_effect,
        mean = self$prior_mean,
        sd = sqrt(self$prior_var)
      ))
    },

    #' @description
    #' Plot the power parameter as a function of drift in treatment effect
    #' @param source_treatment_effect_estimate Treatment effect estimate in the source study
    #' @param target_data Target study data
    #' @param min_drift Minimum drift value
    #' @param max_drift Maximum drift value
    #' @param resolution Number of points on the drift grid.
    #' @return The prior PDF.
    plot_power_parameter_vs_drift = function(source_treatment_effect_estimate,
                                             target_data,
                                             min_drift,
                                             max_drift,
                                             resolution) {
      # Generate a sequence of differences
      drifts <- seq(min_drift, max_drift, length.out = resolution)

      # Calculate power parameters for each difference
      power_parameters <- sapply(drifts, function(drift) {
        target_data_drift <- duplicate(target_data)
        target_data_drift$sample$treatment_effect_estimate <- source_treatment_effect_estimate + drift
        self$power_parameter_estimation(target_data)
      })


      # Create a data frame for plotting
      plot_data <- data.frame(Drift = drifts, PowerParameter = power_parameters)

      # Plot using ggplot2
      ggplot(plot_data, aes(x = Drift, y = PowerParameter)) +
        geom_line(color = "blue") +
        labs(title = "Power Parameter as a Function of Drift", x = "Drift", y = "Power Parameter") +
        theme_minimal()
    }
  )
)
