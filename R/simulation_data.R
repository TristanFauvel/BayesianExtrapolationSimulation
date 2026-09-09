#' Compute the standard error of the log odds ratio
#'
#' @description This function calculates the standard error of the log odds ratio based on the number of noncases and cases in the unexposed and exposed groups.
#'
#' @param n_control_nonresponders Number of noncases in the unexposed group
#' @param n_treatment_nonresponders Number of noncases in the exposed group
#' @param n_control_responders Number of cases in the unexposed group
#' @param n_treatment_responders Number of cases in the exposed group
#'
#' @return The standard error of the log odds ratio
#' @param n_treatment_nonresponders Number of noncases in the exposed group
#' @param n_control_responders Number of cases in the unexposed group
#' @param n_treatment_responders Number of cases in the exposed group
#' @param continuity_correction Logical; if TRUE, applies continuity correction (default is TRUE)
#'
#' @return The standard error of the log odds ratio
#'
#' @examples
#' standard_error_log_odds_ratio(100, 90, 50, 60)
#'
#' @export
#' @keywords internal
standard_error_log_odds_ratio <- function(n_control_nonresponders,
                                          n_treatment_nonresponders,
                                          n_control_responders,
                                          n_treatment_responders,
                                          continuity_correction = TRUE) {
  if (continuity_correction == TRUE) {
    n_control_responders <- 0.5 + n_control_responders
    n_treatment_responders <- 0.5 + n_treatment_responders
    n_control_nonresponders <- 0.5 + n_control_nonresponders
    n_treatment_nonresponders <- 0.5 + n_treatment_nonresponders
  }
  return(
    sqrt(
      1 / n_control_nonresponders + 1 / n_treatment_nonresponders + 1 / n_control_responders + 1 / n_treatment_responders
    )
  )
}


#' Generate binary data from a given rate
#'
#' @description This function generates binary data (0s and 1s) based on a given rate and sample size.
#'
#' @param rate The rate of success (1s) in the generated data
#' @param sample_size The total sample size
#'
#' @return A vector of binary data
#'
#' @export
generate_binary_data_from_rate <- function(rate, sample_size) {
  n_successes <- round(sample_size * rate)
  n_failures <- sample_size - n_successes

  return(c(rep(0, times = n_failures), rep(1, times = n_successes)))
}

#' Calculate the success rate in the target study arm from the drift on the log odds ratio scale
#'
#' @description This function calculates the success rate in the target study arm based on the drift (defined on the log scale) and the success rate in the arm of interest in the source study.
#'
#' @param arm_drift The drift (defined on the log scale) between the target and source study arms
#' @param source_rate The success rate in the arm of interest in the source study
#'
#' @return The success rate in the corresponding arm of the target study
#'
#' @keywords internal
#' @export
rate_from_drift_logOR <- function(arm_drift, source_rate) {
  source_odds <- source_rate / (1 - source_rate)

  exp_arm_drift <- exp(arm_drift)
  p_target <- exp_arm_drift / (exp_arm_drift + 1 / source_odds)

  if (exp_arm_drift == Inf &&
      source_odds != 0) {
    # A situation that can occurs for very large drifts
    p_target <- 1
  }
  return(p_target)
}

#' Calculate the rate in the target study arm from the drift on the log relative risk scale
#'
#' @description This function calculates the rate in the target study arm based on the drift (defined on the log scale) and the rate in the arm of interest in the source study.
#'
#' @param arm_drift The drift (defined on the log scale) between the target and source study arms
#' @param source_rate The rate in the arm of interest in the source study
#'
#' @return The rate in the corresponding arm of the target study
#' @keywords internal
#' @export
rate_from_drift_logRR <- function(arm_drift, source_rate) {
  return(source_rate * exp(arm_drift))
}

negative_binomial_variance <- function(mu, k) {
  mu + mu ^ 2 / k
}

sample_negative_binomial <- function(n, mu, k) {
  stats::rnbinom(n = n, size = k, mu = mu)
}

#' Compute odds ratios based on the number of participants and responders in each arm
#'
#' @description This function computes odds ratios based on the number of participants and responders in the control and treatment arms.
#'
#' @param n_control Number of participants in the control arm
#' @param n_treatment Number of participants in the treatment arm
#' @param n_control_responders Number of responders in the control arm
#' @param n_treatment_responders Number of responders in the treatment arm
#'
#' @return A list containing the log odds ratio, treatment rate, control rate, and standard error of the log odds ratio
#'
#' @export
#' @keywords internal
compute_ORs <- function(n_control,
                        n_treatment,
                        n_control_responders,
                        n_treatment_responders) {
  n_control_nonresponders <- n_control - n_control_responders
  n_treatment_nonresponders <- n_treatment - n_treatment_responders

  log_odds_ratio <- compute_log_odds_ratio_from_counts(
    n_control_responders,
    n_treatment_responders,
    n_control - n_control_responders,
    n_treatment - n_treatment_responders,
    continuity_correction = TRUE
  )

  std_err_log_odds_ratio <- standard_error_log_odds_ratio(
    n_control - n_control_responders,
    n_treatment - n_treatment_responders,
    n_control_responders,
    n_treatment_responders,
    continuity_correction = TRUE
  )

  odds_ratio_dict <- list(
    log_odds_ratio = log_odds_ratio,
    treatment_rate = n_treatment_responders / n_treatment,
    control_rate = n_control_responders / n_control,
    std_err_log_odds_ratio = std_err_log_odds_ratio
  )

  return(odds_ratio_dict)
}

#' Compute the log odds ratio based on two rates
#'
#' @description This function computes the log odds ratio based on two rates.
#'
#' @param rate1 The rate in the first group
#' @param rate2 The rate in the second group
#'
#' @return The log odds ratio
#' @keywords internal
compute_log_odds_ratio_from_rates <- function(rate1, rate2) {
  odds_ratio <- (rate1 / (1 - rate1)) / (rate2 / (1 - rate2))
  log_odds_ratio <- log(odds_ratio)
  return(log_odds_ratio)
}

#' Compute the log odds ratio from count data
#'
#' @param n_control_responders Number of responders in the control arm
#' @param n_treatment_responders Number of responders in the treatment arm
#' @param n_control_nonresponders Number of nonresponders in the control arm
#' @param n_treatment_nonresponders Number of nonresponders in the treatment arm
#' @param continuity_correction Whether to apply continuity correction or not
#'
#' @return The log odds ratio
#' @keywords internal
compute_log_odds_ratio_from_counts <- function(n_control_responders,
                                               n_treatment_responders,
                                               n_control_nonresponders,
                                               n_treatment_nonresponders,
                                               continuity_correction) {
  if (continuity_correction == TRUE) {
    n_control_responders <- 0.5 + n_control_responders
    n_treatment_responders <- 0.5 + n_treatment_responders
    n_control_nonresponders <- 0.5 + n_control_nonresponders
    n_treatment_nonresponders <- 0.5 + n_treatment_nonresponders
  }
  odds_ratio <- (n_treatment_responders * n_control_nonresponders) / (n_treatment_nonresponders * n_control_responders)
  log_odds_ratio <- log(odds_ratio)
  return(log_odds_ratio)
}

#' Sample log odds ratios based on the number of participants and responders in each arm
#'
#' @description This function samples log odds ratios based on the number of participants and responders in the control and treatment arms.
#'
#' @param n_control Number of participants in the control arm
#' @param n_treatment Number of participants in the treatment arm
#' @param treatment_rate The rate of success (1s) in the treatment arm
#' @param control_rate The rate of success (1s) in the control arm
#' @param n_replicates Number of replicates to sample
#'
#' @return A list containing the sampled log odds ratios and the standard error of the log odds ratio
#' @export
sample_log_odds_ratios <- function(n_control,
                                   n_treatment,
                                   treatment_rate,
                                   control_rate,
                                   n_replicates) {
  n_treatment_responders <- rbinom(n_replicates, n_treatment, treatment_rate)
  n_control_responders <- rbinom(n_replicates, n_control, control_rate)

  return(
    compute_ORs(
      n_control,
      n_treatment,
      n_control_responders,
      n_treatment_responders
    )
  )
}

#' Sample rate ratios based on the rates in the control and treatment arms
#'
#' @description This function samples rate ratios based on the rates in the control and treatment arms.
#'
#' @param control_rate The rate in the control arm
#' @param treatment_rate The rate in the treatment arm
#' @param n_replicates Number of replicates to sample
#' @param n_control Number of participants in the control arm
#' @param n_treatment Number of participants in the treatment arm
#'
#' @return A vector of sampled rate ratios
#' @export
sample_rate_ratios <- function(control_rate,
                               treatment_rate,
                               n_replicates,
                               n_control,
                               n_treatment) {
  sample_treatment_rate <- sample_aggregate_binary_data(treatment_rate, n_treatment, n_replicates)
  sample_control_rate <- sample_aggregate_binary_data(control_rate, n_control, n_replicates)

  return(sample_treatment_rate / sample_control_rate)
}

#' Sample aggregate normal data based on the mean and variance
#'
#' @description This function samples aggregate normal data based on the mean and variance.
#'
#' @param mean The mean of the normal distribution
#' @param variance The variance of the normal distribution
#' @param n_replicates Number of replicates to sample
#' @param n_samples_per_arm Number of samples per replicate
#'
#' @return A list containing the sample mean and sample standard error for each replicate
#' @export
sample_aggregate_normal_data <- function(mean,
                                         variance,
                                         n_replicates,
                                         n_samples_per_arm) {
  # The sampling distribution of the sufficient statistics is known in closed
  # form, so the individual observations never have to be materialised:
  # the sample mean is normal and the sample variance is a scaled chi-squared,
  # and the two are independent (Cochran's theorem).
  sample_mean <- rnorm(
    n_replicates,
    mean = mean,
    sd = sqrt(variance / n_samples_per_arm)
  )

  if (n_samples_per_arm > 1) {
    degrees_of_freedom <- n_samples_per_arm - 1
    sample_variance <- variance * rchisq(n_replicates, df = degrees_of_freedom) / degrees_of_freedom
  } else {
    # var() of a single observation is undefined
    sample_variance <- rep(NA_real_, n_replicates)
  }

  sample_standard_error <- sqrt(sample_variance / n_samples_per_arm) # standard error on the mean, for each replicate

  samples <- data.frame(
    treatment_effect_estimate = sample_mean,
    treatment_effect_standard_error = sample_standard_error,
    sample_size_per_arm = n_samples_per_arm,
    standard_deviation = sqrt(sample_variance)
  )

  return(samples)
}

#' Sample aggregate binary data based on the rate and sample size
#'
#' @description This function samples aggregate binary data based on the rate and sample size.
#'
#' @param rate The rate of success (1s) in the generated data
#' @param n The total sample size
#' @param n_replicates Number of replicates to sample
#'
#' @return A vector of sampled rates
#' @export
sample_aggregate_binary_data <- function(rate, n, n_replicates) {
  n_successes <- rbinom(n_replicates, n, rate)
  return(n_successes / n)
}

#' SourceData class
#'
#' @description This class represents the source data used for Bayesian borrowing. It processes the source data to compute necessary statistics for various endpoints.
#'
#' @importFrom R6 R6Class
#' @importFrom stats pexp
#'
#' @export
SourceData <- R6::R6Class("SourceData",
                          inherit = ObservedSourceData,
                          public = list(
                            #' @description Initialize the SourceData object
                            #'
                            #' @param case_study_config list: A configuration list containing details of the case study.
                            #' @param source_denominator numeric: An optional denominator for computing control rates.
                            #'
                            #' @return A new SourceData object.
                            initialize = function(case_study_config, source_denominator = NA) {
                              super$initialize(case_study_config)

                              if (self$endpoint == "binary") {
                                if (self$summary_measure_likelihood == "normal") {
                                  # Observed source data (required for the treatment effect estimate in the source data):

                                  if (is.na(source_denominator)) {
                                    odds_control <- self$control_rate / (1 - self$control_rate)
                                  } else {
                                    odds_control <- source_denominator
                                  }

                                  self$control_rate <- odds_control / (1 + odds_control)

                                  odds_treatment <- odds_control * exp(self$treatment_effect_estimate)
                                  self$treatment_rate <- odds_treatment / (1 + odds_treatment)

                                  # Compute the expected number of responders
                                  n_responders_control <- self$control_rate * self$sample_size_control
                                  n_responders_treatment <- self$treatment_rate * self$sample_size_treatment
                                  self$standard_error <- sqrt(
                                    1 / n_responders_treatment + 1 / (self$sample_size_treatment - n_responders_treatment) + 1 / n_responders_control + 1 / (self$sample_size_control - n_responders_control)
                                  )

                                  assert_single_number(self$treatment_rate)
                                  assert_single_number(self$control_rate)
                                }
                              } else if (self$endpoint == "time_to_event") {
                                if (is.na(source_denominator)) {
                                  self$control_rate <- case_study_config$source$control_rate
                                } else {
                                  self$control_rate <- source_denominator
                                }

                                self$treatment_rate <- case_study_config$source$treatment_rate

                                self$treatment_rate <- self$control_rate * exp(self$treatment_effect_estimate)

                                # The expected number of events in each arm is P(time_to_first_event <= max_time) * N_participants
                                n_control_events <- pexp(case_study_config$source$max_follow_up_time,
                                                         rate = self$control_rate) * self$sample_size_control
                                n_treatment_events <- pexp(case_study_config$source$max_follow_up_time,
                                                           rate = self$treatment_rate) * self$sample_size_treatment

                                SE_log_RR <- sqrt(1 / n_control_events + 1 / n_treatment_events)

                                self$standard_error <- SE_log_RR
                              } else if (self$endpoint == "recurrent_event") {
                                if (is.na(source_denominator)) {
                                  self$control_rate <- case_study_config$source$control_rate
                                } else {
                                  self$control_rate <- source_denominator
                                }

                                self$treatment_rate <- self$control_rate * exp(self$treatment_effect_estimate)

                                mu_treatment <- self$treatment_rate # Mean of the first rate
                                mu_control <- self$control_rate # Mean of the second rate
                                k_treatment <- 0.8 # Size parameter for the treatment arm
                                k_control <- 0.8 # Size parameter for the control arm
                                n_treatment <- case_study_config$source$treatment # Sample size for the first rate
                                n_control <- case_study_config$source$control # Sample size for the second rate

                                # Compute the standard deviations for the negative binomial distributions
                                sigma_treatment <- sqrt(negative_binomial_variance(mu_treatment, k_treatment))
                                sigma_control <- sqrt(negative_binomial_variance(mu_control, k_control))

                                # Compute the standard errors of the rates
                                SE_R_treatment <- sigma_treatment / sqrt(n_treatment)
                                SE_R_control <- sigma_control / sqrt(n_control)

                                self$standard_error <- sqrt((SE_R_treatment / mu_treatment) ^ 2 + (SE_R_control / mu_control) ^
                                                              2)
                              }
                            }
                          ))

#' @title ObservedSourceData class
#'
#' @description This class represents the observed source data used for Bayesian borrowing in clinical studies.
#'
#' @field endpoint character. The endpoint of the study. Possible values are "continuous", "binary", "time_to_event", "recurrent_event".
#' @field summary_measure_likelihood character. The distribution of the summary measure. Possible values are "normal", "Binomial".
#' @field sample_size_control numeric. The sample size in the control arm of the source data.
#' @field sample_size_treatment numeric. The sample size in the treatment arm of the source data.
#' @field treatment_effect_estimate numeric. The estimated treatment effect in the source data.
#' @field standard_error numeric. The standard error of the treatment effect estimate in the source data.
#' @field control_rate numeric. The rate of success (1s) in the control arm of the source data (for binary endpoint).
#' @field treatment_rate numeric. The rate of success (1s) in the treatment arm of the source data (for binary endpoint).
#' @field equivalent_source_sample_size_per_arm numeric. Equivalent sample size per arm.
#'
#' @export
ObservedSourceData <- R6::R6Class(
  "ObservedSourceData",
  public = list(
    endpoint = NULL,
    summary_measure_likelihood = NULL,
    sample_size_control = NULL,
    sample_size_treatment = NULL,
    treatment_effect_estimate = NULL,
    standard_error = NULL,
    control_rate = NULL,
    treatment_rate = NULL,
    equivalent_source_sample_size_per_arm = NULL,
    #' @description Initialize an instance of ObservedSourceData
    #'
    #' @param case_study_config list. A list containing the configuration for the case study.
    #' @return An instance of ObservedSourceData.
    initialize = function(case_study_config) {
      self$endpoint <- case_study_config$endpoint

      self$summary_measure_likelihood <- case_study_config$summary_measure_likelihood
      self$sample_size_control <- case_study_config$source$control
      self$sample_size_treatment <- case_study_config$source$treatment

      assert_single_number(self$sample_size_control)
      assert_single_number(self$sample_size_treatment)

      self$equivalent_source_sample_size_per_arm <- 2 * self$sample_size_control * self$sample_size_treatment / (self$sample_size_control + self$sample_size_treatment)


      if (self$endpoint == "continuous") {
        self$treatment_effect_estimate <- case_study_config$source$treatment_effect
        self$standard_error <- case_study_config$source$standard_error
      } else if (self$endpoint == "binary") {
        self$control_rate <- case_study_config$source$responses$control / self$sample_size_control
        self$treatment_rate <- case_study_config$source$responses$treatment / case_study_config$source$treatment

        assert_single_number(self$treatment_rate)
        assert_single_number(self$control_rate)

        if (self$summary_measure_likelihood == "binomial") {
          self$treatment_effect_estimate <- self$treatment_rate - self$control_rate
          self$standard_error <- sqrt(
            self$treatment_rate * (1 - self$treatment_rate) / self$sample_size_treatment + self$control_rate * (1 - self$control_rate) / self$sample_size_control
          ) # Standard error of the difference of two proportions
        } else if (self$summary_measure_likelihood == "normal") {
          n_control <- case_study_config[["source"]]$control
          n_treatment <- case_study_config[["source"]]$treatment
          n_control_responders <- case_study_config[["source"]]$responses$control
          n_treatment_responders <- case_study_config[["source"]]$responses$treatment

          odds_ratio_dict <- compute_ORs(n_control,
                                         n_treatment,
                                         n_control_responders,
                                         n_treatment_responders)

          self$treatment_effect_estimate <- odds_ratio_dict$log_odds_ratio
          self$standard_error <- odds_ratio_dict$std_err_log_odds_ratio
        } else {
          stop("Not implemented for other distributions")
        }
      } else if (self$endpoint == "time_to_event") {
        if (self$summary_measure_likelihood == "normal") {
          self$treatment_effect_estimate <- case_study_config$source$treatment_effect
          self$standard_error <- case_study_config$source$standard_error
          self$control_rate <- case_study_config$source$control_rate
          self$treatment_rate <- case_study_config$source$treatment_rate
          assert_single_number(self$treatment_rate)
          assert_single_number(self$control_rate)
        } else {
          stop("Not implemented for other distributions")
        }
      } else if (self$endpoint == "recurrent_event") {
        self$treatment_effect_estimate <- case_study_config$source$treatment_effect
        self$standard_error <- case_study_config$source$standard_error
        self$control_rate <- case_study_config[["source"]]$control_rate
        self$treatment_rate <- case_study_config$source$treatment_rate
        assert_single_number(self$treatment_rate)
        assert_single_number(self$control_rate)
      } else {
        stop("Not implemented for other endpoints")
      }
      assert_single_number(self$treatment_effect_estimate)
      assert_single_number(self$standard_error)
    },

    #' Convert the object's data to a dictionary format
    #'
    #' @return list. A list containing the source data attributes.
    to_dict = function() {
      if (self$endpoint == "continuous") {
        source_dict <- list(
          source_treatment_effect_estimate = self$treatment_effect_estimate,
          source_standard_error = self$standard_error,
          endpoint = self$endpoint,
          summary_measure_likelihood = self$summary_measure_likelihood,
          source_sample_size_control = self$sample_size_control,
          source_sample_size_treatment = self$sample_size_treatment,
          equivalent_source_sample_size_per_arm = self$equivalent_source_sample_size_per_arm,
          source_control_rate = NA,
          source_treatment_rate = NA
        )
      } else {
        source_dict <- list(
          source_treatment_effect_estimate = self$treatment_effect_estimate,
          source_standard_error = self$standard_error,
          endpoint = self$endpoint,
          summary_measure_likelihood = self$summary_measure_likelihood,
          source_sample_size_control = self$sample_size_control,
          source_sample_size_treatment = self$sample_size_treatment,
          equivalent_source_sample_size_per_arm = self$equivalent_source_sample_size_per_arm,
          source_control_rate = self$control_rate,
          source_treatment_rate = self$treatment_rate
        )
      }
      return(source_dict)
    }
  )
)

#' @title TargetDataFactory class
#' @description A factory class for creating different types of target data objects.
#' @export
TargetDataFactory <- R6::R6Class("TargetDataFactory", public = list(
  #' @description Creates a target data object based on the source data and configuration.
  #' @param source_data The source data object.
  #' @param case_study_config The case study configuration object.
  #' @param target_sample_size_per_arm The target sample size per arm.
  #' @param control_drift The control drift.
  #' @param treatment_drift The treatment drift.
  #' @param summary_measure_likelihood The summary measure distribution.
  #' @param target_to_source_std_ratio Ratio between the target and source study sampling standard deviation
  #' @return The created target data object.
  create = function(source_data,
                    case_study_config,
                    target_sample_size_per_arm,
                    control_drift = 0,
                    treatment_drift,
                    summary_measure_likelihood,
                    target_to_source_std_ratio = NULL) {
    assertions::assert_class(source_data, c("SourceData", "ObservedSourceData"))
    assert_single_number(target_sample_size_per_arm)
    assert_single_number(control_drift)
    assert_single_number(treatment_drift)
    sampling_approximation <- case_study_config$sampling_approximation
    if (source_data$endpoint == "continuous") {
      if (is.null(target_to_source_std_ratio)) {
        target_to_source_std_ratio <- 1
      }
      target_data <- ContinuousTargetData$new(
        source_data = source_data,
        sampling_approximation = sampling_approximation,
        target_sample_size_per_arm = target_sample_size_per_arm,
        control_drift = control_drift,
        treatment_drift = treatment_drift,
        summary_measure_likelihood = summary_measure_likelihood,
        target_to_source_std_ratio = target_to_source_std_ratio
      )
    } else if (source_data$endpoint == "binary") {
      target_data <- BinaryTargetData$new(
        source_data = source_data,
        sampling_approximation = sampling_approximation,
        target_sample_size_per_arm = target_sample_size_per_arm,
        control_drift = control_drift,
        treatment_drift = treatment_drift,
        summary_measure_likelihood = summary_measure_likelihood
      )
    } else if (source_data$endpoint == "recurrent_event") {
      target_data <- RecurrentEventTargetData$new(
        source_data = source_data,
        sampling_approximation = sampling_approximation,
        target_sample_size_per_arm = target_sample_size_per_arm,
        control_drift = control_drift,
        treatment_drift = treatment_drift,
        summary_measure_likelihood = summary_measure_likelihood
      )
    } else if (source_data$endpoint == "time_to_event") {
      target_data <- TimeToEventTargetData$new(
        source_data = source_data,
        sampling_approximation = sampling_approximation,
        target_sample_size_per_arm = target_sample_size_per_arm,
        control_drift = control_drift,
        treatment_drift = treatment_drift,
        summary_measure_likelihood = summary_measure_likelihood,
        max_follow_up_time = case_study_config$target$max_follow_up_time
      )
    } else {
      stop("Not implemented for other endpoints")
    }
    return(target_data)
  }
))


#' @title TargetData class
#' @description A base class for target data objects.
#' @field endpoint The endpoint of the target data.
#' @field treatment_drift The treatment drift value.
#' @field control_drift The control drift value.
#' @field drift The drift value calculated as the difference between treatment and control drifts.
#' @field summary_measure_likelihood The distribution of the summary measure.
#' @field sample_size_per_arm The target sample size per arm.
#' @field sample_size_control The target sample size for the control group.
#' @field sample_size_treatment The target sample size for the treatment group.
#' @field treatment_effect The target treatment effect value.
#' @field sampling_approximation The flag indicating if sampling approximation is used.
#' @field standard_deviation The target standard deviation value.
#' @field sample The sample data.
#' @export
TargetData <- R6::R6Class(
  "TargetData",
  public = list(
    endpoint = NULL,
    treatment_drift = NULL,
    control_drift = NULL,
    drift = NULL,
    summary_measure_likelihood = NULL,
    sample_size_per_arm = NULL,
    sample_size_control = NULL,
    sample_size_treatment = NULL,
    treatment_effect = NULL,
    sampling_approximation = NULL,
    standard_deviation = NULL,
    sample = NULL,

    #' @description Initializes the target data object.
    #' @param source_data The source data object.
    #' @param sampling_approximation The sampling approximation flag.
    #' @param target_sample_size_per_arm The target sample size per arm.
    #' @param control_drift The control drift.
    #' @param treatment_drift The treatment drift.
    #' @param summary_measure_likelihood The summary measure distribution.
    initialize = function(source_data,
                          sampling_approximation,
                          target_sample_size_per_arm,
                          control_drift = 0,
                          treatment_drift,
                          summary_measure_likelihood) {
      assertions::assert_class(source_data, c("SourceData", "ObservedSourceData"))
      assertions::assert_flag(sampling_approximation)
      assert_single_number(target_sample_size_per_arm)
      assert_single_number(control_drift)
      assert_single_number(treatment_drift)

      self$endpoint <- source_data$endpoint
      self$treatment_drift <- treatment_drift
      self$control_drift <- control_drift
      self$drift <- treatment_drift - control_drift
      self$summary_measure_likelihood <- summary_measure_likelihood
      self$sample_size_per_arm <- target_sample_size_per_arm
      self$sample_size_control <- target_sample_size_per_arm
      self$sample_size_treatment <- target_sample_size_per_arm
      self$treatment_effect <- self$drift + source_data$treatment_effect_estimate
      self$sampling_approximation <- sampling_approximation
    },
    #' @description Converts the target data object to a dictionary.
    #' @return A list representing the target data object.
    to_dict = function() {
      stop("Subclass must implement a to_dict method")
    },

    #' @description Plot the target data samples
    #' @param data Data samples
    #' @return A plot.
    plot_sample = function(data) {
      # Plot 1: Histogram of treatment effect estimate
      p1 <- ggplot(data, aes(x = treatment_effect_estimate)) +
        geom_histogram(
          nbins = 100,
          fill = "black",
          color = "black",
          alpha = 0.7
        ) +
        theme_minimal() +
        ggplot2::labs(x = "Treatment Effect Estimate", y = "Frequency")

      # Plot 2: Histogram of treatment effect standard error
      p2 <- ggplot(data, aes(x = treatment_effect_standard_error)) +
        geom_histogram(
          nbins = 100,
          fill = "black",
          color = "black",
          alpha = 0.7
        ) +
        theme_minimal() +
        ggplot2::labs(x = "Treatment Effect Standard Error", y = "Frequency")

      p3 <- ggplot(data,
                   aes(x = treatment_effect_estimate, y = treatment_effect_standard_error)) +
        geom_hex(bins = 50) + # Adjust the number of bins as needed
        scale_fill_viridis_c() + # Use viridis color scale for better perception
        theme_minimal() +
        ggplot2::labs(x = "Treatment Effect Estimate", y = "Treatment Effect Standard Error", fill = "Count")

      # # Plot 3: Scatter plot of treatment effect estimate vs standard error
      # p3 <- ggplot(data, aes(x = treatment_effect_estimate, y = treatment_effect_standard_error)) +
      #   geom_point(color = "black", size = 0.1) +
      #   theme_minimal() +
      #   ggplot2::labs(x = "Treatment Effect Estimate",
      #        y = "Treatment Effect Standard Error")


      # Arrange the plots in a grid
      # grid.arrange(p1, p2, p3, ncol = 3)
      #  Define the aspect ratio (width to height)
      aspect_ratio <- 2 # For example, width is twice the height

      # Arrange the plots in a grid with a controlled aspect ratio
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(
        layout = grid::grid.layout(
          1,
          3,
          widths = grid::unit(c(1, 1, 1), "null"),
          heights = grid::unit(1 / aspect_ratio, "npc")
        )
      ))
      print(p1, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(p2, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
      print(p3, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 3))
    }
  )
)


#' @title ContinuousTargetData class
#' @description A class for continuous target data objects.
#' @export
ContinuousTargetData <- R6::R6Class(
  "ContinuousTargetData",
  inherit = TargetData,
  public = list(
    #' @description Initializes the continuous target data object.
    #' @param source_data The source data object.
    #' @param sampling_approximation The sampling approximation flag.
    #' @param target_sample_size_per_arm The target sample size per arm.
    #' @param control_drift The control drift.
    #' @param treatment_drift The treatment drift.
    #' @param summary_measure_likelihood The summary measure
    #' @param target_to_source_std_ratio Ratio between the target and source studies standard deviation
    initialize = function(source_data,
                          sampling_approximation,
                          target_sample_size_per_arm,
                          control_drift,
                          treatment_drift,
                          summary_measure_likelihood,
                          target_to_source_std_ratio) {
      super$initialize(
        source_data,
        sampling_approximation,
        target_sample_size_per_arm,
        control_drift,
        treatment_drift,
        summary_measure_likelihood
      )

      if (self$endpoint != "continuous") {
        stop("Invalid endpoint or treatment effect distribution")
      }

      self$treatment_effect <- self$drift + source_data$treatment_effect_estimate

      assert_single_number(target_to_source_std_ratio)

      self$standard_deviation <- target_to_source_std_ratio * source_data$standard_error  * sqrt(source_data$equivalent_source_sample_size_per_arm) # This is the sampling standard deviation for pairs of individual patients data.

      assert_single_number(self$standard_deviation)
      assert_single_number(self$treatment_effect)
    },

    #' @description Generates samples for the continuous target data object.
    #' @param n_replicates The number of replicates to generate.
    #' @return A data frame containing the generated samples.
    generate = function(n_replicates) {
      if (self$summary_measure_likelihood == "normal") {
        samples <- sample_aggregate_normal_data(
          mean = self$treatment_effect,
          variance = self$standard_deviation ^ 2,
          n_replicates = n_replicates,
          n_samples_per_arm = self$sample_size_per_arm
        )
      } else {
        stop("Not implemented for other distributions")
      }
      return(samples)
    },

    #' @description Converts the target data object to a dictionary.
    #' @return A list representing the target data object.
    to_dict = function() {
      return(
        list(
          summary_measure_likelihood = self$summary_measure_likelihood,
          target_sample_size_per_arm = self$sample_size_per_arm,
          target_treatment_effect = self$treatment_effect,
          target_standard_deviation = self$standard_deviation,
          target_control_rate = NA,
          target_treatment_rate = NA
        )
      )
    }
  )
)


#' @title BinaryTargetData class
#' @description A class for binary target data objects.
#' @field control_rate Rate in the control arm of the target study
#' @field treatment_rate Rate in the treatment arm of the target study
#' @export
BinaryTargetData <- R6::R6Class(
  "BinaryTargetData",
  inherit = TargetData,
  public = list(
    control_rate = NULL,
    treatment_rate = NULL,

    #' @description Initializes the binary target data object.
    #' @param source_data The source data object.
    #' @param sampling_approximation The sampling approximation flag.
    #' @param target_sample_size_per_arm The target sample size per arm.
    #' @param control_drift The control drift.
    #' @param treatment_drift The treatment drift.
    #' @param summary_measure_likelihood The summary measure distribution.
    initialize = function(source_data,
                          sampling_approximation,
                          target_sample_size_per_arm,
                          control_drift,
                          treatment_drift,
                          summary_measure_likelihood) {
      super$initialize(
        source_data = source_data,
        sampling_approximation = sampling_approximation,
        target_sample_size_per_arm = target_sample_size_per_arm,
        control_drift = control_drift,
        treatment_drift = treatment_drift,
        summary_measure_likelihood = summary_measure_likelihood
      )

      if (self$endpoint != "binary") {
        stop("Invalid endpoint for BinaryTargetData")
      }

      # Assuming equal sample sizes in each arm for simplicity
      self$sample_size_control <- target_sample_size_per_arm
      self$sample_size_treatment <- target_sample_size_per_arm

      # Logic to adjust the treatment effect based on drifts
      if (summary_measure_likelihood == "binomial") {
        self$control_rate <- source_data$control_rate + control_drift
        self$treatment_rate <- source_data$treatment_rate + treatment_drift
      } else if (summary_measure_likelihood == "normal") {
        # We compute the rate in each arm, based on the drift (defined on the log scale)
        self$control_rate <- rate_from_drift_logOR(control_drift, source_data$control_rate)
        self$treatment_rate <- rate_from_drift_logOR(treatment_drift, source_data$treatment_rate)
      } else {
        stop("Not implemented for other distributions")
      }

      assert_single_number(self$treatment_rate)
      assert_single_number(self$control_rate)

      # Ensuring rates are within valid range ([0,1])
      stopifnot(all.equal(self$control_rate, min(max(self$control_rate, 0), 1)))
      stopifnot(all.equal(self$treatment_rate, min(max(self$treatment_rate, 0), 1)))

      if (summary_measure_likelihood == "binomial") {
        # compute the standard deviation
        treatment_effect_standard_error <- sqrt((
          self$treatment_rate * (1 - self$treatment_rate) / self$sample_size_control + self$control_rate * (1 - self$control_rate) / self$sample_size_treatment
        )
        )
        self$standard_deviation <- treatment_effect_standard_error * sqrt(target_sample_size_per_arm)
      } else if (summary_measure_likelihood == "normal") {
        self$standard_deviation <- standard_error_log_odds_ratio(
          self$sample_size_control * (1 - self$control_rate),
          self$sample_size_treatment * (1 - self$treatment_rate),
          self$sample_size_control * self$control_rate,
          self$sample_size_treatment * self$treatment_rate
        ) * sqrt(self$sample_size_per_arm)
      }

      assert_single_number(self$standard_deviation)

      # Logic to adjust the treatment effect based on drifts
      if (summary_measure_likelihood == "binomial") {
        self$treatment_effect <- self$treatment_rate - self$control_rate
      } else if (summary_measure_likelihood == "normal") {
        self$treatment_effect <- self$drift + source_data$treatment_effect_estimate
        # We make sure that target treatment effect is consistent with the rates in each arm, otherwise this means there is a mistake in the implementation
        # if (abs(compute_log_odds_ratio(self$treatment_rate, self$control_rate)) != Inf && abs(self$treatment_effect - compute_log_odds_ratio(self$treatment_rate, self$control_rate)) > 1e-3) {
        #   stop("The treatment effect (log odds ratio) is not consistent with the drift")
        # }
      }
      assert_single_number(self$treatment_effect)
    },

    #' @description Generates samples for the binary target data object.
    #' @param n_replicates The number of replicates to generate.
    #' @return A data frame containing the generated samples.
    generate = function(n_replicates) {
      if (self$summary_measure_likelihood == "normal" &&
          self$sampling_approximation == TRUE) {
        samples <- sample_aggregate_normal_data(
          mean = self$treatment_effect,
          variance = self$standard_deviation ^ 2,
          n_replicates = n_replicates,
          n_samples_per_arm = self$sample_size_per_arm
        )
      } else if (self$summary_measure_likelihood == "binomial") {
        sample_treatment_rate <- sample_aggregate_binary_data(self$treatment_rate,
                                                              self$sample_size_per_arm,
                                                              n_replicates)

        sample_control_rate <- sample_aggregate_binary_data(self$control_rate,
                                                            self$sample_size_per_arm,
                                                            n_replicates)

        treatment_effect_standard_error <- sqrt(
          sample_treatment_rate * (1 - sample_treatment_rate) / self$sample_size_control + sample_control_rate * (1 - sample_control_rate) / self$sample_size_treatment
        )

        standard_deviation <- sqrt(
          sample_treatment_rate * (1 - sample_treatment_rate) + sample_control_rate * (1 - sample_control_rate)
        )

        # Compute the treatment effect estimate
        samples <- data.frame(
          sample_control_rate = sample_control_rate,
          sample_treatment_rate = sample_treatment_rate,
          sample_size_per_arm = self$sample_size_per_arm,
          treatment_effect_estimate = sample_treatment_rate - sample_control_rate,
          treatment_effect_standard_error = treatment_effect_standard_error,
          standard_deviation = standard_deviation
        )
      } else if (self$summary_measure_likelihood == "normal" &&
                 self$sampling_approximation == FALSE) {
        log_OR_samples <- sample_log_odds_ratios(
          self$sample_size_control,
          self$sample_size_treatment,
          self$treatment_rate,
          self$control_rate,
          n_replicates
        )

        samples <- data.frame(
          treatment_effect_estimate = log_OR_samples$log_odds_ratio,
          treatment_effect_standard_error = log_OR_samples$std_err_log_odds_ratio,
          sample_size_per_arm = self$sample_size_per_arm,
          standard_deviation = log_OR_samples$std_err_log_odds_ratio*sqrt(self$sample_size_per_arm)
        )
      } else {
        stop("Not implemented for other distributions")
      }


      return(samples)
    },
    #' @description Converts the target data object to a dictionary.
    #' @return A list representing the target data object.
    to_dict = function() {
      return(
        list(
          summary_measure_likelihood = self$summary_measure_likelihood,
          target_sample_size_per_arm = self$sample_size_per_arm,
          target_control_rate = self$control_rate,
          target_treatment_rate = self$treatment_rate,
          target_treatment_effect = self$treatment_effect,
          target_standard_deviation = self$standard_deviation
        )
      )
    }
  )
)

# Maximum likelihood estimate of 1 / theta, the inverse of the negative
# binomial size parameter, for an intercept-only model whose mean is already
# known to be the sample mean.
#
# The estimate is parameterised by 1 / theta so that the Poisson limit is the
# finite boundary value 0 rather than an infinity. That boundary is treated as
# an explicit candidate: its profile likelihood is computed in closed form and
# compared against the best interior maximum, rather than being selected from
# the sign of the boundary score alone. The interior search is seeded from a
# coarse grid so that it does not rely on the profile being unimodal.
negative_binomial_inverse_dispersion <- function(input_data, rate_estimate) {
  n_observations <- length(input_data)

  if (rate_estimate <= 0) {
    return(0)
  }

  # Sparse frequency representation: the cost scales with the number of
  # distinct counts rather than with the largest one.
  runs <- rle(sort(input_data))
  values <- runs$values
  counts <- runs$lengths
  total_count <- sum(counts * values)
  largest_count <- values[length(values)]

  # Profile log likelihood of theta at mu = rate_estimate, dropping the
  # -lgamma(y + 1) term, which does not depend on theta.
  #
  # For large theta, lgamma(y + theta) and lgamma(theta) are enormous and
  # nearly equal, so subtracting them loses most of the significant digits
  # exactly where the interior maximum has to be compared against the Poisson
  # boundary. Summing log(theta + j) over j < y instead is accurate
  # throughout, and costs O(largest count), so it is used whenever the counts
  # are small enough for that to be cheap -- which covers the event counts
  # this package simulates. log1p keeps the second term accurate in the same
  # regime.
  use_rising_factorial <- largest_count <= 1000

  profile_loglikelihood <- function(log_theta) {
    theta <- exp(log_theta)
    rising_factorial <- if (use_rising_factorial) {
      cumulative <- c(0, cumsum(log(theta + seq_len(largest_count) - 1)))
      sum(counts * cumulative[values + 1])
    } else {
      sum(counts * (lgamma(values + theta) - lgamma(theta)))
    }
    rising_factorial -
      n_observations * theta * log1p(rate_estimate / theta) +
      total_count * log(rate_estimate / (rate_estimate + theta))
  }

  # Limit of the profile log likelihood as theta -> Inf, where the negative
  # binomial collapses to the Poisson model with the same mean.
  boundary_loglikelihood <- total_count * log(rate_estimate) -
    n_observations * rate_estimate

  # Coarse scan, then refine around the best grid point. Seeding this way
  # avoids assuming that the profile has a single interior maximum.
  log_theta_grid <- seq(log(1e-8), log(1e8), length.out = 24)
  grid_loglikelihood <- vapply(log_theta_grid, profile_loglikelihood, numeric(1))
  best <- which.max(grid_loglikelihood)
  lower <- log_theta_grid[max(best - 1, 1)]
  upper <- log_theta_grid[min(best + 1, length(log_theta_grid))]

  optimum <- stats::optimize(
    profile_loglikelihood,
    interval = c(lower, upper),
    maximum = TRUE,
    tol = 1e-8
  )

  interior_loglikelihood <- max(optimum$objective, grid_loglikelihood[best])
  if (interior_loglikelihood <= boundary_loglikelihood) {
    return(0)
  }

  best_log_theta <- if (optimum$objective >= grid_loglikelihood[best]) {
    optimum$maximum
  } else {
    log_theta_grid[best]
  }

  return(1 / exp(best_log_theta))
}

#' Estimate rate and standard error
#'
#' @description This function estimates the rate parameter of an intercept-only
#' negative binomial model and its standard error.
#'
#' @param input_data The data for which the rate parameter and standard error need to be estimated.
#' @return A list containing the rate estimate, standard error of the rate estimate, and standard error of the log(rate) estimate.
#' @export
negative_binomial_regression <- function(input_data) {
  if (length(input_data) == 0 || anyNA(input_data) ||
        !all(is.finite(input_data))) {
    stop("input_data must be a non-empty vector of finite counts.")
  }
  if (any(input_data < 0) || any(input_data != trunc(input_data))) {
    stop("input_data must contain non-negative integer counts.")
  }

  if (all(input_data == 0)) {
    # The maximum likelihood estimate of the rate is 0. The negative binomial
    # is then degenerate at zero, the dispersion is not identifiable, and the
    # log rate is not finite, so no standard error is defined.
    warning("All counts are zero: the rate estimate is 0 and the dispersion is not identifiable.")
    futile.logger::flog.warn("All counts are zero: the rate estimate is 0 and the dispersion is not identifiable.")
    return(list(
      rate_estimate = 0,
      se_rate = NA_real_,
      se_log_rate = NA_real_
    ))
  }

  n_observations <- length(input_data)

  # For an intercept-only negative binomial model with a log link, the score
  # equation for the mean reduces to sum(y_i - mu) = 0 whatever the dispersion,
  # so the maximum likelihood estimate of the rate is exactly the sample mean.
  # Only the dispersion has to be estimated numerically, which makes the
  # iteratively reweighted least squares performed by glm.nb redundant here.
  rate_estimate <- mean(input_data)

  inverse_dispersion <- negative_binomial_inverse_dispersion(
    input_data = input_data,
    rate_estimate = rate_estimate
  )

  # Standard error for the log(rate) parameter estimate, read off the negative
  # binomial Fisher information at the estimates, which is
  # 1 / (n * mu) + 1 / (n * theta).
  se_log_rate <- sqrt(1 / (n_observations * rate_estimate) +
                        inverse_dispersion / n_observations)

  # Backtransform the SE:
  se_rate <- se_log_rate * rate_estimate
  return(list(
    rate_estimate = rate_estimate,
    se_rate = se_rate,
    se_log_rate = se_log_rate
  ))
}

#' Recurrent Event Target Data
#'
#' @description This class represents target data for recurrent event analysis.
#' It inherits from the TargetData class.
#' @field control_rate Rate in the control arm of the target study
#' @field treatment_rate Rate in the treatment arm of the target study
#' @field k_treatment Negative-binomial size parameter in the treatment arm
#' @field k_control Negative-binomial size parameter in the control arm
RecurrentEventTargetData <- R6::R6Class(
  inherit = TargetData,
  public = list(
    control_rate = NULL,
    treatment_rate = NULL,
    k_treatment = NULL,
    k_control = NULL,
    #
    #' @description Initialize the RecurrentEventTargetData object
    #'
    #' @param source_data The source data used for generating the target data.
    #' @param sampling_approximation A flag indicating whether to use sampling approximation.
    #' @param target_sample_size_per_arm The target sample size per arm.
    #' @param control_drift The control drift.
    #' @param treatment_drift The treatment drift.
    #' @param summary_measure_likelihood The summary measure distribution.
    #' @param k_treatment Size parameter for the negative binomial distribution in the treatment arm.
    #' @param k_control Size parameter for the negative binomial distribution in the control arm.
    initialize = function(source_data,
                          sampling_approximation,
                          target_sample_size_per_arm,
                          control_drift = 0,
                          treatment_drift,
                          summary_measure_likelihood,
                          k_treatment = 0.8,
                          k_control = 0.8) {
      super$initialize(
        source_data = source_data,
        sampling_approximation = sampling_approximation,
        target_sample_size_per_arm = target_sample_size_per_arm,
        control_drift = control_drift,
        treatment_drift = treatment_drift,
        summary_measure_likelihood = summary_measure_likelihood
      )
      if (self$endpoint != "recurrent_event") {
        stop("Invalid endpoint")
      }
      self$sample_size_per_arm <- target_sample_size_per_arm
      assert_single_number(k_treatment)
      assert_single_number(k_control)
      if (k_treatment <= 0 || k_control <= 0) {
        stop("Negative-binomial size parameters must be positive.")
      }
      self$k_treatment <- k_treatment
      self$k_control <- k_control

      self$control_rate <- rate_from_drift_logRR(control_drift, source_data$control_rate)
      self$treatment_rate <- rate_from_drift_logRR(treatment_drift, source_data$treatment_rate)

      # Ensuring rates are within valid range
      stopifnot(self$control_rate >= 0)
      stopifnot(self$treatment_rate >= 0)


      self$treatment_effect <- self$drift + source_data$treatment_effect_estimate

      assert_single_number(self$control_rate)
      assert_single_number(self$treatment_rate)
      # We make sure that target treatment effect is consistent with the rates in each arm, otherwise this means there is an implementation mistake
      if (abs(self$treatment_effect - log(self$treatment_rate / self$control_rate)) > 1e-3) {
        stop("The treatment effect (log rates ratio) is not consistent with the drift")
      }

      if (self$summary_measure_likelihood == "normal") {
        mu_treatment <- self$treatment_rate # Mean of the first rate
        mu_control <- self$control_rate # Mean of the second rate
        n_treatment <- self$sample_size_per_arm # Sample size for the first rate
        n_control <- self$sample_size_per_arm # Sample size for the second rate

        # Compute the standard deviations for the negative binomial distributions
        sigma_treatment <- sqrt(negative_binomial_variance(mu_treatment, self$k_treatment))
        sigma_control <- sqrt(negative_binomial_variance(mu_control, self$k_control))

        # Compute the standard errors of the rates
        SE_R_treatment <- sigma_treatment / sqrt(n_treatment)
        SE_R_control <- sigma_control / sqrt(n_control)

        self$standard_deviation <- sqrt(self$sample_size_per_arm) * sqrt((SE_R_treatment / mu_treatment) ^
                                                                           2 + (SE_R_control / mu_control) ^ 2)

        assert_single_number(self$standard_deviation)
      } else if (self$summary_measure_likelihood != "normal") {
        stop("Not implemented for treatment effect distributions that are not Normal.")
      }
    },

    #' Generate target data
    #'
    #' @description This function generates target data based on the specified parameters.
    #'
    #' @param n_replicates The number of replicates to generate.
    #' @return A data frame containing the generated target data.
    generate = function(n_replicates) {
      if (self$summary_measure_likelihood == "normal" &&
          self$sampling_approximation == TRUE) {
        samples <- sample_aggregate_normal_data(
          mean = self$treatment_effect,
          variance = self$standard_deviation ^ 2,
          n_replicates = n_replicates,
          n_samples_per_arm = self$sample_size_per_arm
        )
      } else if (self$summary_measure_likelihood == "normal" &&
                 self$sampling_approximation == FALSE) {
        treatment_rate <- self$treatment_rate
        control_rate <- self$control_rate

        # Vector to store log rate ratios
        log_rate_ratios <- numeric(n_replicates)
        se_log_rate_ratios <- numeric(n_replicates)

        for (i in 1:n_replicates) {
          # Generate the patient-level data by sampling from negative binomial distributions
          target_data_control <- sample_negative_binomial(
            n = self$sample_size_per_arm,
            mu = control_rate,
            k = self$k_control
          )
          target_data_treatment <- sample_negative_binomial(
            n = self$sample_size_per_arm,
            mu = treatment_rate,
            k = self$k_treatment
          )

          treatment <- negative_binomial_regression(input_data = target_data_treatment)

          control <- negative_binomial_regression(input_data = target_data_control)

          # The log rate ratio is not estimable if an arm records no event at
          # all, in which case its rate estimate is zero.
          if (treatment$rate_estimate == 0 || control$rate_estimate == 0) {
            log_rate_ratio <- NA_real_
          } else {
            log_rate_ratio <- log(treatment$rate_estimate) -
              log(control$rate_estimate)
          }

          # Calculate the SE of the rate ratio using the delta method
          se_log_rate_ratio <- sqrt(treatment$se_log_rate ^ 2 + control$se_log_rate ^
                                      2)

          log_rate_ratios[i] <- log_rate_ratio
          se_log_rate_ratios[i] <- se_log_rate_ratio
        }

        samples <- data.frame(
          treatment_effect_estimate = log_rate_ratios,
          treatment_effect_standard_error = se_log_rate_ratios,
          sample_size_per_arm = self$sample_size_per_arm,
          standard_deviation = se_log_rate_ratios*sqrt(self$sample_size_per_arm)
        )
      } else {
        stop("Not implemented for other distributions")
      }
      return(samples)
    },
    #' @description Converts the target data object to a dictionary.
    #' @return A list representing the target data object.
    to_dict = function() {
      return(
        list(
          summary_measure_likelihood = self$summary_measure_likelihood,
          target_sample_size_per_arm = self$sample_size_per_arm,
          target_treatment_effect = self$treatment_effect,
          target_standard_deviation = self$standard_deviation,
          target_control_rate = self$control_rate,
          target_treatment_rate = self$treatment_rate
        )
      )
    }
  )
)

# Sample the sufficient statistics of one exponential survival arm, for every
# replicate at once. The exponential MLE depends on the patient-level data only
# through the number of events and the total exposure time, so the individual
# survival times never have to be materialised: the number of events is
# binomial, and each observed event time is exponential truncated to the
# follow-up window.
sample_exponential_arm_statistics <- function(n_subjects,
                                              rate,
                                              max_follow_up_time,
                                              n_replicates) {
  event_probability <- stats::pexp(max_follow_up_time, rate = rate)
  n_events <- stats::rbinom(n_replicates, n_subjects, event_probability)

  # Exposure contributed by the patients who relapse before the end of
  # follow-up. Their relapse times are drawn in a single call and totalled per
  # replicate; censored patients each contribute max_follow_up_time.
  exposure_from_events <- numeric(n_replicates)
  total_events <- sum(n_events)

  if (total_events > 0) {
    event_times <- stats::qexp(
      stats::runif(total_events) * event_probability,
      rate = rate
    )
    per_replicate <- rowsum(
      event_times,
      rep.int(seq_len(n_replicates), n_events)
    )
    exposure_from_events[as.integer(rownames(per_replicate))] <- per_replicate[, 1]
  }

  exposure <- exposure_from_events +
    (n_subjects - n_events) * max_follow_up_time

  return(list(n_events = n_events, exposure = exposure))
}

#' Time To Event Target Data
#'
#' @description This class represents target data for time-to-event analysis.
#' It inherits from the TargetData class.
#'
#' @field control_rate Rate in the control arm of the target study
#' @field treatment_rate Rate in the treatment arm of the target study
#' @field max_follow_up_time Maximum follow-up time
#'
#' @export
TimeToEventTargetData <- R6::R6Class(
  inherit = TargetData,
  public = list(
    control_rate = NULL,
    treatment_rate = NULL,
    max_follow_up_time = NULL,

    #' @description Initialize the TimeToEventTargetData object
    #'
    #' @param source_data The source data used for generating the target data.
    #' @param sampling_approximation A flag indicating whether to use sampling approximation.
    #' @param target_sample_size_per_arm The target sample size per arm.
    #' @param control_drift The control drift.
    #' @param treatment_drift The treatment drift.
    #' @param summary_measure_likelihood The summary measure distribution.
    #' @param max_follow_up_time The maximum follow-up time.
    initialize = function(source_data,
                          sampling_approximation,
                          target_sample_size_per_arm,
                          control_drift = 0,
                          treatment_drift,
                          summary_measure_likelihood,
                          max_follow_up_time) {
      super$initialize(
        source_data = source_data,
        sampling_approximation = sampling_approximation,
        target_sample_size_per_arm = target_sample_size_per_arm,
        control_drift = control_drift,
        treatment_drift = treatment_drift,
        summary_measure_likelihood = summary_measure_likelihood
      )
      if (self$endpoint != "time_to_event") {
        stop("Invalid endpoint")
      }

      self$max_follow_up_time <- max_follow_up_time

      self$sample_size_per_arm <- target_sample_size_per_arm

      self$treatment_effect <- self$drift + source_data$treatment_effect_estimate

      # The drift/treatment drift/control drift is defined on the log scale, so we convert them back to the natural scale:

      self$control_rate <- rate_from_drift_logRR(control_drift, source_data$control_rate)
      self$treatment_rate <- rate_from_drift_logRR(treatment_drift, source_data$treatment_rate)

      # Ensuring rates are within valid range
      stopifnot(self$control_rate >= 0)
      stopifnot(self$treatment_rate >= 0)

      # We make sure that target treatment effect is consistent with the rates in each arm, otherwise this means there is an implementation mistake
      if (abs(self$treatment_effect - log(self$treatment_rate / self$control_rate)) > 1e-3) {
        stop("The treatment effect (log rates ratio) is not consistent with the drift")
      }

      # Estimation of the sampling standard deviation on the log(RR)
      n_control_target <- self$sample_size_per_arm
      n_treatment_target <- self$sample_size_per_arm
      self$standard_deviation <- sqrt(
        1 / (
          self$control_rate * self$max_follow_up_time * n_control_target
        ) + 1 / (
          self$treatment_rate * self$max_follow_up_time * n_treatment_target
        )
      )
    },

    #' Generate target data
    #'
    #' @description This function generates target data based on the specified parameters.
    #'
    #' @param n_replicates The number of replicates to generate.
    #' @return A data frame containing the generated target data.
    generate = function(n_replicates) {
      n_control_target <- self$sample_size_per_arm
      n_treatment_target <- self$sample_size_per_arm

      if (self$summary_measure_likelihood == "normal" &&
          self$sampling_approximation == TRUE) {
        # Sample number of events over the follow-up period in each arm according to a Poisson distributions.
        n_control_events <- rpois(n_replicates,
                                  self$control_rate * self$max_follow_up_time * n_control_target)
        n_treatment_events <- rpois(
          n_replicates,
          self$treatment_rate * self$max_follow_up_time * n_treatment_target
        )

        SE_log_RR <- sqrt(1 / n_control_events + 1 / n_treatment_events)

        sampling_standard_deviation <- SE_log_RR * sqrt(self$sample_size_per_arm)

        samples <- sample_aggregate_normal_data(
          mean = self$treatment_effect,
          variance = sampling_standard_deviation ^ 2,
          n_replicates = n_replicates,
          n_samples_per_arm = self$sample_size_per_arm
        )
      } else if (self$summary_measure_likelihood == "normal" &&
                 self$sampling_approximation == FALSE) {
        # The exponential model fitted below has a closed-form maximum
        # likelihood estimate: the rate of an arm is its number of events
        # divided by its total exposure time. Sampling those two sufficient
        # statistics directly avoids fitting one survreg() per replicate.
        control <- sample_exponential_arm_statistics(
          n_subjects = n_control_target,
          rate = self$control_rate,
          max_follow_up_time = self$max_follow_up_time,
          n_replicates = n_replicates
        )
        treatment <- sample_exponential_arm_statistics(
          n_subjects = n_treatment_target,
          rate = self$treatment_rate,
          max_follow_up_time = self$max_follow_up_time,
          n_replicates = n_replicates
        )

        estimated_rate_control <- control$n_events / control$exposure
        estimated_rate_treatment <- treatment$n_events / treatment$exposure

        log_rate_ratios <- log(estimated_rate_treatment) -
          log(estimated_rate_control)

        # SE of the log rate ratio, obtained with the delta method
        se_log_rate_ratios <- sqrt(1 / control$n_events + 1 / treatment$n_events)

        # The rate ratio is not estimable if an arm records no event at all
        no_event <- control$n_events == 0 | treatment$n_events == 0
        log_rate_ratios[no_event] <- NA_real_

        samples <- data.frame(
          treatment_effect_estimate = log_rate_ratios,
          treatment_effect_standard_error = se_log_rate_ratios,
          sample_size_per_arm = self$sample_size_per_arm,
          standard_deviation = se_log_rate_ratios*sqrt(self$sample_size_per_arm)
        )
      } else {
        stop("Not implemented for other distributions")
      }
      return(samples)
    },
    #' @description Converts the target data object to a dictionary.
    #' @return A list representing the target data object.
    to_dict = function() {
      return(
        list(
          summary_measure_likelihood = self$summary_measure_likelihood,
          target_sample_size_per_arm = self$sample_size_per_arm,
          target_treatment_effect = self$treatment_effect,
          target_standard_deviation = self$standard_deviation,
          target_control_rate = self$control_rate,
          target_treatment_rate = self$treatment_rate
        )
      )
    }
  )
)
#' @title ObservedTargetData class
#' @description A wrapper used for analyzing observed data (not used for simulation)
#' @field summary_measure_likelihood Summary measure distribution
#' @field sample_size_per_arm Sample size per arm in the target study
#' @field sample Observed data sample
#' @export
ObservedTargetData <- R6::R6Class(
  "ObservedTargetData",
  public = list(
    summary_measure_likelihood = NULL,
    sample_size_per_arm = NULL,
    sample = NULL,

    #' @description Initializes an ObservedTargetData object with the given parameters
    #' @param treatment_effect_estimate The estimated treatment effect
    #' @param treatment_effect_standard_error The standard error of the treatment effect estimate
    #' @param target_sample_size_per_arm The target sample size per treatment arm
    #' @param summary_measure_likelihood The distribution of the summary measure (e.g., "normal", "binomial")
    #' @return An initialized ObservedTargetData object
    initialize = function(treatment_effect_estimate,
                          treatment_effect_standard_error,
                          target_sample_size_per_arm,
                          summary_measure_likelihood) {
      assert_single_number(treatment_effect_estimate)
      assert_single_number(treatment_effect_standard_error)
      assert_single_number(target_sample_size_per_arm)
      assertions::assert_character(summary_measure_likelihood)
      self$summary_measure_likelihood <- summary_measure_likelihood
      self$sample_size_per_arm <- target_sample_size_per_arm
      self$sample$treatment_effect_estimate <- treatment_effect_estimate
      self$sample$treatment_effect_standard_error <- treatment_effect_standard_error
    }
  )
)
