#' Model Class
#'
#' @description This class represents a statistical model for Bayesian borrowing analysis.
#' It provides methods for creating the model, performing inference, and calculating posterior moments.
#'
#' @field empirical_bayes Logical indicating if empirical Bayes method is used.
#' @field analytic_ocs Results of the analytic operating characteristics simulation.
#' @field posterior_parameters Parameters of the posterior distribution.
#' @field post_mean Mean of the posterior distribution.
#' @field post_median Median of the posterior distribution.
#' @field prior_mean Mean of the prior distribution.
#' @field prior_var Variance of the prior distribution.
#' @field post_var Variance of the posterior distribution.
#' @field parameters List of model parameters.
#' @field RBesT_prior Prior distribution from RBesT package.
#' @field RBesT_posterior Posterior distribution from RBesT package.
#' @field ess_morita Effective sample size calculated using Morita's method.
#' @field ess_elir Effective sample size calculated using ELIR method.
#' @field ess_moment Effective sample size calculated using moment matching.
#' @field prior Prior distribution used in the model.
#' @field method Method used for the analysis.
#' @field mcmc Logical indicating if MCMC is used.
#' @field RBesT_posterior_normix Normal mixture approximation to the posterior distribution
#' @field RBesT_prior_normix Normal mixture approximation to the prior distribution
#' @field summary_measure_likelihood Likelihood familly, that is, the distribution used to model the summary measure of the treatment effect.
#' @field n_components_mixture_approx Number of mixture components used for the mixture approximation
#' @field aic_penalty_parameter_mixture_approx AIC penalty parameter used for the mixture approximation
#' @export
Model <- R6::R6Class(
  "Model",
  public = list(
    empirical_bayes = FALSE,
    analytic_ocs = NULL,
    posterior_parameters = NULL,
    post_mean = NULL,
    post_median = NULL,
    prior_mean = NULL,
    prior_var = NULL,
    post_var = NULL,
    parameters = NULL,
    RBesT_prior = NULL,
    RBesT_posterior = NULL,
    ess_morita = NULL,
    ess_elir = NULL,
    ess_moment = NULL,
    prior = NULL,
    method = NULL,
    mcmc = FALSE,
    RBesT_posterior_normix = NULL,
    RBesT_prior_normix = NULL,
    summary_measure_likelihood = NULL,
    #' @field analysis_critical_value Posterior probability threshold the
    #'   analysis decides at, recorded when a simulation starts. Methods whose
    #'   tuning depends on the decision rule read it, so that they are tuned for
    #'   the test that is actually performed.
    analysis_critical_value = NULL,
    n_components_mixture_approx = seq(1, 4),
    aic_penalty_parameter_mixture_approx = 6,
    # Penalty parameter for AIC calculation (default 6)

    #' @description Initialize the Model object
    #'
    initialize = function() {

    },

    #' Create a Model object
    #'
    #' @description This method creates a Model object based on the specified configuration and method.
    #'
    #' @param case_study_config A list containing the case study configuration.
    #' @param method The method to be used for the analysis.
    #' @param method_parameters A list containing the method parameters.
    #' @param source_data Source data
    #' @param mcmc_config An optional list containing the MCMC configuration.
    #' @return A Model object.
    #'
    create = function(case_study_config,
                      method,
                      method_parameters,
                      source_data,
                      mcmc_config = NULL) {
      prior <- list(source = as.list(source_data),
                    method_parameters = method_parameters)

      null_space <- case_study_config$null_space

      if (case_study_config$summary_measure_likelihood == "binomial" &&
          is.null(mcmc_config)) {
        stop("MCMC configuration must be provided in this case.")
      }

      if (method == "RMP") {
        prior$vague_mean <- case_study_config$theta_0
        if (case_study_config$summary_measure_likelihood == "normal") {
          model <- GaussianRMP_RBesT$new(prior = prior)
        } else if (case_study_config$summary_measure_likelihood == "binomial") {
          model <- TruncatedGaussianRMP$new(prior = prior, mcmc_config = mcmc_config)
        } else {
          stop('Only "normal" or "binomial" treatment effect distributions are supported')
        }
      } else if (method == "separate") {
        if (case_study_config$summary_measure_likelihood == "normal") {
          model <- SeparateGaussian_RBesT$new(prior = prior)
        } else if (case_study_config$summary_measure_likelihood == "binomial") {
          model <- BinomialSeparate$new(prior = prior, mcmc_config = mcmc_config)
        } else {
          stop('Only "normal" or "binomial" treatment effect distributions are supported')
        }
      } else if (method == "pooling") {
        if (case_study_config$summary_measure_likelihood == "normal") {
          model <- PoolGaussian_RBesT$new(prior = prior)
        } else if (case_study_config$summary_measure_likelihood == "binomial") {
          model <- BinomialPooling$new(prior = prior, mcmc_config = mcmc_config)
        } else {
          stop('Only "normal" or "binomial" treatment effect distributions are supported')
        }
      } else if (method == "conditional_power_prior") {
        if (case_study_config$summary_measure_likelihood == "normal") {
          model <- StaticBorrowingGaussian$new(prior = prior)
        } else if (case_study_config$summary_measure_likelihood == "binomial") {
          model <- BinomialCPP$new(prior = prior, mcmc_config = mcmc_config)
        } else {
          stop('Only "normal" or "binomial" treatment effect distributions are supported')
        }
      } else if (method == "p_value_based_PP") {
        if (case_study_config$summary_measure_likelihood == "normal") {
          model <- p_value_based_PP_Gaussian$new(
            prior = prior,
            theta_0 = case_study_config$theta_0,
            null_space = null_space
          )
        } else if (case_study_config$summary_measure_likelihood == "binomial") {
          model <- p_value_based_PP_Binomial$new(
            prior = prior,
            theta_0 = case_study_config$theta_0,
            null_space = null_space,
            mcmc_config = mcmc_config
          )
        } else {
          stop('Only "normal" or "binomial" treatment effect distributions are supported')
        }
      } else if (method == "test_then_pool_equivalence") {
        if (case_study_config$summary_measure_likelihood == "normal") {
          model <- TestThenPoolEquivalence$new(prior = prior)
        } else if (case_study_config$summary_measure_likelihood == "binomial") {
          model <- TestThenPoolEquivalence$new(prior = prior, mcmc_config = mcmc_config)
        }
      } else if (method == "test_then_pool_difference") {
        if (case_study_config$summary_measure_likelihood == "normal") {
          model <- TestThenPoolDifference$new(prior = prior)
        } else if (case_study_config$summary_measure_likelihood == "binomial") {
          model <- TestThenPoolDifference$new(prior = prior, mcmc_config = mcmc_config)
        }
      } else if (method == "PDCCPP") {
        model <- PDCCPP$new(prior, theta_0 = case_study_config$theta_0, null_space = null_space)
      } else if (method == "EB_PP") {
        model <- Gaussian_Gravestock_EBPP$new(prior, null_space = null_space, theta_0 = case_study_config$theta_0)
      } else if (method == "NPP") {
        model <- Gaussian_NPP$new(prior)
      } else if (method == "commensurate_power_prior") {
        if (case_study_config$summary_measure_likelihood == "normal") {
          model <- GaussianCommensuratePowerPrior$new(
            prior = prior,
            mcmc_config = mcmc_config
          )
        } else if (case_study_config$summary_measure_likelihood == "binomial") {
          model <- GaussianCommensuratePowerPrior$new(prior = prior, mcmc_config = mcmc_config)
        }
      } else {
        stop("Method not implemented for this endpoint")
      }

      model$prior <- prior


      if (model$mcmc == TRUE) {
        draws_dir <- stan_draws_directory(case_study_config$name, method)
        if (!dir.exists(draws_dir)) {
          # If it doesn't exist, create it
          dir.create(draws_dir, recursive = TRUE)
        }
        model$draws_dir <- draws_dir
      }

      return(model)
    },

    #' @description Perform inference on the Model object
    #' @param target_data The target data for the inference
    #' @return A string "Success" if inference succeeded
    inference = function(target_data) {
      self$empirical_bayes_update(target_data)
      self$posterior_moments(target_data)
      return("Success")
    },

    #' @description Run every replicate at once, when the method allows it
    #'
    #' Methods with a closed-form posterior can produce the whole simulation
    #' output with vector arithmetic instead of one inference per replicate.
    #' Subclasses that can do so override this method; returning `NULL` means
    #' "no fast path available", and
    #' [Model$simulation_for_given_treatment_effect()] falls back to the
    #' replicate loop.
    #'
    #' @param ... Arguments describing the simulation, passed through by
    #'   [Model$simulation_for_given_treatment_effect()].
    #' @return `NULL`, or a list shaped like the return value of
    #'   [Model$simulation_for_given_treatment_effect()].
    vectorised_replicate_inference = function(...) {
      NULL
    },

    #' @description Check the validity of the data
    #' @param data_list The list of data elements
    #'
    check_data = function(data_list) {
      # Check if any element is numeric(0) or NULL
      if (any(sapply(data_list, function(x) {
        is.null(x) || length(x) == 0
      }))) {
        stop("Error: invalid data, some elements are NULL or numeric(0)")
      }
    },

    #' @description Abstract method to calculate the posterior moments
    #' @param target_data Data for which posterior moments need to be calculated
    #' @return none
    posterior_moments = function(target_data) {
      stop("Subclasses must implement the 'posterior_moments' method.",
           call. = FALSE)
    },

    #' @description Abstract method to calculate the cumulative distribution function of the prior
    #' @param target_treatment_effect The target treatment effect
    #' @return none
    prior_cdf = function(target_treatment_effect) {
      integral <- numeric(length(target_treatment_effect))
      for (i in seq(1, length(target_treatment_effect))) {
        xi <- target_treatment_effect[i]

        # The integrate function has bad behavior for large upper bound, with a non-monotonic behavior despite a positive integrand. The following decomposition solves the issue:
        integral_part_1 <- integrate(
          self$prior_pdf,
          lower = -Inf,
          upper = 0,
          rel.tol = 1e-3,
          stop.on.error = FALSE
        )$value

        integral_part_2 <- integrate(
          self$prior_pdf,
          lower = 0,
          upper = xi,
          rel.tol = 1e-3,
          stop.on.error = FALSE
        )$value

        integral[i] <- integral_part_1 + integral_part_2
      }
      # The value of the CDF may be inaccurate for large x values due to double integration.
      return(integral)
    },

    #' @description Method to update the prior
    #' @param target_data Data
    #' @return none
    empirical_bayes_update = function(target_data) {
      if (self$empirical_bayes) {
        stop("Not implemented", call. = FALSE)
      }
    },

    #' @description Method to calculate the mean of the posterior distribution
    #' @param mcmc Whether to use MCMC or not
    #' @param n_samples_quantiles_estimation Number of samples used to estimates quantiles of distributions
    #' @return The meen of the posterior distribution
    posterior_mean = function(mcmc, n_samples_quantiles_estimation = NA) {
      if (mcmc == TRUE) {
        posterior_samples <- self$sample_posterior(n_samples_quantiles_estimation)
        post_mean <- mean(posterior_samples)
      } else {
        bound <- Inf
        post_mean <- integrate(
          function(theta) {
            theta * self$posterior_pdf(theta)
          },
          lower = -bound,
          upper = bound,
          rel.tol = 1e-3,
          stop.on.error = TRUE
        )$value
      }
      self$post_mean <- post_mean
      return(post_mean)
    },
    #' @description Method to compute quantiles of the posterior distribution
    #' @param p Quantile
    #' @param mcmc Whether to use MCMC estimation or not
    #' @param n_samples_quantiles_estimation Number of samples used to estimates quantiles of distributions
    #' @return The quantile of the posterior distribution
    posterior_quantile = function(p, mcmc, n_samples_quantiles_estimation = NA) {
      if (mcmc == TRUE) {
        posterior_samples <- self$sample_posterior(n_samples_quantiles_estimation)
        return(stats::quantile(posterior_samples, probs = p))
      } else {
        res <- c()
        for (pi in p) {
          root <- stats::uniroot(
            function(theta) {
              self$posterior_cdf(x = theta) - pi
            },
            interval = c(
              self$post_mean - 10 * sqrt(self$post_var),
              self$post_mean + 10 * sqrt(self$post_var)
            ),
            tol = 1e-3
          )$root
          res <- c(res, root)
        }
        return(res)
      }
    },

    #' @description Method to calculate the median of the posterior distribution
    #' @param mcmc Whether to use MCMC or not
    #' @param n_samples_quantiles_estimation Number of samples used to estimates quantiles of distributions
    #' @return The median of the posterior distribution
    posterior_median = function(mcmc, n_samples_quantiles_estimation = NA) {
      return(
        self$posterior_quantile(
          p = 0.5,
          mcmc = mcmc,
          n_samples_quantiles_estimation = n_samples_quantiles_estimation
        )
      )
    },

    #' Calculates the credible interval of the posterior distribution for a given level
    #'
    #' @param level The level of confidence for the credible interval.
    #' @param mcmc Whether to use MCMC or not
    #' @param n_samples_quantiles_estimation Number of samples used to estimates quantiles of distributions
    #' @return The credible interval.
    credible_interval = function(level = 0.95,
                                 mcmc = FALSE,
                                 n_samples_quantiles_estimation = 10000) {
      lower_quantile <- (1 - level) / 2
      upper_quantile <- 1 - lower_quantile

      if (mcmc == TRUE) {
        ci <- self$posterior_quantile(
          p = c(lower_quantile, upper_quantile),
          mcmc = TRUE,
          n_samples_quantiles_estimation = n_samples_quantiles_estimation
        )
      } else {
        ci <- HDInterval::inverseCDF(c(lower_quantile, upper_quantile), function(x) {
          self$posterior_cdf(x)
        })
      }
      return(ci)
    },

    #' @description Method to calculate the effective sample size of the prior distribution
    prior_ESS = function() {
      stop("Not implemented", call. = FALSE)
    },

    #' @description Method to sample from the prior distribution
    #' @param n_samples Number of samples to draw from the prior distribution
    sample_prior = function(n_samples) {
      stop("Not implemented", call. = FALSE)
    },

    #' @description Method to sample from the posterior distribution
    #' @param n_samples Number of samples to draw from the posterior distribution
    sample_posterior = function(n_samples) {
      stop("Not implemented", call. = FALSE)
    },

    #' @description This function returns the test decision for a fitted model.
    #'
    #' @param critical_value The critical value for the test.
    #' @param theta_0 The null hypothesis value.
    #' @param null_space The null space for the test.
    #' @param confidence_level Confidence level for the test decision
    #'
    #' @return A logical value indicating the decision.
    test_decision = function(critical_value,
                             theta_0,
                             null_space,
                             confidence_level) {
      if (self$mcmc == TRUE) {
        stopifnot(
          "Only the 97.5 and 2.5 percentiles are computed, so you can only consider " = critical_value == (1 + confidence_level) / 2
        )
        percentile_97.5 <- self$credible_interval_97.5
        percentile_2.5 <- self$credible_interval_2.5

        if (null_space == "left") {
          return(percentile_2.5 > theta_0)
        } else {
          return(percentile_97.5 < theta_0)
        }
      } else if (!is.null(self$posterior_summary)) {
        if (null_space == "left") {
          return(self$posterior_summary["cri95L"] > theta_0)
        } else {
          return(self$posterior_summary["cri95U"] < theta_0)
        }
      } else {
        if (null_space == "left") {
          return(1 - self$posterior_cdf(theta_0) > critical_value)
        } else {
          return(self$posterior_cdf(theta_0) > critical_value)
        }
      }
    },

    #' @description Method to simulate for a given treatment effect
    #' @param target_data Data for the target treatment effect
    #' @param n_replicates Number of replicates to simulate
    #' @param critical_value Critical value for hypothesis testing
    #' @param theta_0 Null hypothesis value
    #' @param confidence_level Confidence level for the credible interval
    #' @param null_space The null space for hypothesis testing
    #' @param case_study Case study name
    #' @param method Method name
    #' @param to_return List of OCs to return
    #' @param verbose Verbosity level (0 or 1)
    #' @param n_samples_quantiles_estimation Number of samples used to estimate distributions quantiles.
    #' @param simulation_config Simulation configuration. Only required when
    #'   `to_return` includes `"ess_moment"`, `"ess_precision"` or `"ess_elir"`
    #'   and the method has no vectorised fast path, since those outputs are
    #'   computed via `posterior_to_RBesT()`/`prior_to_RBesT()`, which need
    #'   `simulation_config$n_samples_mixture_approx`.
    #' @return A list of simulation results including test decisions, posterior means, medians, credible intervals, and posterior parameters
    simulation_for_given_treatment_effect = function(target_data,
                                                     n_replicates,
                                                     critical_value,
                                                     theta_0,
                                                     confidence_level,
                                                     null_space,
                                                     case_study,
                                                     method,
                                                     to_return = c(
                                                       "credible_interval",
                                                       "test_decision",
                                                       "posterior_mean",
                                                       "posterior_median",
                                                       "posterior_parameters",
                                                       "ess_moment",
                                                       "ess_precision",
                                                       "ess_elir",
                                                       "fit_success",
                                                       "mcmc_diagnostics"
                                                     ),
                                                     verbose = 0,
                                                     n_samples_quantiles_estimation,
                                                     simulation_config = NULL) {
      assert_whole_number(n_replicates)

      requested <- function(output) output %in% to_return

      test_decisions <- if (requested("test_decision")) numeric(n_replicates) else NULL
      posterior_means <- if (requested("posterior_mean")) numeric(n_replicates) else NULL
      posterior_medians <- if (requested("posterior_median")) numeric(n_replicates) else NULL
      credible_intervals <- if (requested("credible_interval")) {
        matrix(numeric(2 * n_replicates), nrow = n_replicates)
      } else {
        NULL
      }
      ess_moments <- if (requested("ess_moment")) numeric(n_replicates) else NULL
      ess_precisions <- if (requested("ess_precision")) numeric(n_replicates) else NULL
      ess_elir <- if (requested("ess_elir")) numeric(n_replicates) else NULL
      posterior_parameters_list <- if (requested("posterior_parameters")) {
        vector("list", n_replicates)
      } else {
        NULL
      }
      fit_success <- if (requested("fit_success")) character(n_replicates) else NULL
      mcmc_ess <- if (requested("mcmc_diagnostics")) numeric(n_replicates) else NULL
      rhat <- if (requested("mcmc_diagnostics")) numeric(n_replicates) else NULL
      n_divergences <- if (requested("mcmc_diagnostics")) numeric(n_replicates) else NULL

      for (string in to_return) {
        if (!(
          string %in% c(
            "credible_interval",
            "test_decision",
            "posterior_mean",
            "posterior_median",
            "posterior_parameters",
            "ess_moment",
            "ess_precision",
            "ess_elir",
            "fit_success",
            "mcmc_diagnostics"
          )
        )) {
          stop("to_return argument is not valid.")
        }
      }

      stan_draws_path <- NULL
      next_stan_cleanup <- NULL

      cleanup_stan_draws <- function() {
        draw_files <- list.files(stan_draws_path, full.names = TRUE)

        if (length(draw_files) == 0) {
          return(invisible(NULL))
        }

        creation_times <- file.info(draw_files)$ctime
        file_ages <- as.numeric(difftime(Sys.time(), creation_times, units = "secs"))
        stale_files <- draw_files[!is.na(file_ages) & file_ages > 60]

        if (length(stale_files) > 0) {
          file.remove(stale_files)
        }

        invisible(NULL)
      }

      if (self$mcmc && !is.null(self$draws_dir)) {
        # Clean the directory the sampler actually writes to, rather than
        # rebuilding the path here and risking the two drifting apart.
        stan_draws_path <- self$draws_dir
        cleanup_stan_draws()
        next_stan_cleanup <- Sys.time() + 60
      }

      # Recorded so that methods tuned against the decision rule can see the
      # threshold this run decides at, rather than assuming one.
      self$analysis_critical_value <- critical_value

      # Generate data for n_replicates clinical trials
      target_data_samples <- target_data$generate(n_replicates)

      # Methods with a closed-form posterior compute every replicate at once.
      # No random number is drawn below this point for those methods, so the
      # two paths see exactly the same data and agree replicate by replicate.
      vectorised_results <- self$vectorised_replicate_inference(
        target_data = target_data,
        samples = target_data_samples,
        to_return = to_return,
        critical_value = critical_value,
        theta_0 = theta_0,
        confidence_level = confidence_level,
        null_space = null_space
      )

      if (!is.null(vectorised_results)) {
        return(vectorised_results)
      }

      for (r in 1:nrow(target_data_samples)) {
        retry <- TRUE # Indicator as to whether to restart the iteration or not
        while (retry) {
          retry <- FALSE
          start_time <- Sys.time()
          # drop = FALSE keeps a single-column sample a data frame, so that
          # downstream `$` access keeps working.
          target_data$sample <- target_data_samples[r, , drop = FALSE]

          # Fit the model
          inference_status <- self$inference(target_data = target_data)

          if (requested("fit_success")) {
            fit_success[r] <- inference_status
          }

          assertions::assert_number(self$post_mean)

          if (requested("posterior_mean")) {
            posterior_means[r] <- self$post_mean
          }

          if (requested("posterior_median")) {
            if (is.null(self$post_median)) {
              posterior_medians[r] <- self$posterior_median(n_samples_quantiles_estimation)
            } else {
              posterior_medians[r] <- self$post_median
            }
          }

          if (requested("credible_interval")) {
            credible_intervals[r, ] <- self$credible_interval(level = confidence_level)
          }

          if (requested("test_decision")) {
            test_decisions[r] <- self$test_decision(
              critical_value = critical_value,
              theta_0 = theta_0,
              null_space = null_space,
              confidence_level = confidence_level
            )
          }

          if (requested("posterior_parameters") && !is.null(self$posterior_parameters)) {
            posterior_parameters_list[[r]] <- data.frame(self$posterior_parameters)
          }

          if (requested("ess_moment") || requested("ess_precision")) {

            self$posterior_to_RBesT(target_data = target_data, simulation_config = simulation_config)
            if (requested("ess_moment")) {
              ess_moments[r] <- prior_moment_ess(rbest_model = self$RBesT_posterior_normix,
                                                 target_data = target_data)
            }

            if (requested("ess_precision")) {
              ess_precisions[r] <- prior_precision_ess(rbest_model = self$RBesT_posterior_normix,
                                                       target_data = target_data)
            }
          }

          if (requested("ess_elir")) {
            if (r == 1 || self$empirical_bayes == TRUE){
              # Only Empirical Bayes Methods have a prior that varies from on replicate to another
              self$prior_to_RBesT(simulation_config$n_samples_mixture_approx)
            }

            target_standard_deviation = target_data$sample$standard_deviation

            RBesT::sigma(self$RBesT_prior) <- target_standard_deviation
            RBesT::sigma(self$RBesT_prior_normix) <- target_standard_deviation

            ess_elir[r] <- prior_ess_elir(rbest_model = self$RBesT_prior_normix,
                                          target_data = target_data)
          }

          if (requested("mcmc_diagnostics") && self$mcmc) {
            # If the target ESS is not reached, start the iteration again, increasing the chain length, unless it is already at the maximal value allowed, which is 3x the required ESS.
            # If the target ESS is reached by a large factor, decrease the chain length and proceed to the next iteration.
            factor <- self$mcmc_ess / self$mcmc_config$target_ess
            if (1.1 < factor) {
              # The target MCMC ESS is exceeded by more than 10%
              # Decrease the chain lengths for the next replicates
              self$mcmc_config$chain_length <- as.integer(1.1 * self$mcmc_config$chain_length / factor)
            } else if (factor < 1) {
              # The target MCMC ESS is not reached
              # Increase the chain lengths and start the iteration again
              new_length <- as.integer(1.1 * self$mcmc_config$chain_length / factor)
              if (new_length > self$mcmc_config$max_chain_length) {
                new_length <- self$mcmc_config$max_chain_length
                retry <- FALSE
              } else {
                retry <- TRUE
              }
              self$mcmc_config$chain_length <- new_length
              print(paste0("Restart iteration ", r))
            }

            mcmc_ess[r] <- self$mcmc_ess
            rhat[r] <- self$rhat
            n_divergences[r] <- self$n_divergences
          }

          end_time <- Sys.time()
          time_taken <- difftime(end_time, start_time, units = "s")

          if (verbose == 1) {
            print(paste0("Duration of iteration ", r, " is ", time_taken, " seconds."))
          }

          # Avoid scanning the draws directory after every replicate. Files cannot
          # become eligible for age-based cleanup more often than once per minute.
          if (!is.null(next_stan_cleanup) && Sys.time() >= next_stan_cleanup) {
            cleanup_stan_draws()
            next_stan_cleanup <- Sys.time() + 60
          }
        }
      }

      posterior_parameters <- if (!is.null(posterior_parameters_list)) {
        do.call(rbind, Filter(Negate(is.null), posterior_parameters_list))
      } else {
        NULL
      }

      return(
        list(
          test_decisions = test_decisions,
          posterior_means = posterior_means,
          posterior_medians = posterior_medians,
          credible_intervals = credible_intervals,
          posterior_parameters = posterior_parameters,
          ess_moments = ess_moments,
          ess_precisions = ess_precisions,
          ess_elir = ess_elir,
          fit_success = fit_success,
          mcmc_ess = mcmc_ess,
          rhat = rhat,
          n_divergences = n_divergences
        )
      )
    },

    #' @description Method to calculate the prior probability of treatment benefit
    #' @param theta_0 Null hypothesis value
    #' @param null_space The null space for hypothesis testing
    #' @param n_prior_samples Number of prior samples
    #' @param tuning_length Tuning parameter length
    prior_treatment_benefit = function(theta_0,
                                       null_space,
                                       n_prior_samples,
                                       tuning_length) {
      stop("Not implemented", call. = FALSE)
    },

    #' @description Method to estimate frequentist operating characteristics
    #' @param theta_0 Null hypothesis value
    #' @param target_data Data for the target treatment effect
    #' @param n_replicates Number of replicates to simulate
    #' @param critical_value Critical value for hypothesis testing
    #' @param confidence_level Confidence level for the credible interval
    #' @param null_space The null space for hypothesis testing
    #' @param n_samples_quantiles_estimation Number of samples used to estimate distributions quantiles.
    #' @param case_study Case study name
    #' @param method Method name
    #' @param verbose Verbosity level (0 or 1)
    #' @param simulation_config Simulation configuration, needed by
    #'   `simulation_for_given_treatment_effect()` for `ess_moment`,
    #'   `ess_precision` and `ess_elir` on methods without a vectorised fast path.
    #' @return A list of estimated frequentist operating characteristics including coverage, MSE, bias, posterior mean, median, precision, credible intervals, and success probability
    estimate_frequentist_operating_characteristics = function(theta_0,
                                                              target_data,
                                                              n_replicates,
                                                              critical_value,
                                                              confidence_level,
                                                              null_space,
                                                              n_samples_quantiles_estimation,
                                                              case_study,
                                                              method,
                                                              verbose = 0,
                                                              simulation_config = NULL) {
      target_treatment_effect <- target_data$treatment_effect


      to_return <- c(
        "test_decision",
        "posterior_mean",
        "posterior_median",
        "credible_interval",
        "posterior_parameters",
        "ess_precision",
        "ess_moment",
        "ess_elir",
        "fit_success",
        "mcmc_diagnostics"
      )

      results <- self$simulation_for_given_treatment_effect(
        target_data = target_data,
        n_replicates = n_replicates,
        critical_value = critical_value,
        theta_0 = theta_0,
        confidence_level = confidence_level,
        null_space = null_space,
        case_study = case_study,
        method = method,
        to_return = to_return,
        verbose = verbose,
        n_samples_quantiles_estimation = n_samples_quantiles_estimation,
        simulation_config = simulation_config
      )

      test_decisions <- results$test_decisions
      posterior_means <- results$posterior_means
      posterior_medians <- results$posterior_medians
      credible_intervals <- results$credible_intervals
      posterior_parameters <- results$posterior_parameters
      ess_moments <- results$ess_moments
      ess_precisions <- results$ess_precisions
      fit_success <- results$fit_success
      rhat_values <- results$rhat
      mcmc_ess_values <- results$mcmc_ess
      n_divergences_values <- results$n_divergences

      # Determine whether the true value of the target treatment effect lies within the credible interval (to compute the coverage)
      estimate_in_CrI <- (
        credible_intervals[, 1] <= target_treatment_effect &
          target_treatment_effect <= credible_intervals[, 2]
      )

      coverage <- mean(estimate_in_CrI)

      conf_int_coverage <- binom.test(sum(estimate_in_CrI), length(estimate_in_CrI), conf.level = confidence_level)$conf.int

      errors <- (posterior_means - target_treatment_effect)
      squared_errors <- errors ^ 2
      mse <- mean(squared_errors)

      bias <- mean(errors)

      post_mean <- mean(posterior_means)

      post_median <- mean(posterior_medians)

      posterior_params <- list()
      if (!is.null(posterior_parameters)) {
        for (parameter in colnames(posterior_parameters)) {
          posterior_params[[parameter]] <- mean(posterior_parameters[[parameter]])

          if (n_replicates >= 10000){
            ci <- Hmisc::smean.cl.normal(posterior_parameters[[parameter]], conf.int = confidence_level)
          } else {
            ci <- Hmisc::smean.cl.boot(posterior_parameters[[parameter]], conf.int = confidence_level)
          }
          posterior_params[[paste0("conf_int_lower_", parameter)]] <- ci[2]
          posterior_params[[paste0("conf_int_upper_", parameter)]] <- ci[3]
        }
      }

      half_widths <- (credible_intervals[, 2] - credible_intervals[, 1]) / 2
      precision <- mean(half_widths)

      credible_interval <- colMeans(credible_intervals)

      proba_success <- mean(test_decisions)
      conf_int_proba_success <- binom.test(sum(test_decisions), length(test_decisions), conf.level = confidence_level)$conf.int
      mcse_proba_success <- sqrt(proba_success * (1 - proba_success) / n_replicates)

      ess_moment <- mean(ess_moments)

      ess_precision <- mean(ess_precisions)

      results$ess_elir <- na.omit(results$ess_elir)
      ess_elir  <- mean(results$ess_elir)


      if (all(fit_success == "Success")) {
        warning <- NA
      } else {
        # Find the first element that is not "Success"
        warning <- fit_success[fit_success != "Success"][1]
      }

      rhat <- mean(rhat_values)
      mcmc_ess <- mean(mcmc_ess_values)
      n_divergences <- mean(n_divergences_values)

      if (n_replicates >= 1000){
        conf_int_precision <-  Hmisc::smean.cl.normal(half_widths, conf.int = confidence_level)[2:3]
        conf_int_ess_moment <- Hmisc::smean.cl.normal(ess_moments, conf.int = confidence_level)[2:3]
        conf_int_ess_precision <- Hmisc::smean.cl.normal(ess_precisions, conf.int = confidence_level)[2:3]
        conf_int_ess_elir <- Hmisc::smean.cl.normal(results$ess_elir, conf.int = confidence_level)[2:3]
        conf_int_mse <- Hmisc::smean.cl.normal(squared_errors, conf.int = confidence_level)[2:3]
        conf_int_bias <- Hmisc::smean.cl.normal(errors, conf.int = confidence_level)[2:3]
        conf_int_posterior_mean <- Hmisc::smean.cl.normal(posterior_means, conf.int = confidence_level)[2:3]
        conf_int_posterior_median <- Hmisc::smean.cl.normal(posterior_medians, conf.int = confidence_level)[2:3]
        conf_int_rhat <- Hmisc::smean.cl.normal(rhat_values, conf.int = confidence_level)[2:3]
        conf_int_mcmc_ess <- Hmisc::smean.cl.normal(mcmc_ess_values, conf.int = confidence_level)[2:3]
        conf_int_n_divergences <- Hmisc::smean.cl.normal(n_divergences_values, conf.int = confidence_level)[2:3]
      } else {
        conf_int_precision <-  Hmisc::smean.cl.boot(half_widths, conf.int = confidence_level)[2:3]
        conf_int_ess_moment <- Hmisc::smean.cl.boot(ess_moments, conf.int = confidence_level)[2:3]
        conf_int_ess_precision <- Hmisc::smean.cl.boot(ess_precisions, conf.int = confidence_level)[2:3]
        conf_int_ess_elir <- Hmisc::smean.cl.boot(results$ess_elir, conf.int = confidence_level)[2:3]
        conf_int_mse <- Hmisc::smean.cl.boot(squared_errors, conf.int = confidence_level)[2:3]
        conf_int_bias <- Hmisc::smean.cl.boot(errors, conf.int = confidence_level)[2:3]
        conf_int_posterior_mean <- Hmisc::smean.cl.boot(posterior_means, conf.int = confidence_level)[2:3]
        conf_int_posterior_median <- Hmisc::smean.cl.boot(posterior_medians, conf.int = confidence_level)[2:3]
        conf_int_rhat <- Hmisc::smean.cl.boot(rhat_values, conf.int = confidence_level)[2:3]
        conf_int_mcmc_ess <- Hmisc::smean.cl.boot(mcmc_ess_values, conf.int = confidence_level)[2:3]
        conf_int_n_divergences <- Hmisc::smean.cl.boot(n_divergences_values, conf.int = confidence_level)[2:3]
      }

      result <- list(
        success_proba = proba_success,
        mcse_success_proba = mcse_proba_success,
        conf_int_success_proba_lower = conf_int_proba_success[1],
        conf_int_success_proba_upper = conf_int_proba_success[2],
        coverage = coverage,
        conf_int_coverage_lower = conf_int_coverage[1],
        conf_int_coverage_upper = conf_int_coverage[2],
        mse = mse,
        conf_int_mse_lower = conf_int_mse[1],
        conf_int_mse_upper = conf_int_mse[2],
        bias = bias,
        conf_int_bias_lower = conf_int_bias[1],
        conf_int_bias_upper = conf_int_bias[2],
        posterior_mean = post_mean,
        conf_int_posterior_mean_lower = conf_int_posterior_mean[1],
        conf_int_posterior_mean_upper = conf_int_posterior_mean[2],
        posterior_median = post_median,
        conf_int_posterior_median_lower = conf_int_posterior_median[1],
        conf_int_posterior_median_upper = conf_int_posterior_median[2],
        precision = precision,
        conf_int_precision_lower = conf_int_precision[1],
        conf_int_precision_upper = conf_int_precision[2],
        credible_interval_lower = credible_interval[1],
        credible_interval_upper = credible_interval[2],
        posterior_parameters = posterior_params,
        ess_moment = ess_moment,
        conf_int_ess_moment_lower = conf_int_ess_moment[1],
        conf_int_ess_moment_upper = conf_int_ess_moment[2],
        ess_precision = ess_precision,
        conf_int_ess_precision_lower = conf_int_ess_precision[1],
        conf_int_ess_precision_upper = conf_int_ess_precision[2],
        ess_elir = ess_elir,
        conf_int_ess_elir_lower = conf_int_ess_elir[1],
        conf_int_ess_elir_upper = conf_int_ess_elir[2],
        rhat = rhat,
        conf_int_rhat_lower = conf_int_rhat[1],
        conf_int_rhat_upper = conf_int_rhat[2],
        mcmc_ess = mcmc_ess,
        conf_int_mcmc_ess_lower = conf_int_mcmc_ess[1],
        conf_int_mcmc_ess_upper = conf_int_mcmc_ess[2],
        n_divergences = n_divergences,
        conf_int_n_divergences_lower = conf_int_n_divergences[1],
        conf_int_n_divergences_upper = conf_int_n_divergences[2],
        warning = warning
      )

      return(result)
    },

    #' @description Method to estimate Bayesian operating characteristics
    #' @param design_prior Design prior
    #' @param theta_0 Null hypothesis value
    #' @param source_data Source data
    #' @param n_replicates Number of replicates to simulate
    #' @param critical_value Critical value for hypothesis testing
    #' @param confidence_level Confidence level for the credible interval
    #' @param null_space The null space for hypothesis testing
    #' @param n_samples_design_prior Number of samples from the design prior
    #' @param target_sample_size_per_arm Sample size per arm in the target study
    #' @param case_study_config Configuration of the case study
    #' @param target_to_source_std_ratio Ratio between the target and source study standard deviation
    #' @param simulation_config Simulation configuration
    #' @param case_study Case study name
    #' @param method Method name
    #' @param n_samples_quantiles_estimation Number of samples used to estimate distribution quantiles
    #' @return A list of estimated frequentist operating characteristics including coverage, MSE, bias, posterior mean, median, precision, credible intervals, and success probability
    estimate_bayesian_operating_characteristics = function(design_prior,
                                                           theta_0,
                                                           source_data,
                                                           n_replicates,
                                                           critical_value,
                                                           confidence_level,
                                                           null_space,
                                                           n_samples_design_prior,
                                                           target_sample_size_per_arm,
                                                           case_study_config,
                                                           target_to_source_std_ratio,
                                                           simulation_config,
                                                           case_study,
                                                           method,
                                                           n_samples_quantiles_estimation) {
      test_decisions <- matrix(rep(0, n_samples_design_prior * n_replicates), nrow = n_samples_design_prior)

      control_drift <- 0
      design_prior_samples <- design_prior$sample(n_samples_design_prior)

      to_return <- c("test_decision")

      for (i in seq_along(design_prior_samples)) {
        drift <- design_prior_samples[i] - source_data$treatment_effect_estimate # This implies that target_data$treatment_effect <- design_prior_samples[i]
        treatment_drift <- drift

        target_data <- TargetDataFactory$new()

        assertions::assert_number(treatment_drift)

        target_data <- target_data$create(
          source_data = source_data,
          case_study_config = case_study_config,
          target_sample_size_per_arm = target_sample_size_per_arm,
          control_drift = control_drift,
          treatment_drift = treatment_drift,
          summary_measure_likelihood = case_study_config$summary_measure_likelihood,
          target_to_source_std_ratio = target_to_source_std_ratio
        )


        results <- self$simulation_for_given_treatment_effect(
          target_data = target_data,
          n_replicates = n_replicates,
          critical_value = critical_value,
          theta_0 = theta_0,
          confidence_level = confidence_level,
          null_space = null_space,
          case_study = case_study,
          method = method,
          to_return = to_return,
          n_samples_quantiles_estimation = n_samples_quantiles_estimation,
          simulation_config = simulation_config
        )

        test_decisions[i, ] <- results$test_decisions
      }

      prior_proba_success_estimate <- mean(test_decisions)
      conditional_proba_success <- rowMeans(test_decisions) # Contains Pr(Study success | theta_T)

      if (null_space == "left") {
        prior_proba_no_benefit_estimate <- design_prior$cdf(theta_0)
      } else if (null_space == "right") {
        prior_proba_no_benefit_estimate <- 1 - design_prior$cdf(theta_0)
      }

      prepost_proba_FP_estimate <- preposterior_proba_FP_MC(
        conditional_proba_success,
        design_prior_samples,
        theta_0,
        null_space
      )
      prepost_proba_TP_estimate <- preposterior_proba_TP_MC(
        conditional_proba_success,
        design_prior_samples,
        theta_0,
        null_space
      )

      average_tie_estimate <- average_tie(prepost_proba_FP_estimate,
                                          prior_proba_no_benefit_estimate)

      average_power_estimate <- average_power(prepost_proba_TP_estimate,
                                              prior_proba_no_benefit_estimate)

      upper_bound_proba_FP_estimate <- upper_bound_proba_FP_MC(
        model = self,
        prior_proba_no_benefit = prior_proba_no_benefit_estimate,
        source_data = source_data,
        theta_0 = theta_0,
        target_sample_size_per_arm = target_sample_size_per_arm,
        case_study_config = case_study_config,
        target_to_source_std_ratio = target_to_source_std_ratio,
        n_replicates = n_replicates,
        confidence_level = confidence_level,
        null_space = null_space,
        critical_value = critical_value,
        case_study = case_study,
        method = method,
        n_samples_quantiles_estimation = n_samples_quantiles_estimation
      )

      result <- list(
        average_tie = average_tie_estimate,
        average_power = average_power_estimate,
        prior_proba_no_benefit = prior_proba_no_benefit_estimate,
        prepost_proba_FP = prepost_proba_FP_estimate,
        prepost_proba_TP = prepost_proba_TP_estimate,
        upper_bound_proba_FP = upper_bound_proba_FP_estimate,
        prior_proba_success = prior_proba_success_estimate
      )
      return(result)
    },

    #' @description Method to convert the model to RBesT
    #' @param n_samples_mixture_approx Number of samples used to approximate the prior with a mixture.
    prior_to_RBesT = function(n_samples_mixture_approx) {
      # Note that this function is overriden in models for which there is no need to sample the prior and posterior to get the mixture approximation.
      if (is.null(n_samples_mixture_approx)) {
        stop(
          "prior_to_RBesT() needs simulation_config$n_samples_mixture_approx ",
          "to sample the prior for the mixture approximation, but it was not ",
          "provided. Pass simulation_config through from ",
          "simulation_for_given_treatment_effect()."
        )
      }

      prior_samples <- self$sample_prior(n_samples = n_samples_mixture_approx) # Samples to be fitted by a mixture distribution

      if (self$summary_measure_likelihood == "normal") {
        self$RBesT_prior_normix <- RBesT::automixfit(
          prior_samples,
          Nc = self$n_components_mixture_approx,
          k = self$aic_penalty_parameter_mixture_approx,
          thresh = -Inf,
          verbose = FALSE,
          type = c("norm")
        )
        prior_mixture_approximation <- self$RBesT_prior_normix
      } else if (self$summary_measure_likelihood == "binomial") {
        transformed_samples <- (prior_samples + 1) / 2 # Transformed samples in the [0, 1] range
        prior_mixture_approximation <- RBesT::automixfit(
          transformed_samples,
          Nc = self$n_components_mixture_approx,
          k = self$aic_penalty_parameter_mixture_approx,
          thresh = -Inf,
          verbose = FALSE,
          type = c("beta")
        )

        self$RBesT_prior_normix <- RBesT::automixfit(
          prior_samples,
          Nc = self$n_components_mixture_approx,
          k = self$aic_penalty_parameter_mixture_approx,
          thresh = -Inf,
          verbose = FALSE,
          type = c("norm")
        )
      } else {
        stop("Distribution not supported")
      }
      # if (!(is.null(target_standard_deviation))) {
      #   RBesT::sigma(self$RBesT_prior_normix) <- target_standard_deviation # Set the reference scale
      #   RBesT::sigma(prior_mixture_approximation) <- target_standard_deviation # Set the reference scale
      # }
      self$RBesT_prior <- prior_mixture_approximation
    },

    #' @description Convert the posterior distribution to RBesT format
    #' @param target_data Target study data
    #' @param simulation_config Configuration of simulation study
    posterior_to_RBesT = function(target_data, simulation_config) {
      # Note that this function is overriden in models for which there is no need to sample the prior and posterior to get the mixture approximation.
      if (is.null(simulation_config)) {
        stop(
          "posterior_to_RBesT() needs simulation_config$n_samples_mixture_approx ",
          "to sample the posterior for the mixture approximation, but ",
          "simulation_config was not provided. Pass it through from ",
          "simulation_for_given_treatment_effect()."
        )
      }

      posterior_samples <- self$sample_posterior(simulation_config$n_samples_mixture_approx) # Samples to be fitted by a mixture

      if (self$summary_measure_likelihood == "normal") {
        posterior_mixture_approximation <- RBesT::automixfit(
          posterior_samples,
          Nc = self$n_components_mixture_approx,
          k = self$aic_penalty_parameter_mixture_approx,
          thresh = -Inf,
          verbose = FALSE,
          type = c("norm")
        )

        RBesT::sigma(posterior_mixture_approximation) <- target_data$sample$standard_deviation # Set the reference scale

        self$RBesT_posterior_normix <- posterior_mixture_approximation

      } else if (self$summary_measure_likelihood == "binomial") {
        transformed_samples <- (posterior_samples + 1) / 2 # Transformed samples in the [0, 1] range
        # The posterior approximation with a mixture of Beta distributions is based on the non-transformed samples.
        posterior_mixture_approximation <- RBesT::automixfit(
          transformed_samples,
          Nc = self$n_components_mixture_approx,
          k = self$aic_penalty_parameter_mixture_approx,
          thresh = -Inf,
          verbose = FALSE,
          type = c("beta")
        )

        RBesT::sigma(posterior_mixture_approximation) <- target_data$sample$standard_deviation # Set the reference scale

        # The posterior approximation with a mixture of Gaussians is based on the non-transformed samples.
        self$RBesT_posterior_normix <- RBesT::automixfit(
          posterior_samples,
          Nc = self$n_components_mixture_approx,
          k = self$aic_penalty_parameter_mixture_approx,
          thresh = -Inf,
          verbose = FALSE,
          type = c("norm")
        )

        RBesT::sigma(self$RBesT_posterior_normix) <- target_data$sample$standard_deviation # Set the reference scale
      } else {
        stop("Distribution not supported")
      }
      self$RBesT_posterior <- posterior_mixture_approximation
    },

    #' @description
    #' Plot prior and posterior probability density functions (PDF).
    #' @param xmin Lower bound of the treatment effect range
    #' @param xmax Upper bound of the treatment effect range
    #' @param resolution Resolution of the treatment effect range
    #' @param ... Optional argument
    #' @return A plot
    plot_pdfs = function(xmin = -2,
                         xmax = 2,
                         resolution = 100,
                         ...) {
      x_values <- seq(xmin, xmax, length.out = resolution)

      prior_pdf <- model$prior_pdf(x_values)
      posterior_pdf <- model$posterior_pdf(x_values)

      df <- data.frame(x = x_values,
                       prior_pdf = prior_pdf,
                       posterior_pdf = posterior_pdf)

      df_long <- reshape2::melt(df, id = "x")

      plt <- ggplot(df_long, aes(x = x, y = value, color = variable)) +
        geom_line(size = 1) +
        ggplot2::labs(title = "Prior and posterior distribution of the treatment effect", x = "Treatment effect", y = "Density") +
        scale_color_manual(
          values = c(
            "prior_pdf" = "blue",
            "posterior_pdf" = "red"
          ),
          labels = c("prior_pdf" = "Prior pdf", "posterior_pdf" = "Posterior pdf")
        ) +
        theme_minimal() +
        guides(color = guide_legend(title = NULL))

      return(plt)
    },

    #' @description
    #' Plot prior probability density function (PDF).
    #' @param xmin Lower bound of the treatment effect range
    #' @param xmax Upper bound of the treatment effect range
    #' @param resolution Resolution of the treatment effect range
    #' @return A plot
    plot_prior_pdf = function(xmin = -2,
                              xmax = 2,
                              resolution = 100) {
      x_values <- seq(xmin, xmax, length.out = resolution)

      prior_pdf <- model$prior_pdf(x_values)

      df <- data.frame(x = x_values, prior_pdf = prior_pdf)

      # Use ggplot2 to plot both PDFs on the same plot
      plt <- ggplot2::ggplot(df, ggplot2::aes(x = x_values)) +
        geom_line(ggplot2::aes(y = prior_pdf),
                  color = "blue",
                  size = 1) +
        ggplot2::labs(
          title = "Prior distribution of the treatment effect",
          x = "Treatment effect",
          y = "Density",
          labels = c("Prior pdf")
        ) +
        theme_minimal() +
        scale_color_manual(values = c("blue")) +
        guides(color = guide_legend(title = NULL)) +
        scale_fill_manual(
          name = "PDF",
          labels = c("Prior pdf"),
          values = c("blue")
        )
      return(plt)
    },

    #' @description
    #' Plot posterior probability density function (PDF).
    #' @param xmin Lower bound of the treatment effect range
    #' @param xmax Upper bound of the treatment effect range
    #' @param resolution Resolution of the treatment effect range
    #' @return A plot
    plot_posterior_pdf = function(xmin = -2,
                                  xmax = 2,
                                  resolution = 100) {
      x_values <- seq(xmin, xmax, length.out = resolution)

      posterior_pdf <- model$posterior_pdf(x_values)

      df <- data.frame(x = x_values, posterior_pdf = posterior_pdf)

      # Use ggplot2 to plot both PDFs on the same plot
      plt <- ggplot2::ggplot(df, ggplot2::aes(x = x_values)) +
        geom_line(ggplot2::aes(y = posterior_pdf),
                  color = "red",
                  size = 1) +
        ggplot2::labs(
          title = "Posterior distribution of the treatment effect",
          x = "Treatment effect",
          y = "Density",
          labels = c("Posterior pdf")
        ) +
        theme_minimal() +
        scale_color_manual(values = c("red")) +
        guides(color = guide_legend(title = NULL)) +
        scale_fill_manual(
          name = "PDF",
          labels = c("Posterior pdf"),
          values = c("red")
        )
      return(plt)
    }
  )
)


#' ConjugateGaussian class
#'
#' @description This class represents a conjugate Gaussian model (Gaussian prior and Gaussian likelihood)
#'
#' @field prior_mean Prior mean
#' @field prior_var Prior variance
#' @field empirical_bayes Whether the method relies on empirical Bayes or not
#' @field post_mean Posterior mean
#' @field post_var Posterior variance
#' @field posterior_parameters Posterior parameters
#'
#' @export
ConjugateGaussian <- R6::R6Class(
  "ConjugateGaussian",
  # Model corresponding to a Gaussian prior and a Gaussian likelihood
  inherit = Model,
  public = list(
    prior_mean = NULL,
    prior_var = NULL,
    empirical_bayes = FALSE,
    post_mean = NULL,
    post_var = NULL,
    posterior_parameters = NULL,
    #' @description Initialize object from the ConjugateGaussian class
    #' @param prior Prior
    initialize = function(prior) {
      if (!(prior$method_parameters$initial_prior[[1]] == "noninformative")) {
        stop("Only implemented for a noninformative initial prior")
      }
      super$initialize()
      self$mcmc <- FALSE

      self$summary_measure_likelihood <- "normal"

      self$prior_mean <- prior$source$treatment_effect_estimate
      self$prior_var <- prior$source$standard_error ^ 2

      assertions::assert_number(self$prior_mean)
      assertions::assert_number(self$prior_var)
    },
    #' @description Sample from the prior
    #' @param n_samples Number of samples from the prior
    sample_prior = function(n_samples) {
      # Sample from the prior distribution
      rnorm(n_samples,
            mean = self$prior_mean,
            sd = sqrt(self$prior_var))
    },
    #' @description Sample from the posterior
    #' @param n_samples Number of samples from the posterior
    sample_posterior = function(n_samples) {
      # Sample from the prior distribution
      rnorm(n_samples,
            mean = self$post_mean,
            sd = sqrt(self$post_var))
    },
    #' @description Prior PDF
    #' @param target_treatment_effect Point at which to evaluate the prior PDF
    prior_pdf = function(target_treatment_effect) {
      dnorm(
        target_treatment_effect,
        mean = self$prior_mean,
        sd = sqrt(self$prior_var)
      )
    },
    #' @description Prior CDF
    #' @param target_treatment_effect Point at which to evaluate the prior CDF
    prior_cdf = function(target_treatment_effect) {
      pnorm(
        target_treatment_effect,
        mean = self$prior_mean,
        sd = sqrt(self$prior_var)
      )
    },
    #' @description Posterior CDF
    #' @param target_treatment_effect Point at which to evaluate the posterior CDF
    posterior_cdf = function(target_treatment_effect) {
      pnorm(
        target_treatment_effect,
        mean = self$post_mean,
        sd = sqrt(self$post_var)
      )
    },

    #' @description Posterior PDF
    #' @param target_treatment_effect Point at which to evaluate the posterior PDF
    posterior_pdf = function(target_treatment_effect) {
      dnorm(
        target_treatment_effect,
        mean = self$post_mean,
        sd = sqrt(self$post_var)
      )
    },
    #' @description Posterior mean
    #' @param target_data Target study data
    posterior_mean = function(target_data) {
      post_mean <- (
        self$prior_mean / (
          self$prior_var / target_data$sample$treatment_effect_standard_error ^ 2 + 1
        )
      ) + (
        target_data$sample$treatment_effect_estimate / (
          1 + target_data$sample$treatment_effect_standard_error ^ 2 / self$prior_var
        )
      )

      assertions::assert_number(post_mean)
      return(post_mean)
    },
    #' @description Posterior variance
    #' @param target_data Target study data
    posterior_variance = function(target_data) {
      return(1 / (
        1 / self$prior_var + 1 / target_data$sample$treatment_effect_standard_error ^
          2
      ))
    },
    #' @description Posterior moments
    #' @param target_data Target study data
    posterior_moments = function(target_data) {
      self$post_mean <- self$posterior_mean(target_data)
      self$post_var <- self$posterior_variance(target_data)
    },
    #' @description Run every replicate at once
    #'
    #' The prior is a single normal, so the posterior is available in closed
    #' form for all replicates simultaneously. Empirical Bayes subclasses
    #' re-derive the prior variance from each replicate; they supply it through
    #' `vectorised_prior_variance()`.
    #'
    #' @param target_data Target study data.
    #' @param samples Data frame of generated replicates.
    #' @param to_return Character vector of requested outputs.
    #' @param critical_value Critical value for hypothesis testing.
    #' @param theta_0 Null hypothesis value.
    #' @param confidence_level Confidence level for the credible interval.
    #' @param null_space The null space for hypothesis testing.
    #' @return A list of simulation results, or `NULL` to use the replicate loop.
    vectorised_replicate_inference = function(target_data, samples, to_return,
                                              critical_value, theta_0,
                                              confidence_level, null_space) {
      prior_variance <- self$vectorised_prior_variance(target_data, samples)
      if (is.null(prior_variance)) {
        return(NULL)
      }

      n_replicates <- nrow(samples)

      vectorised_normal_mixture_simulation(
        weights = matrix(1, nrow = n_replicates, ncol = 1),
        means = matrix(self$prior_mean, nrow = n_replicates, ncol = 1),
        sds = matrix(sqrt(prior_variance), nrow = n_replicates, ncol = 1),
        samples = samples,
        target_data = target_data,
        to_return = to_return,
        critical_value = critical_value,
        theta_0 = theta_0,
        confidence_level = confidence_level,
        null_space = null_space,
        decision_rule = "posterior_cdf",
        posterior_parameters = self$vectorised_posterior_parameters(prior_variance)
      )
    },

    #' @description Prior variance for each replicate
    #'
    #' Declines the vectorised path by default. Subclasses with a genuinely
    #' fixed prior return it, and empirical Bayes subclasses return one
    #' variance per replicate.
    #'
    #' @param target_data Target study data.
    #' @param samples Data frame of generated replicates.
    #' @return A scalar, a vector with one entry per replicate, or `NULL`.
    vectorised_prior_variance = function(target_data, samples) {
      NULL
    },

    #' @description Posterior parameters reported by the vectorised path
    #'
    #' The plain conjugate models report none. Empirical Bayes subclasses
    #' override this to report the power parameter they estimated.
    #'
    #' @param prior_variance Per-replicate prior variance.
    #' @return A data frame, or `NULL`.
    vectorised_posterior_parameters = function(prior_variance) {
      NULL
    },

    #' @description Posterior median
    #' @param ... Additional argument
    #' @return The posterior median
    posterior_median = function(...) {
      return(self$post_mean)
    },
    #' @description Credible interval
    #' @param level Level of the credible interval
    credible_interval = function(level = 0.95) {
      alpha <- (1 - level) / 2
      lower <- stats::qnorm(alpha,
                            mean = self$post_mean,
                            sd = sqrt(self$post_var))
      upper <- stats::qnorm(1 - alpha,
                            mean = self$post_mean,
                            sd = sqrt(self$post_var))
      return(c(lower, upper))
    },
    #' @description Convert the prior to RBesT format
    #' @param ... Additional arguments
    prior_to_RBesT = function(...) {
      # Convert the prior to a format suitable for RBesT
      self$RBesT_prior <- RBesT::mixnorm(dist = c(1, self$prior_mean, sqrt(self$prior_var)))

      self$RBesT_prior_normix <- self$RBesT_prior
    },
    #' @description Convert the posterior distribution to RBesT format
    #' @param target_data Target study data
    #' @param ... Additional arguments
    posterior_to_RBesT = function(target_data, ...) {
      self$RBesT_posterior <- RBesT::mixnorm(
        dist = c(1, self$post_mean, sqrt(self$post_var)),
        sigma = target_data$sample$standard_deviation
      )
      self$RBesT_posterior_normix <- self$RBesT_posterior
    }
  )
)


#' StaticBorrowingGaussian class
#'
#' @description This class represents a model with a Gaussian prior derived from static borrowing, and a Gaussian likelihood. It inherits from the `ConjugateGaussian` class.
#'
#' @field power_parameter The power parameter for the model. A NULL value indicates no power parameter, whereas a non-zero value sets the prior variance based on the source's standard error and the power parameter.
#' @field prior_var The variance of the prior. This is set based on the power parameter.
#' @field method Method name
#' @examples NA
StaticBorrowingGaussian <- R6::R6Class(
  "StaticBorrowingGaussian",
  inherit = ConjugateGaussian,
  public = list(
    power_parameter = NULL,
    prior_var = NULL,
    method = "Static borrowing",

    #' @param prior Prior
    #' @return A Model object.
    initialize = function(prior) {
      # Call the initialize method of the superclass
      super$initialize(prior = prior)

      # Ensure the initial prior is noninformative
      if (!is.null(prior$method_parameters$initial_prior[[1]]) &&
          prior$method_parameters$initial_prior[[1]] != "noninformative") {
        stop("Not implemented for this prior, use a noninformative prior")
      }

      # Set the power parameter
      self$power_parameter <- prior$method_parameters$power_parameter[[1]]

      # Calculate the prior variance based on the power parameter
      if (is.null(self$power_parameter)) {
        self$prior_var <- NULL
      } else if (self$power_parameter != 0) {
        self$prior_var <- (prior$source$standard_error ^ 2) / self$power_parameter
      } else {
        self$prior_var <- 1000 # Vague prior
      }
    },

    #' @description Prior variance for each replicate
    #'
    #' Static borrowing fixes the prior up front, so every replicate shares it.
    #'
    #' @param target_data Target study data.
    #' @param samples Data frame of generated replicates.
    #' @return The prior variance.
    vectorised_prior_variance = function(target_data, samples) {
      self$prior_var
    }
  )
)

#' SeparateGaussian class
#'
#' @description This class represents a model with separate Gaussian components, inheriting from `StaticBorrowingGaussian`. The power parameter is set to 0.
#' @field method Method name
#' @examples NA
SeparateGaussian <- R6::R6Class(
  "SeparateGaussian",
  inherit = StaticBorrowingGaussian,
  public = list(
    method = "separate",
    #' @param prior Prior
    #' @return A Model object.
    initialize = function(prior) {
      # Set the power parameter to 0 for SeparateGaussian
      prior$method_parameters$power_parameter <- 0
      super$initialize(prior)
    }
  )
)

#' PoolGaussian class
#'
#' @description This class represents a model with pooled Gaussian components, inheriting from `StaticBorrowingGaussian`. The power parameter is set to 1.
#' @field method Method name
#' @examples NA
PoolGaussian <- R6::R6Class(
  "PoolGaussian",
  inherit = StaticBorrowingGaussian,
  public = list(
    method = "gaussian",
    #' @param prior Prior
    #' @return A Model object.
    initialize = function(prior) {
      # Set the power parameter to 1 for PoolGaussian
      prior$method_parameters$power_parameter <- 1
      super$initialize(prior)
    }
  )
)

#' @title Model_RBesT
#' @description An R6 class representing a Bayesian model using RBesT.
#' @field posterior_summary Summary of the posterior distribution
#' @export
Model_RBesT <- R6::R6Class(
  "Model_RBesT",
  inherit = Model,
  public = list(
    posterior_summary = NULL,

    #' @description Initializes the GaussianRMP object
    #'
    #' @param prior The prior information for the analysis.
    initialize = function(prior) {
      if (!(prior$method_parameters$initial_prior[[1]] == "noninformative")) {
        stop("Only implemented for a noninformative initial prior")
      }
      super$initialize()

      self$summary_measure_likelihood <- "normal"
    },

    #' @description Calculates the prior probability density function (PDF) for a given target treatment effect.
    #' @param target_treatment_effect The target treatment effect.
    #' @return The prior PDF.
    prior_pdf = function(target_treatment_effect) {
      return(RBesT::dmix(self$RBesT_prior, target_treatment_effect, log = FALSE))
    },

    #' @description Calculates the prior cumulative distribution function (CDF) for a given target treatment effect.
    #'
    #' @param target_treatment_effect The target treatment effect.
    #' @return The prior CDF.
    prior_cdf = function(target_treatment_effect) {
      return(
        RBesT::pmix(
          self$RBesT_prior,
          target_treatment_effect,
          lower.tail = TRUE,
          log.p = FALSE
        )
      )
    },

    #' @description Calculates the posterior moments based on the target data.
    #' @param target_data The target data for the analysis.
    #' @return None
    posterior_moments = function(target_data) {
      sample_mean <- target_data$sample$treatment_effect_estimate
      standard_error <- target_data$sample$treatment_effect_standard_error
      target_sample_size_per_arm <- target_data$target_sample_size_per_arm

      self$RBesT_posterior <- RBesT::postmix(
        self$RBesT_prior,
        m = target_data$sample$treatment_effect_estimate,
        se = target_data$sample$treatment_effect_standard_error
      )

      inference_results <- summary(self$RBesT_posterior)
      names(inference_results) <- c("mean", "standard_deviation", "cri95L", "median", "cri95U")
      self$posterior_summary <- inference_results

      self$post_mean <- self$posterior_summary["mean"]

      self$post_var <- self$posterior_summary["standard_deviation"] ^ 2
      self$post_median <- self$posterior_summary["median"]
    },

    #' @description Calculates the posterior mean.
    #' @return The posterior mean.
    posterior_mean = function() {
      inference_results <- summary(self$RBesT_posterior)
      names(inference_results) <- c("mean", "standard_deviation", "cri95L", "median", "cri95U")
      self$posterior_summary <- inference_results
      return(self$posterior_summary["mean"])
    },

    #' @description Calculates the posterior variance.
    #' @return The posterior variance.
    posterior_variance = function() {
      inference_results <- summary(self$RBesT_posterior)
      names(inference_results) <- c("mean", "standard_deviation", "cri95L", "median", "cri95U")
      self$posterior_summary <- inference_results
      return(self$posterior_summary["standard_deviation"] ^
               2)
    },

    #' @description Calculates the posterior median.
    #' @param ... Additional arguments
    #' @return The posterior median.
    posterior_median = function(...) {
      return(self$post_median)
    },

    #' @description Calculates the posterior probability density function (PDF) for a given target treatment effect.
    #'
    #' @param target_treatment_effect The target treatment effect.
    #' @return The posterior PDF.
    posterior_pdf = function(target_treatment_effect) {
      return(RBesT::dmix(self$RBesT_posterior, target_treatment_effect, log = FALSE))
    },

    #' @description Calculates the posterior cumulative distribution function (CDF) for a given target treatment effect.
    #'
    #' @param target_treatment_effect The target treatment effect.
    #' @return The posterior CDF.
    posterior_cdf = function(target_treatment_effect) {
      return(
        RBesT::pmix(
          self$RBesT_posterior,
          target_treatment_effect,
          lower.tail = TRUE,
          log.p = FALSE
        )
      )
    },

    #' @description Samples from the prior distribution.
    #'
    #' @param n_samples The number of samples to generate.
    #' @return The samples from the prior distribution.
    sample_prior = function(n_samples) {
      samples <- RBesT::rmix(self$RBesT_prior, n = n_samples)
      return(samples)
    },

    #' @description Samples from the posterior distribution.
    #'
    #' @param n_samples The number of samples to generate.
    #' @return The samples from the posterior distribution.
    sample_posterior = function(n_samples) {
      samples <- RBesT::rmix(self$RBesT_posterior, n = n_samples)
      return(samples)
    },

    #' @description Converts the prior distribution to the RBesT format.
    #' @param ... Additional arguments
    #' @return None
    prior_to_RBesT = function(...) {
      stop("Subclass must implement a prior_to_RBesT method")
    },

    #' @description Converts the posterior distribution to the RBesT format.
    #' @param target_data Target study data
    #' @param ... Additional arguments
    posterior_to_RBesT = function(target_data, ...) {
      stop("Subclass must implement a posterior_to_RBesT method")
    },

    #' @description Calculates the credible interval.
    #' @param level Level of the credible interval.
    #' @return A vector containing the lower and upper bounds of the credible interval.
    credible_interval = function(level = 0.95) {
      if (level != 0.95) {
        stop("Not implemented for other than 95% CrI")
      }
      return(c(self$posterior_summary["cri95L"], self$posterior_summary["cri95U"]))
    },

    #' @description Prior mixture components for each replicate
    #'
    #' Subclasses return the `weights`, `means` and `sds` of their prior, either
    #' as vectors shared by every replicate or as matrices with one row per
    #' replicate. Returning `NULL` disables the vectorised path.
    #'
    #' @param target_data Target study data.
    #' @param samples Data frame of generated replicates.
    #' @return A list with `weights`, `means` and `sds`, or `NULL`.
    vectorised_prior_components = function(target_data, samples) {
      NULL
    },

    #' @description Posterior parameters reported by the vectorised path
    #' @param posterior Posterior mixture returned by [normal_mixture_posterior()].
    #' @return A data frame, or `NULL`.
    vectorised_posterior_parameters = function(posterior) {
      NULL
    },

    #' @description Run every replicate at once
    #'
    #' @param target_data Target study data.
    #' @param samples Data frame of generated replicates.
    #' @param to_return Character vector of requested outputs.
    #' @param critical_value Critical value for hypothesis testing.
    #' @param theta_0 Null hypothesis value.
    #' @param confidence_level Confidence level for the credible interval.
    #' @param null_space The null space for hypothesis testing.
    #' @return A list of simulation results, or `NULL` to use the replicate loop.
    vectorised_replicate_inference = function(target_data, samples, to_return,
                                              critical_value, theta_0,
                                              confidence_level, null_space) {
      prior <- self$vectorised_prior_components(target_data, samples)
      if (is.null(prior)) {
        return(NULL)
      }

      if (confidence_level != 0.95) {
        # Matches the scalar credible_interval(), which only supports 95%.
        stop("Not implemented for other than 95% CrI")
      }

      n_replicates <- nrow(samples)
      posterior <- normal_mixture_posterior(
        weights = prior$weights,
        means = prior$means,
        sds = prior$sds,
        estimate = samples$treatment_effect_estimate,
        standard_error = samples$treatment_effect_standard_error
      )

      vectorised_normal_mixture_simulation(
        weights = prior$weights,
        means = prior$means,
        sds = prior$sds,
        samples = samples,
        target_data = target_data,
        to_return = to_return,
        critical_value = critical_value,
        theta_0 = theta_0,
        confidence_level = confidence_level,
        null_space = null_space,
        decision_rule = "credible_interval",
        posterior_parameters = self$vectorised_posterior_parameters(posterior),
        posterior = posterior
      )
    }
  )
)

#' @title SeparateGaussian_RBesT
#' @description An R6 class representing a separate Gaussian model using RBesT.
#' @field method Method name
#' @export
SeparateGaussian_RBesT <- R6::R6Class(
  "SeparateGaussian_RBesT",
  inherit = Model_RBesT,
  public = list(
    method = "separate",
    #' @description Initializes the SeparateGaussian_RBesT object.
    #' @param prior Prior information for the analysis.
    #' @return A new SeparateGaussian_RBesT object.
    initialize = function(prior) {
      super$initialize(prior)
      self$prior_mean <- prior$source$treatment_effect_estimate
      self$prior_var <- 1000 # Vague prior
      self$RBesT_prior <- RBesT::mixnorm(c(1, self$prior_mean, sqrt(self$prior_var)))
      self$empirical_bayes <- FALSE
      self$mcmc <- FALSE
    },

    #' @description Converts the prior distribution to the RBesT format.
    #' @param ... Additional arguments.
    #' @return None
    prior_to_RBesT = function(...) {
      self$RBesT_prior <- RBesT::mixnorm(c(1, self$prior_mean, sqrt(self$prior_var)))

      self$RBesT_prior_normix <- self$RBesT_prior
    },

    #' @description Converts the posterior distribution to the RBesT format.
    #' @param target_data Target study data.
    #' @param ... Additional arguments.
    #' @return None
    posterior_to_RBesT = function(target_data, ...) {
      self$RBesT_posterior <- RBesT::mixnorm(c(1, self$post_mean, sqrt(self$post_var)), sigma =  target_data$sample$standard_deviation)
      self$RBesT_posterior_normix <- self$RBesT_posterior
    },

    #' @description Prior mixture components for each replicate.
    #' @param target_data Target study data.
    #' @param samples Data frame of generated replicates.
    #' @return A list with `weights`, `means` and `sds`.
    vectorised_prior_components = function(target_data, samples) {
      list(weights = 1, means = self$prior_mean, sds = sqrt(self$prior_var))
    }
  )
)

#' @title PoolGaussian_RBesT
#' @description An R6 class representing a pooled Gaussian model using RBesT.
#' @field method Method name
#' @export
PoolGaussian_RBesT <- R6::R6Class(
  "PoolGaussian_RBesT",
  inherit = Model_RBesT,
  public = list(
    method = "pooling",
    #' @description Initializes the PoolGaussian_RBesT object.
    #' @param prior Prior information for the analysis.
    #' @return A new PoolGaussian_RBesT object.
    initialize = function(prior) {
      super$initialize(prior)
      self$prior_mean <- prior$source$treatment_effect_estimate
      self$prior_var <- prior$source$standard_error ^
        2

      assertions::assert_number(self$prior_mean)
      assertions::assert_number(self$prior_var)

      self$RBesT_prior <- RBesT::mixnorm(c(1, self$prior_mean, sqrt(self$prior_var)))
      self$empirical_bayes <- FALSE
      self$mcmc <- FALSE
    },

    #' @description Converts the prior distribution to the RBesT format.
    #' @param ... Additional arguments.
    #' @return None
    prior_to_RBesT = function(...) {

      self$RBesT_prior <- RBesT::mixnorm(c(1, self$prior_mean, sqrt(self$prior_var)))

      self$RBesT_prior_normix <- self$RBesT_prior
    },

    #' @description Converts the posterior distribution to the RBesT format.
    #' @param target_data Target study data object.
    #' @param ... Additional arguments.
    #' @return None
    posterior_to_RBesT = function(target_data, ...) {
      self$RBesT_posterior <- RBesT::mixnorm(c(1, self$post_mean, sqrt(self$post_var)), sigma =  target_data$sample$standard_deviation)
      self$RBesT_posterior_normix <- self$RBesT_posterior

      smix <- summary(self$RBesT_posterior)
    },

    #' @description Prior mixture components for each replicate.
    #' @param target_data Target study data.
    #' @param samples Data frame of generated replicates.
    #' @return A list with `weights`, `means` and `sds`.
    vectorised_prior_components = function(target_data, samples) {
      list(weights = 1, means = self$prior_mean, sds = sqrt(self$prior_var))
    }
  )
)


#' MCMCModel class
#'
#' @description This class represents a Bayesian borrowing model using MCMC sampling.
#' It inherits from the Model class.
#'
#' @field stan_model_code Code of the Stan model
#' @field stan_model The compiled Stan model
#' @field summary_variables Variables to summarise from the posterior draws
#' @field fit The MCMC fit object
#' @field fit_summary Summary of the Stan fit
#' @field treatment_effect_summary Summary statistics of the treatment effect posterior distribution
#' @field credible_interval_97.5 The upper bound of the credible interval
#' @field credible_interval_2.5 The lower bound of the credible interval
#' @field mcmc_config The MCMC configuration parameters
#' @field mcmc_ess MCMC ESS
#' @field n_divergences Number of divergences in MCMC inference
#' @field rhat r-hat statistics
#' @field draws_dir Directory where to store MCMC draws (used by Stan)
#' @field prior_draws Draws from the prior
#' @field prior_pdf_approx Approximation to the prior probability density function
#' @field prior_cdf_approx Approximation to the prior cumulative density function
#' @field posterior_pdf_approx  Approximation to the posterior probability density function
#' @field posterior_cdf_approx  Approximation to the posterior cumulative density function
#'
#' @export
MCMCModel <- R6::R6Class(
  "MCMCModel",
  inherit = Model,
  public = list(
    stan_model = NULL,
    stan_model_code = NULL,
    summary_variables = "target_treatment_effect",
    fit_summary = NULL,
    fit = NULL,
    treatment_effect_summary = NULL,
    credible_interval_97.5 = NULL,
    credible_interval_2.5 = NULL,
    mcmc_config = NULL,
    mcmc_ess = NULL,
    n_divergences = NULL,
    rhat = NULL,
    draws_dir = NULL,
    prior_draws = NULL,
    prior_pdf_approx = NULL,
    prior_cdf_approx = NULL,
    posterior_pdf_approx = NULL,
    posterior_cdf_approx = NULL,

    #' @description Initialize the MCMCModel object
    #' @param prior The prior object
    #' @param mcmc_config The MCMC configuration parameters
    initialize = function(prior, mcmc_config) {
      super$initialize()

      self$mcmc <- TRUE

      self$check_mcmc_config(mcmc_config)
      self$mcmc_config <- mcmc_config
    },

    #' @description Check validity of the MCMC configuration
    #' @param mcmc_config MCMC configuration
    check_mcmc_config = function(mcmc_config) {
      assertions::assert_whole_number(mcmc_config$num_chains)
      assertions::assert_whole_number(mcmc_config$parallel_chains)
      assertions::assert_whole_number(mcmc_config$tune)
      assertions::assert_number(mcmc_config$target_accept)
      if (mcmc_config$target_accept <= 0 || mcmc_config$target_accept >= 1) {
        stop("target_accept must lie strictly between 0 and 1.", call. = FALSE)
      }
      assertions::assert_whole_number(mcmc_config$chain_length)
      assertions::assert_whole_number(mcmc_config$target_ess)
      assertions::assert_number(mcmc_config$rhat_threshold)
      assertions::assert_number(mcmc_config$max_divergence_rate)
      if (mcmc_config$max_divergence_rate < 0 ||
          mcmc_config$max_divergence_rate > 1) {
        stop("max_divergence_rate must lie between 0 and 1.", call. = FALSE)
      }
    },

    #' @description Prepare the data for inference.
    #' Subclasses must implement the 'prepare_data' method.
    #' @param target_data The target data for inference
    #'
    prepare_data = function(target_data) {
      stop("Subclasses must implement the 'prepare_data' method.", call. = FALSE)
    },

    #' @description Perform inference using MCMC sampling
    #' @param target_data The target data for inference
    #'
    inference = function(target_data) {
      self$empirical_bayes_update(target_data)

      data_list <- self$prepare_data(target_data)

      self$check_data(data_list)

      # Sample from the posterior
      self$fit <- self$stan_model$sample(
        data = data_list,
        chains = self$mcmc_config$num_chains,
        parallel_chains = self$mcmc_config$parallel_chains,
        iter_sampling = self$mcmc_config$chain_length,
        iter_warmup = self$mcmc_config$tune,
        adapt_delta = self$mcmc_config$target_accept,
        seed = stan_sampler_seed(),
        refresh = 0,
        show_messages = FALSE,
        output_dir = self$draws_dir
      )

      # Summarise the draws once. The moments, the credible interval bounds and
      # the convergence diagnostics all come out of this single pass, over only
      # the variables the simulation reads.
      self$fit_summary <- summarise_posterior_draws(
        self$fit$draws(variables = self$summary_variables)
      )

      # Extract the summary statistics for the treatment effect parameter
      self$treatment_effect_summary <- self$fit_summary[self$fit_summary$variable == "target_treatment_effect", ]

      self$post_mean <- self$treatment_effect_summary$mean
      self$post_var <- self$treatment_effect_summary$sd ^ 2
      self$post_median <- self$treatment_effect_summary$median

      self$compute_posterior_parameters()

      mcmc_diagnostics <- self$fit$diagnostic_summary()

      # MCMC Effective Sample Size
      self$mcmc_ess <- self$treatment_effect_summary$n_eff

      self$rhat <- self$treatment_effect_summary$rhat

      # number of divergences reported is the sum of the per chain values
      self$n_divergences <- sum(mcmc_diagnostics$num_divergent)

      if (self$mcmc_ess < self$mcmc_config$target_ess) {
        return(paste0("Target MCMC ESS not reached, ESS is : ", self$mcmc_ess)) # Store as warning in the results table
      }

      if (self$rhat > self$mcmc_config$rhat_threshold) {
        return(paste0("Large rhat values: ", self$rhat)) # Store as warning in the results table
      }

      # A divergent transition means the sampler failed to follow the posterior
      # geometry, so the draws may be biased however well behaved rhat and the
      # effective sample size look. Expressed as a rate because chain_length is
      # adapted between replicates.
      divergence_rate <- self$n_divergences /
        (self$mcmc_config$num_chains * self$mcmc_config$chain_length)

      if (divergence_rate > self$mcmc_config$max_divergence_rate) {
        return(paste0(
          "Too many divergent transitions: ",
          self$n_divergences,
          " (rate ",
          divergence_rate,
          ")"
        )) # Store as warning in the results table
      }

      treatment_effect_draws <- self$fit$draws("target_treatment_effect")
      treatment_effect_draws <- unlist(as.list(treatment_effect_draws))
      self$posterior_pdf_approx <- density(treatment_effect_draws) # Kernel Density Estimation
      self$posterior_cdf_approx <- ecdf(treatment_effect_draws)

      if (is.numeric((self$post_mean)) &&
          is.numeric((self$post_var)) &&
          is.numeric(self$post_median)) {
        return("Success")
      } else {
        stop("Non-numeric moments")
      }
    },

    #' @description Calculate the credible interval
    #' @param level The confidence level for the credible interval (default is 0.95)
    #' @return The credible interval as a numeric vector
    #'
    credible_interval = function(level = 0.95) {
      if (level != 0.95) {
        stop("Not implemented for level other than 0.95")
      }
      ci <- c(self$treatment_effect_summary$q2.5,
              self$treatment_effect_summary$q97.5)
      self$credible_interval_97.5 <- self$treatment_effect_summary$q97.5
      self$credible_interval_2.5 <- self$treatment_effect_summary$q2.5
      return(ci)
    },

    #' @description Get the posterior median
    #' @param ... Optional argument
    #' @return The posterior median as a numeric value
    #'
    posterior_median = function(...) {
      return(self$post_median)
    },

    #' @description Sample from the posterior distribution
    #' @param n_samples The number of samples to draw from the posterior distribution
    #' @return The sampled treatment effect values as a numeric vector
    #'
    sample_posterior = function(n_samples) {
      treatment_effect_draws <- self$fit$draws("target_treatment_effect")
      treatment_effect_draws <- unlist(as.list(treatment_effect_draws))
      return(sample(
        treatment_effect_draws,
        size = n_samples,
        replace = TRUE
      ))
    },

    #' @description Compute the posterior parameters. If there are posterior borrowing parameters,
    #' the following method must be overriden in the subclass.
    compute_posterior_parameters = function() {
      return()
    },
    #' @description Draw samples from the prior using MCMC
    draw_mcmc_prior = function() {
      stop("Subclass must implement a draw_mcmc_prior method.")
    },
    #' @description Posterior PDF
    #' @param target_treatment_effect Point at which to evaluate the posterior PDF
    posterior_pdf = function(target_treatment_effect) {
      if (is.null(self$posterior_pdf_approx)){
        treatment_effect_draws <- self$fit$draws("target_treatment_effect")
        treatment_effect_draws <- unlist(as.list(treatment_effect_draws))
        self$posterior_pdf_approx <- density(treatment_effect_draws) # Kernel Density Estimation
      }
      pdf_x <- approx(self$posterior_pdf_approx$x, self$posterior_pdf_approx$y, xout = target_treatment_effect)$y
      return(pdf_x)
    },
    #' @description Calculates the posterior cumulative distribution function (CDF) for a given target treatment effect.
    #'
    #' @param target_treatment_effect The target treatment effect.
    #' @return The posterior CDF.
    posterior_cdf = function(target_treatment_effect) {
      if (is.null(self$posterior_cdf_approx)){
        treatment_effect_draws <- self$fit$draws("target_treatment_effect")
        treatment_effect_draws <- unlist(as.list(treatment_effect_draws))
        self$posterior_cdf_approx <- ecdf(treatment_effect_draws)
      }
      cdf_x <- self$posterior_cdf_approx(target_treatment_effect)
      return(cdf_x)
    },
    #' @description Prior PDF
    #' @param target_treatment_effect Point at which to evaluate the prior PDF
    #' @param n_samples_quantile_estimation Number of samples used to estimate the quantiles of the distribution
    prior_pdf = function(target_treatment_effect, n_samples_quantile_estimation = 10000) {
      if (is.null(self$prior_pdf_approx)){
        treatment_effect_draws <- self$sample_prior(n_samples_quantile_estimation)
        self$prior_pdf_approx <- density(treatment_effect_draws) # Kernel Density Estimation
      }
      pdf_x <- approx(self$prior_pdf_approx$x, self$prior_pdf_approx$y, xout = target_treatment_effect)$y
      return(pdf_x)
    },
    #' @description Prior CDF
    #' @param target_treatment_effect Point at which to evaluate the prior CDF
    #' @param n_samples_quantile_estimation Number of samples used to estimate the quantiles of the distribution
    prior_cdf = function(target_treatment_effect, n_samples_quantile_estimation = 10000) {
      if (is.null(self$prior_cdf_approx)){
        treatment_effect_draws <- self$sample_prior(n_samples_quantile_estimation)
        self$prior_cdf_approx <- ecdf(treatment_effect_draws)
      }
      cdf_x <- self$prior_cdf_approx(target_treatment_effect)
      return(cdf_x)
    },

    #' @description Sample from the prior distribution
    #' @param n_samples Number of samples to draw
    #' @return A vector of samples
    sample_prior = function(n_samples) {
      if (is.null(self$prior_draws)) {
        self$draw_mcmc_prior()
      }
      treatment_effect_draws <- self$prior_draws$draws("target_treatment_effect")
      treatment_effect_draws <- unlist(as.list(treatment_effect_draws))
      return(sample(
        treatment_effect_draws,
        size = n_samples,
        replace = TRUE
      ))
      return(samples)
    }
  )
)

#' BinomialSeparate class
#'
#' @description This class analyses the target study on its own, with a uniform
#' prior on each arm response rate. The posterior is available in closed form,
#' so it inherits from the `BinomialConjugate` class rather than sampling.
#' @field method Method name
#' @export
BinomialSeparate <- R6::R6Class(
  "BinomialSeparate",
  inherit = BinomialConjugate,
  public = list(
    method = "separate",

    #' @description Assemble the event counts of the target study
    #' @param target_data The target data for the analysis.
    #' @return List of event counts the posterior conditions on
    prepare_data = function(target_data) {
      data_list <- list(
        n_treatment = as.integer(target_data$sample_size_treatment),
        n_control = as.integer(target_data$sample_size_control),
        n_successes_treatment = as.integer(
          target_data$sample_size_treatment * target_data$sample$sample_treatment_rate
        ),
        n_successes_control = as.integer(
          target_data$sample_size_control * target_data$sample$sample_control_rate
        )
      )
      return(data_list)
    }
  )
)

#' BinomialPooling class
#'
#' @description This class pools the source and target studies before analysing
#' them with a uniform prior on each arm response rate. The posterior is
#' available in closed form, so it inherits from the `BinomialConjugate` class
#' rather than sampling.
#' @field method Method name
#' @export
BinomialPooling <- R6::R6Class(
  "BinomialPooling",
  inherit = BinomialConjugate,
  public = list(
    method = "pooling",

    #' @description Assemble the pooled event counts of the source and target studies
    #' @param target_data The target data for the analysis.
    #' @return List of event counts the posterior conditions on
    prepare_data = function(target_data) {
      data_list <- list(
        n_treatment = as.integer(
          target_data$sample_size_treatment + self$prior$source$sample_size_treatment
        ),
        n_control = as.integer(
          target_data$sample_size_control + self$prior$source$sample_size_control
        ),
        n_successes_treatment = as.integer(
          target_data$sample_size_treatment * target_data$sample$sample_treatment_rate + self$prior$source$sample_size_treatment *
            self$prior$source$treatment_rate
        ),
        n_successes_control = as.integer(
          target_data$sample_size_control * target_data$sample$sample_control_rate + self$prior$source$sample_size_control * self$prior$source$control_rate
        )
      )
      return(data_list)
    }
  )
)
