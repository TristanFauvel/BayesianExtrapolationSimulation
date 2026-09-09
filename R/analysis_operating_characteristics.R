#' Whether power can be computed analytically for this target data
#'
#' @description The analytical power formulas apply to a normal summary measure
#'   on a continuous endpoint; the remaining endpoints have to be simulated.
#'   Sharing the predicate keeps the power computation and the propagation of
#'   its uncertainty on the same branch.
#'
#' @param target_data Target data object.
#' @param case_study Optional case-study name.
#'
#' @return `TRUE` when power has a closed form for this target data.
#'
#' @keywords internal
uses_analytical_power <- function(target_data, case_study = NULL) {
  target_data$endpoint == "normal" ||
    target_data$endpoint == "continuous" ||
    isTRUE(case_study == "mepolizumab")
}


#' Simulate the p-values of the frequentist test
#'
#' @description Generates `n_replicates` trials and returns the p-value of the
#'   test in each one. Returning the p-values rather than the decisions lets the
#'   power be read off at any significance level without re-simulating.
#'
#' @param target_data Target data object.
#' @param frequentist_test Type of frequentist test to apply.
#' @param theta_0 Boundary of the null hypothesis space.
#' @param alternative Direction of the alternative hypothesis.
#' @param simulation_config Simulation configuration.
#' @param n_replicates Number of trials to simulate.
#'
#' @return A numeric vector of `n_replicates` p-values.
#'
#' @keywords internal
simulate_test_p_values <- function(target_data,
                                   frequentist_test,
                                   theta_0,
                                   alternative,
                                   simulation_config,
                                   n_replicates) {
  if (frequentist_test != "t-test") {
    stop("Only implemented for a t-test.")
  }

  set.seed(simulation_config$seed)

  # Estimate the frequentist OCs for the model in the scenario considered
  # Generate data for n_replicates clinical trials
  target_data_samples <- target_data$generate(n_replicates)

  p_values <- numeric(nrow(target_data_samples))
  for (r in seq_len(nrow(target_data_samples))) {
    target_data$sample <- target_data_samples[r, , drop = FALSE]
    test <- BSDA::tsum.test(
      mean.x = target_data$sample$treatment_effect_estimate,
      mu = theta_0,
      alternative = alternative,
      s.x = target_data$sample$standard_deviation,
      n.x = target_data$sample$sample_size_per_arm
    )
    p_values[r] <- test$p.value
  }

  p_values
}


#' Compute the frequentist power
#'
#' @description This function computes the power of a test for a given significance level
#' @description This function computes the power of a test for a given significance level
#'
#' @param alpha The significance level.
#' @param target_data The target data containing sample size, treatment effect, standard deviation, and summary measure distribution.
#' @param frequentist_test The type of frequentist test ("t-test" or "z-test").
#' @param theta_0 The null hypothesis value.
#' @param target_data Target data object
#' @param frequentist_test Type of frequentist test to apply, either z-test or t-test
#' @param theta_0 Boundary of the null hypothesis space
#' @param null_space Side of the null space, either left or right.
#' @param simulation_config Simulation configuration.
#' @param case_study Optional case-study name.
#' @param n_replicates Number of Monte Carlo replicates for non-analytical power calculations.
#'
#' @return A list containing the power and its confidence interval.
#'
#' @export
compute_freq_power <- function(alpha,
                               target_data,
                               frequentist_test,
                               theta_0,
                               null_space,
                               simulation_config,
                               case_study = NULL,
                               n_replicates = 1000) {
  if (null_space == "left") {
    alternative <- "greater"
  } else if (null_space == "right") {
    alternative <- "less"
  } else {
    stop("Null space must be either 'left' or 'right'")
  }

  if (is.na(alpha)){
    return(list(
      power = NA_real_,
      conf_int_power = rep(NA_real_, 2)
    ))
  }

  assertions::assert_number(target_data$treatment_effect)
  assertions::assert_number(target_data$standard_deviation)
  assertions::assert_number(target_data$sample_size_per_arm)

  power <- NA # Default value in case of an unsupported distribution

  if (target_data$summary_measure_likelihood == "normal") {
    if (uses_analytical_power(target_data, case_study)) {
      # In this case, we use an analytical computation of power
      effect_size <- (target_data$treatment_effect - theta_0) / target_data$standard_deviation

      if (frequentist_test == "t-test") {
        # Use pwr::pwr.t.test for a t-test power calculation
        power <- pwr::pwr.t.test(
          d = effect_size,
          n = target_data$sample_size_per_arm,
          sig.level = alpha,
          type = "one.sample",
          alternative = alternative
        )$power
      } else if (frequentist_test == "z-test") {
        # Calculate the power of the z-test
        power <- pwr::pwr.norm.test(
          d = effect_size,
          n = target_data$sample_size_per_arm,
          sig.level = alpha,
          alternative = alternative
        )$power
      } else {
        stop("Unsupported test type.")
      }

      conf_int_power <- c(power, power)
    } else {
      # In this case, we cannot use an analytical computation of power
      p_values <- simulate_test_p_values(
        target_data = target_data,
        frequentist_test = frequentist_test,
        theta_0 = theta_0,
        alternative = alternative,
        simulation_config = simulation_config,
        n_replicates = n_replicates
      )

      test_decisions <- p_values < alpha
      power <- mean(test_decisions)
      conf_int_power <- binom.test(sum(test_decisions), length(test_decisions), conf.level = 0.95)$conf.int
    }
  } else if (target_data$summary_measure_likelihood == "binomial") {
    # Compute Cohen's h
    h <- pwr::ES.h(target_data$treatment_rate, target_data$control_rate)

    power <- pwr::pwr.2p2n.test(
      h = h,
      n1 = target_data$sample_size_per_arm,
      n2 = target_data$sample_size_per_arm,
      sig.level = alpha,
      alternative = alternative
    )$power

    conf_int_power <- c(power, power)
  } else {
    stop("Unsupported likelihood type.")
  }

  return(list(power = power, conf_int_power = conf_int_power))
}

#' Compute a binomial credible interval
#'
#' @description Computes the Clopper-Pearson (exact) confidence interval for a
#'   binomial proportion estimated from 0/1 samples, along with the overall
#'   mean of the samples.
#'
#' @param samples A vector of 0/1 samples, or a matrix whose rows each contain
#'   a separate set of 0/1 samples.
#' @param confidence_level The confidence level of the interval.
#'
#' @return A list with `mean` (the overall mean of `samples`) and `conf_int`
#'   (a data frame with one row per group, containing `lower` and `upper`
#'   bounds).
#'
#' @export
compute_binomial_credible_interval <- function(samples, confidence_level = 0.95) {
  if (is.matrix(samples)) {
    successes <- rowSums(samples)
    trials <- ncol(samples)
  } else {
    successes <- sum(samples)
    trials <- length(samples)
  }

  conf_int <- binom::binom.confint(successes, trials, conf.level = confidence_level, methods = "exact")

  list(
    mean = mean(samples),
    conf_int = conf_int[, c("lower", "upper")]
  )
}

#' Compute the frequentist power
#'
#' @description This function computes the power of a test for a pooled analysis at a given significance level
#'
#' @param alpha The significance level.
#' @param target_data The target data containing sample size, treatment effect, standard deviation, and summary measure distribution.
#' @param source_data Source study data
#' @param frequentist_test The type of frequentist test ("t-test" or "z-test").
#' @param theta_0 The null hypothesis value.
#' @param target_data Target data object
#' @param frequentist_test Type of frequentist test to apply, either z-test or t-test
#' @param theta_0 Boundary of the null hypothesis space
#' @param null_space Side of the null space, either left or right.
#' @param simulation_config Simulation configuration.
#' @param case_study Optional case-study name.
#' @param n_replicates Number of Monte Carlo replicates for non-analytical power calculations.
#'
#' @return The power of the test.
#'
#' @export
compute_freq_power_pooling <- function(alpha,
                                       target_data,
                                       source_data,
                                       frequentist_test,
                                       theta_0,
                                       null_space,
                                       simulation_config,
                                       case_study =  NULL,
                                       n_replicates = 1000) {

  if (null_space == "left") {
    alternative <- "greater"
  } else if (null_space == "right") {
    alternative <- "less"
  } else {
    stop("Null space must be either 'left' or 'right'")
  }

  assertions::assert_number(target_data$treatment_effect)
  assertions::assert_number(target_data$standard_deviation)
  assertions::assert_number(target_data$sample_size_per_arm)

  power <- NA # Default value in case of an unsupported distribution

  if (target_data$summary_measure_likelihood == "normal") {
    if (target_data$endpoint == "normal"  || target_data$endpoint == "continuous"  || case_study == "mepolizumab"){
      target_treatment_effect_standard_error <- target_data$standard_deviation / sqrt(target_data$sample_size_per_arm)

      pooled_treatment_effect <- (
        source_data$treatment_effect_estimate / (
          source_data$standard_error ^ 2 / target_treatment_effect_standard_error ^
            2 + 1
        )
      ) + (
        target_data$treatment_effect / (
          1 + target_treatment_effect_standard_error ^ 2 / source_data$standard_error ^
            2
        )
      )


      pooled_standard_error_2 <- 1 / (1 / source_data$standard_error ^ 2 + 1 / target_treatment_effect_standard_error ^
                                        2)

      pooled_variance <- pooled_standard_error_2 * (
        target_data$sample_size_per_arm + source_data$equivalent_source_sample_size_per_arm
      )

      effect_size <- (pooled_treatment_effect - theta_0) / sqrt(pooled_variance)


      if (frequentist_test == "t-test") {
        # Use pwr::pwr.t.test for a t-test power calculation
        power <- pwr::pwr.t.test(
          d = effect_size,
          n = target_data$sample_size_per_arm + source_data$equivalent_source_sample_size_per_arm,
          sig.level = alpha,
          type = "one.sample",
          alternative = alternative
        )$power
      } else if (frequentist_test == "z-test") {
        # Calculate the power of the z-test
        power <- pwr::pwr.norm.test(
          d = effect_size,
          n = target_data$sample_size_per_arm + source_data$equivalent_source_sample_size_per_arm,
          sig.level = alpha,
          alternative = alternative
        )$power
      } else {
        stop("Only implemented for a t-test or z-test.")
      }
      conf_int_power <- c(power, power)
    } else {
      # In this case, we cannot use an analytical computation of power
      set.seed(simulation_config$seed)

      # Estimate the frequentist OCs for the model in the scenario considered
      # Generate data for n_replicates clinical trials
      target_data_samples <- target_data$generate(n_replicates)

      test_decisions <- numeric(n_replicates)
      for (r in seq_len(nrow(target_data_samples))) {
        target_data$sample <- target_data_samples[r, , drop = FALSE]

        target_treatment_effect_standard_error <- target_data$sample$standard_deviation / sqrt(target_data$sample$sample_size_per_arm)

        pooled_treatment_effect <- (
          source_data$treatment_effect_estimate / (
            source_data$standard_error ^ 2 / target_treatment_effect_standard_error ^
              2 + 1
          )
        ) + (
          target_data$sample$treatment_effect_estimate / (
            1 + target_treatment_effect_standard_error ^ 2 / source_data$standard_error ^
              2
          )
        )


        pooled_standard_error_2 <- 1 / (1 / source_data$standard_error ^ 2 + 1 / target_treatment_effect_standard_error ^
                                          2)

        pooled_sample_size <- target_data$sample_size_per_arm + source_data$equivalent_source_sample_size_per_arm

        pooled_variance <- pooled_standard_error_2 * pooled_sample_size

        if (frequentist_test == 't-test'){
          if (null_space == "left"){
            alternative = "greater"
          } else if (null_space == "right"){
            alternative = "less"
          }

          test <- BSDA::tsum.test(
            mean.x = pooled_treatment_effect,
            mu = theta_0,
            alternative = alternative,
            s.x = sqrt(pooled_variance),
            n.x = pooled_sample_size
          )
        } else {
          stop("Only implemented for a t-test.")
        }
        test_decisions[r] <- test$p.value < alpha
      }

      power <- mean(test_decisions)
      conf_int_power <- binom.test(sum(test_decisions), length(test_decisions), conf.level = 0.95)$conf.int
    }
  } else if (target_data$summary_measure_likelihood == "binomial") {
    pooled_sample_size_treatment <- target_data$sample_size_per_arm + source_data$sample_size_treatment
    treatment_rate_pooled <- (
      target_data$treatment_rate * target_data$sample_size_per_arm + source_data$treatment_rate * source_data$sample_size_treatment
    ) / (pooled_sample_size_treatment)

    pooled_sample_size_control <- target_data$sample_size_per_arm + source_data$sample_size_control
    control_rate_pooled <- (
      target_data$control_rate * target_data$sample_size_per_arm + source_data$control_rate * source_data$sample_size_control
    ) / (pooled_sample_size_control)

    # Compute Cohen's h
    h <- pwr::ES.h(treatment_rate_pooled, control_rate_pooled)
    power <- pwr::pwr.2p2n.test(
      h = h,
      n1 = pooled_sample_size_treatment,
      n2 = pooled_sample_size_control,
      sig.level = alpha,
      alternative = alternative
    )$power

    conf_int_power <- c(power, power)
  } else {
    stop("This likelihood is not supported.")
  }

  assertions::assert_number(power)
  return(list(power = power, conf_int_power = conf_int_power))
}


#' Draw plausible values of the equivalent type I error
#'
#' @description Draws from the posterior of the type I error implied by the
#'   replicates it was estimated from, rather than from a normal centred on the
#'   estimate whose spread is read off the width of an exact interval.
#'
#' @param alpha A list with the type I error estimate (`mean`), its exact
#'   interval bounds and, when available, its Monte Carlo standard error
#'   (`mcse`) or replicate count (`n_replicates`).
#' @param n_samples Number of draws to return.
#'
#' @return A numeric vector of `n_samples` draws, or `NA` when the replicate
#'   count behind the estimate cannot be recovered.
#'
#' @keywords internal
sample_equivalent_tie <- function(alpha, n_samples) {
  if (alpha$conf_int_upper <= alpha$conf_int_lower) {
    # The type I error carries no uncertainty of its own, so every draw sits at
    # the estimate and the simulated power supplies the only spread.
    return(rep(alpha$mean, n_samples))
  }

  n_replicates <- alpha$n_replicates
  if (is.null(n_replicates) || is.na(n_replicates)) {
    n_replicates <- binomial_replicate_count(
      estimate = alpha$mean,
      mcse = if (is.null(alpha$mcse)) NA_real_ else alpha$mcse,
      conf_int_lower = alpha$conf_int_lower,
      conf_int_upper = alpha$conf_int_upper
    )
  }

  sample_binomial_proportion(n_samples, alpha$mean, n_replicates)
}


#' Compute the frequentist power at an estimated type I error
#'
#' @description Propagates the uncertainty of the equivalent type I error, and
#'   the Monte Carlo error of the power itself, into an interval for the power a
#'   separate frequentist analysis would reach at that type I error.
#'
#'   The reported bounds are quantiles of the resulting posterior for the power,
#'   not a frequentist confidence interval; they are stored under the existing
#'   `conf_int_power` name for continuity with the columns downstream.
#'
#' @param alpha A list describing the estimated type I error: its `mean`, its
#'   exact interval bounds, and its `mcse` or `n_replicates` when available.
#' @param target_data Target data object.
#' @param frequentist_test Type of frequentist test to apply, either z-test or t-test.
#' @param theta_0 Boundary of the null hypothesis space.
#' @param null_space Side of the null space, either left or right.
#' @param simulation_config Simulation configuration.
#' @param case_study Optional case-study name.
#' @param n_replicates Number of Monte Carlo replicates for non-analytical power.
#' @param n_samples Number of draws of the type I error.
#'
#' @return A list with the power, its interval, and the number of draws used.
#'
#' @export
compute_power_with_tie_ci <- function(alpha,
                                      target_data,
                                      frequentist_test,
                                      theta_0,
                                      null_space,
                                      simulation_config,
                                      case_study = NULL,
                                      n_replicates = 1000,
                                      n_samples = 1000) {
  missing_result <- list(
    power = NA_real_,
    conf_int_power = rep(NA_real_, 2),
    n_effective_samples = 0L
  )

  if (is.na(alpha$mean) || is.na(alpha$conf_int_upper) || is.na(alpha$conf_int_lower)) {
    return(missing_result)
  }

  alpha_samples <- sample_equivalent_tie(alpha, n_samples)
  # A degenerate type I error of zero or one carries no information about the
  # power, so the whole estimate is reported as missing rather than some draws
  # being dropped from an otherwise usable sample.
  if (anyNA(alpha_samples) || any(alpha_samples <= 0) || any(alpha_samples >= 1)) {
    return(missing_result)
  }

  if (uses_analytical_power(target_data, case_study)) {
    # Power is a deterministic function of alpha here, so the type I error is
    # the only source of uncertainty.
    power_samples <- vapply(alpha_samples, function(sampled_alpha) {
      compute_freq_power(
        alpha = sampled_alpha,
        target_data = target_data,
        frequentist_test = frequentist_test,
        theta_0 = theta_0,
        null_space = null_space,
        case_study = case_study,
        simulation_config = simulation_config,
        n_replicates = n_replicates
      )$power
    }, numeric(1))
  } else {
    # Simulate the trials once and read the rejection count off the same
    # p-values at every sampled alpha, then propagate the Monte Carlo error of
    # each count through its own posterior. The reported interval therefore
    # carries both the type I error uncertainty and the replicate noise, which
    # re-simulating under a fixed seed used to suppress entirely.
    alternative <- if (null_space == "left") "greater" else "less"
    p_values <- simulate_test_p_values(
      target_data = target_data,
      frequentist_test = frequentist_test,
      theta_0 = theta_0,
      alternative = alternative,
      simulation_config = simulation_config,
      n_replicates = n_replicates
    )
    rejections <- vapply(alpha_samples, function(sampled_alpha) {
      sum(p_values < sampled_alpha)
    }, numeric(1))
    power_samples <- stats::rbeta(
      n_samples,
      shape1 = rejections + 0.5,
      shape2 = length(p_values) - rejections + 0.5
    )
  }

  list(
    power = mean(power_samples, na.rm = TRUE),
    conf_int_power = unname(
      stats::quantile(power_samples, probs = c(0.025, 0.975), na.rm = TRUE)
    ),
    n_effective_samples = length(alpha_samples)
  )
}


#' Compute the frequentist power at equivalent tie
#'
#' @description This function computes the frequentist power at equivalent tie for a given set of results and analysis configuration.
#'
#' @param results The results data frame.
#' @param analysis_config The analysis configuration.
#'
#' @return The final results data frame with power and frequentist test columns added.
#'
#' @export
frequentist_power_at_equivalent_tie <- function(results, analysis_config, simulation_config, parallelization = FALSE) {
  if (nrow(results) == 0) {
    stop("The results dataframe is empty.")
  }

  # Remove the tie, mcse_tie, conf_int_tie_lower and conf_int_tie_upper if they exist
  results <- results[, !(
    names(results) %in% c(
      "tie",
      "mcse_tie",
      "conf_int_tie_lower",
      "conf_int_tie_upper",
      "frequentist_power_at_equivalent_tie",
      "frequentist_power_at_equivalent_tie_lower",
      "frequentist_power_at_equivalent_tie_upper",
      "frequentist_test"
    )
  )]

  matching_columns <- c(
    "method",
    "parameters",
    "control_drift",
    "source_denominator",
    "source_denominator_change_factor",
    "case_study",
    "target_to_source_std_ratio",
    "target_sample_size_per_arm",
    "theta_0",
    "null_space",
    "sampling_approximation",
    "summary_measure_likelihood",
    "source_sample_size_treatment",
    "source_sample_size_control",
    "endpoint",
    "source_standard_error",
    "source_treatment_effect_estimate",
    "equivalent_source_sample_size_per_arm"
  )

  for (case_study in unique(results$case_study)){
    if (sum(results[results$case_study == case_study, ]['target_treatment_effect'] == results$theta_0) == 0){
      stop("theta_0 not included among the target study treatment effects considered in the simulation study!")
    }
  }

  # Select the results that correspond to TIE computation.
  results_freq_df_tie <- results[results$target_treatment_effect == results$theta_0, c(
    matching_columns,
    c(
      "success_proba",
      "mcse_success_proba",
      "conf_int_success_proba_lower",
      "conf_int_success_proba_upper"
    )
  )]

  # For these results the probability of success corresponds to TIE, so we rename the columns accordingly.
  results_freq_df_tie <- results_freq_df_tie %>%
    dplyr::rename(
      tie = success_proba,
      mcse_tie = mcse_success_proba,
      conf_int_tie_lower = conf_int_success_proba_lower,
      conf_int_tie_upper = conf_int_success_proba_upper
    )

  # We merge the TIE results onto the corresponding scenarios of the results dataframe.
  results <- dplyr::left_join(results, results_freq_df_tie, by = matching_columns)

  frequentist_test <- analysis_config[["frequentist_test"]]

  if (parallelization == TRUE){
    # Set up parallel backend
    n_cores <- get_parallel_worker_count()
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)

    required_libraries <- c(
      "RBExT", "pwr", "dplyr", "yaml", "BSDA"
    )

    # Export necessary functions and objects to the cluster
    paths <- .libPaths()
    parallel::clusterExport(cl,
                  varlist = c("paths", "required_libraries"),
                  envir = environment())

    # Load required libraries in workers
    parallel::clusterEvalQ(cl, {
      .libPaths(paths)
      sapply(required_libraries, library, character.only = TRUE)
    })

    # Use foreach for parallel computation
    results_list <- foreach(i = seq_len(nrow(results)), .packages = c("dplyr", "yaml", "pwr", "BSDA")) %dopar% {
      if (is.na(results$tie[i])) {
        warning("TIE is NA")
        return(list(
          frequentist_power_at_equivalent_tie = NA_real_,
          frequentist_power_at_equivalent_tie_lower = NA_real_,
          frequentist_power_at_equivalent_tie_upper = NA_real_,
          frequentist_test = NA_character_
        ))
      }

      target_data <- load_data(results[i, ], type = "target", reload_data_objects = TRUE)
      source_data <- load_data(results[i, ], type = "source", reload_data_objects = TRUE)

      alpha <- list(
        mean = results$tie[i],
        conf_int_lower = results$conf_int_tie_lower[i],
        conf_int_upper = results$conf_int_tie_upper[i],
        mcse = results$mcse_tie[i]
      )

      power_estimation <- compute_power_with_tie_ci(
        alpha = alpha,
        target_data = target_data,
        frequentist_test = frequentist_test,
        theta_0 = results$theta_0[i],
        null_space = results$null_space[i],
        case_study = results[i, ]$case_study,
        simulation_config = simulation_config
      )

      list(
        frequentist_power_at_equivalent_tie = power_estimation$power,
        frequentist_power_at_equivalent_tie_lower = power_estimation$conf_int_power[1],
        frequentist_power_at_equivalent_tie_upper = power_estimation$conf_int_power[2],
        frequentist_test = frequentist_test
      )
    }

    # Stop parallel backend
    parallel::stopCluster(cl)

    # Combine results into the dataframe
    results <- cbind(results, dplyr::bind_rows(results_list))
  } else {
    # Progress bar function in R
    progress_bar <- function(n) {
      pb <- txtProgressBar(min = 0,
                           max = n,
                           style = 3)
      return(function(i) {
        setTxtProgressBar(pb, i)
      })
    }

    frequentist_test <- analysis_config[["frequentist_test"]]
    # Iterate through rows and compute power at equivalent TIE.
    for (i in seq_len(nrow(results))) {
      target_data <- load_data(results[i, ],
                               type = "target",
                               reload_data_objects = TRUE)

      source_data <- load_data(results[i, ],
                               type = "source",
                               reload_data_objects = TRUE)

      if (is.na(results$tie[i])){
        warning("TIE is NA")
        next
      }

      alpha = list(mean = results$tie[i], conf_int_lower = results$conf_int_tie_lower[i], conf_int_upper = results$conf_int_tie_upper[i], mcse = results$mcse_tie[i])

      power_estimation <- compute_power_with_tie_ci(
        alpha = alpha,
        target_data = target_data,
        frequentist_test = frequentist_test,
        theta_0 = results$theta_0[i],
        null_space = results$null_space[i],
        case_study =  results[i, ]$case_study,
        simulation_config = simulation_config
      )

      results$frequentist_power_at_equivalent_tie[i] <- power_estimation$power
      results$frequentist_power_at_equivalent_tie_lower[i] <- power_estimation$conf_int_power[1]
      results$frequentist_power_at_equivalent_tie_upper[i] <- power_estimation$conf_int_power[2]

    results$frequentist_test[i] <- frequentist_test

    # Update progress bar
    pb <- progress_bar(nrow(results))(i)
    }
  }

  return(results)
}


frequentist_power_at_nominal_tie <- function(results, analysis_config, simulation_config) {
  if (nrow(results) == 0) {
    stop("The results dataframe is empty.")
  }

  results <- results[, !(
    names(results) %in% c(
     "nominal_frequentist_power_separate",
     "nominal_frequentist_power_pooling"
    )
  )]

  nominal_tie <- analysis_config[["nominal_tie"]]
  frequentist_test <- analysis_config[["frequentist_test"]]

  # Initialize the new columns
  results$nominal_frequentist_power_separate <- NA
  results$nominal_frequentist_power_pooling <- NA

  # Initialize the new columns
  results$nominal_frequentist_power_separate <- NA
  results$nominal_frequentist_power_pooling <- NA

  # Progress bar function in R
  progress_bar <- function(n) {
    pb <- txtProgressBar(min = 0,
                         max = n,
                         style = 3)
    return(function(i) {
      setTxtProgressBar(pb, i)
    })
  }

  # Iterate through rows and compute power
  for (i in seq_len(nrow(results))) {
    target_data <- load_data(results[i, ],
                             type = "target",
                             reload_data_objects = TRUE)
    source_data <- load_data(results[i, ],
                             type = "source",
                             reload_data_objects = TRUE)



    nominal_frequentist_power_separate <- compute_freq_power(
      alpha = nominal_tie,
      target_data = target_data,
      frequentist_test = frequentist_test,
      theta_0 = results$theta_0[i],
      null_space = results$null_space[i],
      case_study =  results[i, ]$case_study,
      simulation_config = simulation_config
    )

    results$nominal_frequentist_power_separate[i] <- nominal_frequentist_power_separate$power

    nominal_frequentist_power_pooling <- compute_freq_power_pooling(
      alpha = nominal_tie,
      target_data = target_data,
      source_data = source_data,
      frequentist_test = frequentist_test,
      theta_0 = results$theta_0[i],
      null_space = results$null_space[i],
      case_study =  results[i, ]$case_study,
      simulation_config = simulation_config
    )

    results$nominal_frequentist_power_pooling[i] <- nominal_frequentist_power_pooling$power

    # Update progress bar
    pb <- progress_bar(nrow(results))(i)
  }
  return(results)
}
