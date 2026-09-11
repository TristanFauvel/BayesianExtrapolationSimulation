frequentist_col_types <- cols(
  method = col_character(),
  parameters = col_character(),
  target_sample_size_per_arm = col_double(),
  drift = col_double(),
  treatment_drift = col_double(),
  control_drift = col_double(),
  source_denominator = col_double(),
  source_denominator_change_factor = col_double(),
  case_study = col_character(),
  parallelization = col_logical(),
  sampling_approximation = col_logical(),
  source_treatment_effect_estimate = col_double(),
  target_treatment_effect = col_double(),
  target_to_source_std_ratio = col_double(),
  theta_0 = col_double(),
  null_space = col_character(),
  summary_measure_likelihood = col_character(),
  target_standard_deviation = col_double(),
  target_treatment_rate = col_double(),
  target_control_rate = col_double(),
  source_standard_error = col_double(),
  source_sample_size_control = col_double(),
  source_sample_size_treatment = col_double(),
  equivalent_source_sample_size_per_arm = col_double(),
  endpoint = col_character(),
  source_control_rate = col_double(),
  source_treatment_rate = col_double(),
  rng_state = col_character(),
  computation_time = col_double(),
  success_proba = col_double(),
  mcse_success_proba = col_double(),
  conf_int_success_proba_lower = col_double(),
  conf_int_success_proba_upper = col_double(),
  coverage = col_double(),
  conf_int_coverage_lower = col_double(),
  conf_int_coverage_upper = col_double(),
  mse = col_double(),
  conf_int_mse_lower = col_double(),
  conf_int_mse_upper = col_double(),
  bias = col_double(),
  conf_int_bias_lower = col_double(),
  conf_int_bias_upper = col_double(),
  posterior_mean = col_double(),
  conf_int_posterior_mean_lower = col_double(),
  conf_int_posterior_mean_upper = col_double(),
  posterior_median = col_double(),
  conf_int_posterior_median_lower = col_double(),
  conf_int_posterior_median_upper = col_double(),
  precision = col_double(),
  conf_int_precision_lower = col_double(),
  conf_int_precision_upper = col_double(),
  credible_interval_lower = col_double(),
  credible_interval_upper = col_double(),
  posterior_parameters = col_character(),
  ess_moment = col_double(),
  conf_int_ess_moment_lower = col_double(),
  conf_int_ess_moment_upper = col_double(),
  ess_precision = col_double(),
  conf_int_ess_precision_lower = col_double(),
  conf_int_ess_precision_upper = col_double(),
  ess_elir = col_double(),
  conf_int_ess_elir_lower = col_double(),
  conf_int_ess_elir_upper = col_double(),
  rhat = col_double(),
  conf_int_rhat_lower = col_double(),
  conf_int_rhat_upper = col_double(),
  mcmc_ess = col_double(),
  conf_int_mcmc_ess_lower = col_double(),
  conf_int_mcmc_ess_upper = col_double(),
  n_divergences = col_double(),
  conf_int_n_divergences_lower = col_double(),
  conf_int_n_divergences_upper = col_double(),
  warning = col_logical(),
  tie = col_double(),
  mcse_tie = col_double(),
  conf_int_tie_lower = col_double(),
  conf_int_tie_upper = col_double(),
  frequentist_power_at_equivalent_tie = col_double(),
  frequentist_power_at_equivalent_tie_lower = col_double(),
  frequentist_power_at_equivalent_tie_upper = col_double(),
  frequentist_test = col_character(),
  nominal_frequentist_power_separate = col_double(),
  nominal_frequentist_power_pooling = col_double()
)


bayesian_col_types <- cols(
  case_study = col_character(),
  method = col_character(),
  target_sample_size_per_arm = col_double(),
  parameters = col_character(),
  source_denominator_change_factor = col_double(),
  target_to_source_std_ratio = col_double(),
  design_prior_type = col_character(),
  prior_proba_success = col_double(),
  prior_proba_no_benefit = col_double(),
  prior_proba_benefit = col_double(),
  prepost_proba_FP = col_double(),
  prepost_proba_TP = col_double(),
  average_tie = col_double(),
  average_power = col_double(),
  upper_bound_proba_FP = col_double(),
  source_treatment_effect_estimate = col_double(),
  source_standard_error = col_double(),
  endpoint = col_character(),
  summary_measure_likelihood = col_character(),
  source_sample_size_control = col_double(),
  source_sample_size_treatment = col_double(),
  equivalent_source_sample_size_per_arm = col_double()
)


sweet_spot_col_types <- cols(
  method = col_character(),
  parameters = col_character(),
  target_sample_size_per_arm = col_double(),
  control_drift = col_double(),
  source_denominator = col_double(),
  source_denominator_change_factor = col_double(),
  case_study = col_character(),
  parallelization = col_logical(),
  sampling_approximation = col_logical(),
  source_treatment_effect_estimate = col_double(),
  target_to_source_std_ratio = col_double(),
  theta_0 = col_double(),
  null_space = col_character(),
  summary_measure_likelihood = col_character(),
  source_standard_error = col_double(),
  source_sample_size_control = col_double(),
  source_sample_size_treatment = col_double(),
  equivalent_source_sample_size_per_arm = col_double(),
  endpoint = col_character(),
  source_control_rate = col_double(),
  source_treatment_rate = col_double(),
  sweet_spot_lower = col_double(),
  sweet_spot_upper = col_double(),
  sweet_spot_width = col_double(),
  drift_range_upper = col_double(),
  drift_range_lower = col_double(),
  metric = col_character()
)


scenario_columns <- c("target_sample_size_per_arm", "control_drift", "source_denominator", "source_denominator_change_factor",
                      "case_study", "sampling_approximation", "source_treatment_effect_estimate",
                      "target_to_source_std_ratio", "theta_0", "null_space",
                      "summary_measure_likelihood", "source_standard_error", "source_sample_size_control",
                      "source_sample_size_treatment", "equivalent_source_sample_size_per_arm", "endpoint",
                      "source_control_rate", "source_treatment_rate")

unique_scenario_columns <- c("case_study", "target_sample_size_per_arm", "source_denominator_change_factor",
                      "target_to_source_std_ratio")


results_columns <- c("success_proba", "mcse_success_proba", "conf_int_success_proba_lower",
                     "conf_int_success_proba_upper", "coverage", "conf_int_coverage_lower",
                     "conf_int_coverage_upper", "mse", "conf_int_mse_lower", "conf_int_mse_upper",
                     "bias", "conf_int_bias_lower", "conf_int_bias_upper", "posterior_mean",
                     "conf_int_posterior_mean_lower", "conf_int_posterior_mean_upper", "posterior_median",
                     "conf_int_posterior_median_lower", "conf_int_posterior_median_upper", "precision",
                     "conf_int_precision_lower", "conf_int_precision_upper", "credible_interval_lower",
                     "credible_interval_upper", "posterior_parameters", "ess_moment",
                     "conf_int_ess_moment_lower", "conf_int_ess_moment_upper", "ess_precision",
                     "conf_int_ess_precision_lower", "conf_int_ess_precision_upper", "ess_elir",
                     "conf_int_ess_elir_lower", "conf_int_ess_elir_upper", "rhat", "conf_int_rhat_lower",
                     "conf_int_rhat_upper", "mcmc_ess", "conf_int_mcmc_ess_lower", "conf_int_mcmc_ess_upper",
                     "n_divergences", "conf_int_n_divergences_lower", "conf_int_n_divergences_upper",
                     "warning", "tie", "mcse_tie", "conf_int_tie_lower", "conf_int_tie_upper",
                     "frequentist_power_at_equivalent_tie", "frequentist_power_at_equivalent_tie_lower",
                     "frequentist_power_at_equivalent_tie_upper", "frequentist_test",
                     "nominal_frequentist_power_separate", "nominal_frequentist_power_pooling")


expected_colnames_scenario <- c(
  "method",
  "parameters",
  "target_sample_size_per_arm",
  "drift",
  "treatment_drift",
  "control_drift",
  "source_denominator",
  "source_denominator_change_factor",
  "case_study",
  "parallelization",
  "sampling_approximation",
  "source_treatment_effect_estimate",
  "target_treatment_effect",
  "target_to_source_std_ratio",
  "theta_0",
  "null_space"
)

expected_colnames_results <- c(
  "success_proba",
  "mcse_success_proba",
  "conf_int_success_proba_lower",
  "conf_int_success_proba_upper",
  "coverage",
  "conf_int_coverage_lower",
  "conf_int_coverage_upper",
  "mse",
  "conf_int_mse_lower",
  "conf_int_mse_upper",
  "bias",
  "conf_int_bias_lower",
  "conf_int_bias_upper",
  "posterior_mean",
  "conf_int_posterior_mean_lower",
  "conf_int_posterior_mean_upper",
  "posterior_median",
  "conf_int_posterior_median_lower",
  "conf_int_posterior_median_upper",
  "precision",
  "conf_int_precision_lower",
  "conf_int_precision_upper",
  "credible_interval_lower",
  "credible_interval_upper",
  "posterior_parameters",
  "ess_moment",
  "conf_int_ess_moment_lower",
  "conf_int_ess_moment_upper",
  "ess_precision",
  "conf_int_ess_precision_lower",
  "conf_int_ess_precision_upper",
  "ess_elir",
  "conf_int_ess_elir_lower",
  "conf_int_ess_elir_upper",
  "rhat",
  "conf_int_rhat_lower",
  "conf_int_rhat_upper",
  "mcmc_ess",
  "conf_int_mcmc_ess_lower",
  "conf_int_mcmc_ess_upper",
  "n_divergences",
  "conf_int_n_divergences_lower",
  "conf_int_n_divergences_upper",
  "warning"
)

expected_colnames_source <- c(
  "source_treatment_effect_estimate",
  "source_standard_error",
  "source_sample_size_control",
  "source_sample_size_treatment",
  "equivalent_source_sample_size_per_arm",
  "summary_measure_likelihood",
  "endpoint",
  "source_control_rate",
  "source_treatment_rate"
)


#' Column types expected across the simulation results
#'
#' @description Maps the column names shared by the scenario, source and
#'   results frames to the type each is expected to hold. Only columns whose
#'   type is load-bearing downstream are listed; `check_colnames()` ignores
#'   columns absent from the spec.
#'
#' @keywords internal
expected_coltypes <- c(
  # Scenario
  method = "character",
  parameters = "list|character",
  target_sample_size_per_arm = "numeric",
  drift = "numeric",
  treatment_drift = "numeric",
  control_drift = "numeric",
  source_denominator = "numeric",
  source_denominator_change_factor = "numeric",
  case_study = "character",
  sampling_approximation = "logical",
  source_treatment_effect_estimate = "numeric",
  target_treatment_effect = "numeric",
  target_to_source_std_ratio = "numeric",
  theta_0 = "numeric",
  null_space = "character",
  # Source data
  source_standard_error = "numeric",
  source_sample_size_control = "numeric",
  source_sample_size_treatment = "numeric",
  equivalent_source_sample_size_per_arm = "numeric",
  summary_measure_likelihood = "character",
  endpoint = "character",
  # Results
  success_proba = "numeric",
  mcse_success_proba = "numeric",
  conf_int_success_proba_lower = "numeric",
  conf_int_success_proba_upper = "numeric",
  coverage = "numeric",
  mse = "numeric",
  bias = "numeric",
  posterior_mean = "numeric",
  posterior_median = "numeric",
  precision = "numeric",
  credible_interval_lower = "numeric",
  credible_interval_upper = "numeric",
  posterior_parameters = "list|character",
  ess_moment = "numeric",
  ess_precision = "numeric",
  ess_elir = "numeric",
  rhat = "numeric",
  mcmc_ess = "numeric",
  n_divergences = "numeric",
  # Derived by the analysis layer
  tie = "numeric",
  conf_int_tie_lower = "numeric",
  conf_int_tie_upper = "numeric",
  frequentist_power_at_equivalent_tie = "numeric",
  frequentist_power_at_equivalent_tie_lower = "numeric",
  frequentist_power_at_equivalent_tie_upper = "numeric",
  nominal_frequentist_power_separate = "numeric",
  nominal_frequentist_power_pooling = "numeric"
)


#' Columns every consumer of a results frame relies on
#'
#' @description The identity columns that the analysis, plot and table layers
#'   read by name from whichever results frame they are handed. `drift` is
#'   deliberately absent: the Bayesian operating-characteristics frame does not
#'   carry it, and these columns must hold for both.
#'
#' @keywords internal
required_colnames_consumer <- c(
  "case_study",
  "method",
  "parameters",
  "target_sample_size_per_arm",
  "source_denominator_change_factor"
)


#' Look up the expected types for a set of columns
#'
#' @param expected_colnames The column names to look up.
#'
#' @return The subset of [expected_coltypes] describing those columns. Columns
#'   the spec does not describe are omitted rather than reported, so that a
#'   frame carrying extra bookkeeping columns still passes.
#'
#' @keywords internal
default_coltypes <- function(expected_colnames) {
  expected_coltypes[intersect(names(expected_coltypes), expected_colnames)]
}


#' Predicates used to check a column against a declared type
#'
#' @description The vocabulary `check_colnames()` accepts in its `types`
#'   argument. `numeric` deliberately accepts integer columns, since sample
#'   sizes and replicate counts are read back as either.
#'
#' @keywords internal
column_type_predicates <- list(
  numeric = is.numeric,
  integer = is.integer,
  character = is.character,
  logical = is.logical,
  factor = is.factor,
  list = is.list
)


#' Describe the type a column actually holds
#'
#' @param column A dataframe column.
#'
#' @return A single string naming the column's type, for use in error messages.
#'
#' @keywords internal
describe_column_type <- function(column) {
  paste(class(column), collapse = "/")
}


#' Check columns in a dataframe
#'
#' This function checks if a dataframe contains the expected columns and only the expected columns.
#' When `types` is supplied it additionally checks that those columns hold the
#' declared type, reporting every mismatch at once.
#'
#' @param df A dataframe to check.
#' @param expected_colnames A character vector of column names the dataframe should contain.
#' @param types An optional named character vector mapping column names to the
#'   type they are expected to hold, using the vocabulary of
#'   [column_type_predicates]. Names must be among `expected_colnames`; columns
#'   the spec does not mention are not type checked.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
check_colnames <- function(df,
                           expected_colnames,
                           types = default_coltypes(expected_colnames)) {
  missing_cols <- setdiff(expected_colnames, colnames(df))
  if (length(missing_cols) > 0) {
    error_message <- paste(
      "The dataframe is missing the following columns:",
      paste(missing_cols, collapse = ", ")
    )
    stop(error_message)
  }

  missing_cols <- setdiff(colnames(df), expected_colnames)
  if (length(missing_cols) > 0) {
    error_message <- paste(
      "The dataframe has unexpected columns:",
      paste(missing_cols, collapse = ", ")
    )
    stop(error_message)
  }

  check_coltypes(df, types, expected_colnames)
}


#' Check that a dataframe carries the columns a consumer relies on
#'
#' @description Unlike [check_colnames()], this tolerates additional columns.
#'   Use it at the entry point of the analysis, plot and table layers, which
#'   receive frames enriched with derived columns but read a known subset of
#'   them by name.
#'
#' @param df A dataframe to check.
#' @param required_colnames A character vector of column names that must be
#'   present.
#' @param types An optional named character vector of expected types; defaults
#'   to the entries of [expected_coltypes] describing the required columns.
#' @param context An optional string naming the caller, included in the error
#'   so that a schema drift points at the consumer that noticed it.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
check_required_colnames <- function(df,
                                    required_colnames,
                                    types = default_coltypes(required_colnames),
                                    context = NULL) {
  prefix <- if (is.null(context)) "" else paste0(context, ": ")

  missing_cols <- setdiff(required_colnames, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste0(
      prefix, "the dataframe is missing the following required columns: ",
      paste(missing_cols, collapse = ", ")
    ))
  }

  tryCatch(
    check_coltypes(df, types, required_colnames),
    error = function(e) stop(paste0(prefix, conditionMessage(e)), call. = FALSE)
  )
}


#' Check the types of a dataframe's columns
#'
#' @param df A dataframe whose columns have already been checked for presence.
#' @param types A named character vector mapping column names to expected types.
#' @param expected_colnames The column names `types` is allowed to mention.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
check_coltypes <- function(df, types, expected_colnames = names(types)) {
  if (length(types) == 0) {
    return(invisible(NULL))
  }

  if (is.null(names(types)) || any(names(types) == "")) {
    stop("The type specification must be a named character vector.")
  }

  alternatives <- strsplit(types, "|", fixed = TRUE)
  unknown_types <- setdiff(unique(unlist(alternatives)), names(column_type_predicates))
  if (length(unknown_types) > 0) {
    stop(paste(
      "The type specification names types that cannot be checked:",
      paste(unknown_types, collapse = ", ")
    ))
  }

  unknown_cols <- setdiff(names(types), expected_colnames)
  if (length(unknown_cols) > 0) {
    stop(paste(
      "The type specification names columns that are not expected:",
      paste(unknown_cols, collapse = ", ")
    ))
  }

  mismatches <- character(0)
  for (column_name in names(types)) {
    column <- df[[column_name]]

    # A column holding nothing but NA carries no type information: R types it
    # as logical regardless of what the column means, so checking it would
    # reject frames that are merely empty for this scenario.
    if (is.atomic(column) && length(column) > 0 && all(is.na(column))) {
      next
    }

    accepted <- alternatives[[column_name]]
    if (!any(vapply(accepted, function(type) column_type_predicates[[type]](column), logical(1)))) {
      mismatches <- c(mismatches, paste0(
        column_name, " (expected ", types[[column_name]],
        ", found ", describe_column_type(column), ")"
      ))
    }
  }

  if (length(mismatches) > 0) {
    stop(paste(
      "The dataframe has columns of the wrong type:",
      paste(mismatches, collapse = "; ")
    ))
  }
}

#' Remove columns from a dataframe
#'
#' This function removes specified columns from a dataframe.
#'
#' @param df A dataframe to modify.
#' @param to_remove A character vector of column names to remove.
#'
#' @return A dataframe with specified columns removed.
#'
#' @keywords internal
remove_columns_from_df <- function(df, to_remove) {
  for (key in to_remove) {
    if (key %in% colnames(df)) {
      df <- df[, -which(colnames(df) == key)]
    }
  }
  return(df)
}

#' Concatenate all simulation results to a global dataframe
#'
#' This function reads all CSV files with names starting with "results" from the specified
#' directory, concatenates them, and saves the result to a new CSV file.
#'
#' @param results_dir A character string specifying the path of the results folder.
#' @param ocs_filename A character string specifying the filename of the results dataframe.
#'
#' @return No return value, called for side effects.
#'
#' @importFrom dplyr bind_rows
#'
#' @keywords internal
concatenate_simulation_results <- function(results_dir, ocs_filename) {
  # Initialize an empty data frame to store the concatenated results
  df_results <- data.frame()

  if (grepl("frequentist", ocs_filename)) {
    ocs_type <- "frequentist"
  } else if (grepl("bayesian", ocs_filename)) {
    ocs_type <- "bayesian"
  } else {
    stop("Wrong filename defined in output config.")
  }
  folder_dir <- file.path(results_dir, ocs_type)

  # List all files in the directory tree
  files <- list.files(folder_dir, recursive = TRUE, full.names = TRUE)

  for (file_path in files) {
    if (grepl("\\.csv$", file_path)) {
      if (startsWith(basename(file_path), "results")) {
        # Read the CSV file
        df <- read.csv(file_path, stringsAsFactors = FALSE)

        # Concatenate to the global data frame
        df_results <- dplyr::bind_rows(df_results, df)
      }
    }
  }

  # Save the global data frame to a new CSV file
  readr::write_csv(df_results, file.path(results_dir, ocs_filename))
}


# Function to concatenate files
concatenate_files <- function(file_name, folders, output_path) {
  if (file_name == "results_frequentist.csv"){
    col_types <- frequentist_col_types
  } else if (file_name == "results_bayesian_simpson.csv"){
    col_types <- bayesian_col_types
  } else if (file_name == "sweet_spot.csv"){
    col_types <- sweet_spot_col_types
  } else {
    col_types <- NULL
  }

  combined_data <- bind_rows(lapply(folders, function(folder) {
    file_path <- file.path(folder, file_name)
    if (file.exists(file_path)) {
      data <- read_csv(file_path, col_types = col_types, col_names = TRUE)
      return(data)
    } else {
      NULL  # Handle cases where the file might not exist in some folders
    }
  }))


  if (!(setequal(colnames(combined_data), names(col_types$cols)))){
    warning("Invalid column names in the concatenated results. Concatenated results file not created.")
  } else {
    # Write the combined data to the root folder
    write_csv(combined_data, file.path(output_path, file_name))
  }
}


#' Concatenate all simulation logs to a global dataframe
#'
#' This function reads all CSV files with names starting with "logs" from both the "bayesian" and
#' "frequentist" subdirectories, concatenates them, adds an "OCs" column, and saves the result to a new CSV file.
#'
#' @param results_dir A character string specifying the path of the results folder.
#'
#' @return No return value, called for side effects.
#'
#' @importFrom dplyr bind_rows select everything
#'
#' @export
concatenate_simulation_logs <- function(results_dir) {
  # Initialize an empty data frame to store the concatenated results
  df_logs <- data.frame()

  ocs_types <- c("bayesian", "frequentist")

  for (ocs_type in ocs_types) {
    folder_dir <- file.path(results_dir, ocs_type)

    # List all files in the directory tree
    files <- list.files(folder_dir, recursive = TRUE, full.names = TRUE)

    for (file_path in files) {
      if (grepl("\\.csv$", file_path)) {
        if (startsWith(basename(file_path), "logs")) {
          # Read the CSV file
          df <- read.csv(file_path, stringsAsFactors = FALSE)
          # Add OCs column
          df$OCs <- ocs_type
          # Concatenate to the global data frame
          df_logs <- dplyr::bind_rows(df_logs, df)
        }
      }
    }
  }
  # Reorder columns to place OCs column at the start
  df_logs <- df_logs %>%
    dplyr::select(OCs, everything())
  # Save the global data frame to a new CSV file
  readr::write_csv(df_logs, file.path(results_dir, "logs.csv"))
}


#' Format simulation output as a printable table
#'
#' @param output A simulation output list (as returned by e.g.
#'   `Model$estimate_frequentist_operating_characteristics()`).
#'
#' @return A data frame with one row per metric, suitable for printing.
#'
#' @export
format_simulation_output_table <- function(output) {
  # Extract the data
  data <- list(
    "Success Probability" = c(
      output$success_proba,
      output$conf_int_success_proba_lower,
      output$conf_int_success_proba_upper
    ),
    "Coverage" = c(
      output$coverage,
      output$conf_int_coverage_lower,
      output$conf_int_coverage_upper
    ),
    "MSE" = c(
      output$mse,
      output$conf_int_mse_lower,
      output$conf_int_mse_upper
    ),
    "Bias" = c(
      output$bias,
      output$conf_int_bias_lower,
      output$conf_int_bias_upper
    ),
    "Posterior Mean" = c(
      output$posterior_mean,
      output$conf_int_posterior_mean_lower,
      output$conf_int_posterior_mean_upper
    ),
    "Posterior Median" = c(
      output$posterior_median,
      output$conf_int_posterior_median_lower,
      output$conf_int_posterior_median_upper
    ),
    "Precision" = c(
      output$precision,
      output$conf_int_precision_lower,
      output$conf_int_precision_upper
    ),
    "Credible Interval" = c(
      NA,
      output$credible_interval_lower,
      output$credible_interval_upper
    ),
    "ESS Moment" = c(
      output$ess_moment,
      output$conf_int_ess_moment_lower,
      output$conf_int_ess_moment_upper
    ),
    "ESS Precision" = c(
      output$ess_precision,
      output$conf_int_ess_precision_lower,
      output$conf_int_ess_precision_upper
    ),
    "ESS ELIR" = c(
      output$ess_elir,
      output$conf_int_ess_elir_lower,
      output$conf_int_ess_elir_upper
    )
  )

  # Create a data frame
  df <- data.frame(
    Metric = names(data),
    Value = sapply(data, `[`, 1),
    CI_Lower = sapply(data, `[`, 2),
    CI_Upper = sapply(data, `[`, 3)
  )

  # Format the values
  df$Value <- format(df$Value, digits = 4, scientific = FALSE)
  df$CI_Lower <- format(df$CI_Lower, digits = 4, scientific = FALSE)
  df$CI_Upper <- format(df$CI_Upper, digits = 4, scientific = FALSE)

  # Create the confidence interval column
  df$`95% CI` <- paste0("[", df$CI_Lower, ", ", df$CI_Upper, "]")

  # Select and rename columns
  df <- df[, c("Metric", "Value", "95% CI")]

  # Print the table
  print(df, row.names = FALSE)
}

format_parameters_to_json <- function(json_parameters, escape = FALSE) {
  json_parameters <- jsonlite::toJSON(json_parameters, pretty = TRUE)
  json_parameters <- gsub("\"", "\'", json_parameters)
  json_parameters <- as.character(json_parameters)
  if (escape == TRUE) {
    json_parameters <- paste0("[\n  ", json_parameters, "\n]")
  }
  return(json_parameters)
}


#' Format a case study configuration as a printable table
#'
#' @param case_study_config Case study configuration list.
#'
#' @return A data frame with one row per configuration field, suitable for
#'   printing (used by the data-generation vignettes to describe a case study).
#'
#' @export
format_case_study_config <- function(case_study_config) {
  if (case_study_config$endpoint == "continuous" ||
      case_study_config$endpoint == "recurrent_event") {
    data <- data.frame(
      Parameter = c(
        "Name",
        "Control",
        "Summary Measure Likelihood",
        "Theta 0",
        "Endpoint",
        "Null Space",
        "Sampling Approximation",
        "Control",
        "Treatment",
        "Total",
        "Treatment Effect",
        "Standard Error",
        "Control",
        "Treatment",
        "Total",
        "Treatment Effect",
        "Standard Error"
      ),
      Value = c(
        case_study_config$name,
        case_study_config$control,
        case_study_config$summary_measure_likelihood,
        case_study_config$theta_0,
        case_study_config$endpoint,
        case_study_config$null_space,
        case_study_config$sampling_approximation,
        case_study_config$target$control,
        case_study_config$target$treatment,
        case_study_config$target$total,
        case_study_config$target$treatment_effect,
        case_study_config$target$standard_error,
        case_study_config$source$control,
        case_study_config$source$treatment,
        case_study_config$source$total,
        case_study_config$source$treatment_effect,
        case_study_config$source$standard_error
      )
    )

    # Create the table with headers and horizontal lines
    kable(data,
          col.names = c("Parameter", "Value"),
          align = "l") %>%
      kableExtra::add_header_above(c("Case Study Configuration" = 2)) %>%
      kableExtra::kable_styling("striped", full_width = FALSE) %>%
      kableExtra::pack_rows("General", 1, 7) %>%
      kableExtra::pack_rows("Target", 8, 12) %>%
      kableExtra::pack_rows("Source", 13, 17)
  } else if (case_study_config$endpoint == "time_to_event") {
    data <- data.frame(
      Parameter = c(
        "Name",
        "Summary Measure Likelihood",
        "Theta 0",
        "Endpoint",
        "Null Space",
        "Sampling Approximation",
        "Control (Target)",
        "Treatment (Target)",
        "Total (Target)",
        "Treatment Effect (Target)",
        "Standard Error (Target)",
        "Maximum follow-up time (Target)",
        "Control (Source)",
        "Treatment (Source)",
        "Total (Source)",
        "Treatment Effect (Source)",
        "Standard Error (Source)",
        "Maximum follow-up time (Source)"
      ),
      Value = c(
        case_study_config$name,
        case_study_config$summary_measure_likelihood,
        case_study_config$theta_0,
        case_study_config$endpoint,
        case_study_config$null_space,
        case_study_config$sampling_approximation,
        case_study_config$target$control,
        case_study_config$target$treatment,
        case_study_config$target$total,
        case_study_config$target$treatment_effect,
        case_study_config$target$standard_error,
        case_study_config$target$max_follow_up_time,
        case_study_config$source$control,
        case_study_config$source$treatment,
        case_study_config$source$total,
        case_study_config$source$treatment_effect,
        case_study_config$source$standard_error,
        case_study_config$source$max_follow_up_time
      )
    )

    # Create the table with headers and horizontal lines
    kable(data,
          col.names = c("Parameter", "Value"),
          align = "l") %>%
      kableExtra::add_header_above(c("Case Study Configuration" = 2)) %>%
      kableExtra::kable_styling("striped", full_width = FALSE) %>%
      kableExtra::pack_rows("General", 1, 6) %>%
      kableExtra::pack_rows("Target", 7, 12) %>%
      kableExtra::pack_rows("Source", 13, 18)
  } else if (case_study_config$endpoint == "binary") {
    data <- data.frame(
      Parameter = c(
        "Name",
        "Control",
        "Summary Measure Likelihood",
        "Theta 0",
        "Endpoint",
        "Null Space",
        "Sampling Approximation",
        "Target Control",
        "Target Treatment",
        "Target Total",
        "Target Responses (Control)",
        "Target Responses (Treatment)",
        "Target Treatment Effect",
        "Target Standard Error",
        "Source Control",
        "Source Treatment",
        "Source Total",
        "Source Responses (Control)",
        "Source Responses (Treatment)",
        "Source Treatment Effect",
        "Source Standard Error"
      ),
      Value = c(
        case_study_config$name,
        case_study_config$control,
        case_study_config$summary_measure_likelihood,
        case_study_config$theta_0,
        case_study_config$endpoint,
        case_study_config$null_space,
        case_study_config$sampling_approximation,
        case_study_config$target$control,
        case_study_config$target$treatment,
        case_study_config$target$total,
        case_study_config$target$responses$control,
        case_study_config$target$responses$treatment,
        case_study_config$target$treatment_effect,
        case_study_config$target$standard_error,
        case_study_config$source$control,
        case_study_config$source$treatment,
        case_study_config$source$total,
        case_study_config$source$responses$control,
        case_study_config$source$responses$treatment,
        case_study_config$source$treatment_effect,
        case_study_config$source$standard_error
      )
    )

    # Create the table with headers and horizontal lines
    kable(data,
          col.names = c("Parameter", "Value"),
          align = "l") %>%
      kableExtra::add_header_above(c("Case Study Configuration" = 2)) %>%
      kableExtra::kable_styling("striped", full_width = FALSE) %>%
      kableExtra::pack_rows("General", 1, 7) %>%
      kableExtra::pack_rows("Target", 8, 14) %>%
      kableExtra::pack_rows("Source", 15, 21)
  }
}


#' Write Stan code to a file only when the content would change
#'
#' cmdstanr decides whether to rebuild by comparing the timestamps of the model
#' file and the executable, so rewriting identical code would force a rebuild
#' every time a model is constructed. Leaving an unchanged file untouched also
#' means parallel workers do not write over a file another worker is compiling.
#'
#' @param path Path of the Stan model file.
#' @param stan_model_code Stan code, as a single string or a character vector.
#'
#' @return TRUE when the file was written, FALSE when it was already up to date.
#' @noRd
write_stan_file_if_changed <- function(path, stan_model_code) {
  if (file.exists(path)) {
    current <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
    if (identical(current, unlist(strsplit(stan_model_code, "\n", fixed = TRUE)))) {
      return(invisible(FALSE))
    }
  }

  writeLines(stan_model_code, con = path)

  return(invisible(TRUE))
}

#' Draw a seed for one Stan sampler run
#'
#' Taken from the session random number stream, which the scenario simulation
#' seeds from the configuration. Runs are therefore reproducible, while every
#' replicate still gets its own stream: a single fixed seed would correlate
#' draws across replicates that are meant to be independent.
#'
#' @return A positive integer seed.
#' @noRd
stan_sampler_seed <- function() {
  sample.int(.Machine$integer.max, size = 1)
}

#' Directory holding the Stan draws of one model in one process
#'
#' Parallel workers share a case study and a method, and the draws cleanup
#' removes files by age, so a shared directory lets one worker delete the CSVs
#' backing another worker's fit. cmdstanr reads those files lazily, so give each
#' process a directory of its own.
#'
#' @param case_study Case study name.
#' @param method Method name.
#' @param process_id Identifier of the process writing the draws.
#'
#' @return Path of the directory, which is not created here.
#' @noRd
stan_draws_directory <- function(case_study, method, process_id = Sys.getpid()) {
  # system.file() returns "" for a directory absent from the installed package,
  # so build the path from the package root, which always exists.
  file.path(
    system.file(package = "RBExT"),
    "stan",
    "draws",
    paste0(tolower(case_study), "_", method, "_", process_id)
  )
}

#' Remove the Stan draws of one model across every process
#'
#' @description Each process writes to its own directory, so one run leaves
#'   a directory per worker behind. They hold intermediate MCMC output that
#'   runs to gigabytes for the larger case studies, so a finished run clears
#'   them. The prefix is taken from `stan_draws_directory()` rather than
#'   rebuilt here, so the two cannot drift apart.
#'
#' @param case_study Case study name.
#' @param method Method name.
#' @param root Directory the per-process directories sit in.
#'
#' @return Paths of the directories that were removed, invisibly.
#' @export
clear_stan_draws <- function(case_study,
                             method,
                             root = dirname(stan_draws_directory(case_study, method))) {
  prefix <- basename(stan_draws_directory(case_study, method, process_id = ""))
  entries <- list.files(root, full.names = TRUE)
  names <- basename(entries)

  # A method whose name extends another's shares its prefix, so require the
  # remainder to be the process id and nothing else.
  suffix <- substring(names, nchar(prefix) + 1L)
  mine <- startsWith(names, prefix) & grepl("^[0-9]+$", suffix)

  unlink(entries[mine], recursive = TRUE)
  invisible(entries[mine])
}


#' Directory holding the compiled Stan models
#'
#' @description `inst/stan` is excluded from the built package, so
#'   `system.file("stan", package = "RBExT")` returns `""` once RBExT is
#'   installed and every model path would resolve to the filesystem root.
#'   Compiled models are build artifacts rather than package contents, so
#'   they belong in the user cache directory, which stays writable even when
#'   the library does not.
#'
#' @return Path of the directory, created if it does not exist.
#' @noRd
stan_model_directory <- function() {
  directory <- file.path(tools::R_user_dir("RBExT", "cache"), "stan")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  directory
}

#' Remove the compiled Stan models
#'
#' @description The models are compiled on first use and cached in
#'   `stan_model_directory()`. Clearing the cache forces the next run to
#'   recompile them, which is what `inst/scripts/main.R` does when its
#'   `delete_stan_files` switch is set.
#'
#' @param directory Directory holding the cached models.
#'
#' @return Paths of the files that were removed, invisibly.
#' @export
clear_stan_model_cache <- function(directory = stan_model_directory()) {
  cached <- list.files(directory, full.names = TRUE)
  file.remove(cached)
  invisible(cached)
}

compile_stan_model <- function(model_name, stan_model_code) {
  stan_directory <- stan_model_directory()
  stan_model_file_path <- file.path(stan_directory, paste0(model_name, ".stan"))
  stan_exe_file_path <- file.path(stan_directory, paste0(model_name, ".exe"))

  stan_file_changed <- write_stan_file_if_changed(
    stan_model_file_path,
    stan_model_code
  )

  executable_is_stale <- !file.exists(stan_exe_file_path) ||
    file.mtime(stan_exe_file_path) < file.mtime(stan_model_file_path)

  # Always hand cmdstanr the model file, so that an executable left over from an
  # earlier version of the code is rebuilt rather than silently reused. None of
  # the models use reduce_sum or map_rect, so within-chain threading cannot
  # engage: building with STAN_THREADS would only make the autodiff stack
  # thread-local, which costs speed for no parallelism in return.
  stan_model <- cmdstanr::cmdstan_model(stan_model_file_path,
                                        exe_file = stan_exe_file_path,
                                        force_recompile = stan_file_changed ||
                                          executable_is_stale)

  return(stan_model)
}

load_data <- function(results_row, type, reload_data_objects = FALSE, case_studies_config_dir = NULL) {
  if (!(type %in% c("target", "source"))){
    stop("type must either be 'target' or 'source'.")
  }

  if (reload_data_objects) {
    if (is.null(case_studies_config_dir)) {
      case_studies_config_dir <- paste0(system.file("conf/case_studies", package = "RBExT"), "/")
    }
    case_study_config <- yaml::yaml.load_file(paste0(case_studies_config_dir, results_row$case_study, ".yml"))

    source_data <- SourceData$new(
      case_study_config = case_study_config,
      source_denominator = results_row$source_denominator
    )
    if (type == "target") {
      data <- TargetDataFactory$new()
      data <- data$create(
        source_data = source_data,
        case_study_config = case_study_config,
        target_sample_size_per_arm = results_row$target_sample_size_per_arm,
        treatment_drift = results_row$treatment_drift,
        control_drift = results_row$control_drift,
        summary_measure_likelihood = source_data$summary_measure_likelihood,
        target_to_source_std_ratio = results_row$target_to_source_std_ratio
      )
    } else if (type == "source") {
      data <- source_data
    }
  } else {
    if (type == "target") {
      data <- list(
        sample_size_per_arm = results_row$target_sample_size_per_arm,
        sample_size_control = results_row$target_sample_size_per_arm,
        sample_size_treatment = results_row$target_sample_size_per_arm,
        treatment_effect = results_row$target_treatment_effect,
        standard_deviation = results_row$target_standard_deviation,
        summary_measure_likelihood = results_row$summary_measure_likelihood,
        treatment_rate = results_row$target_treatment_rate,
        control_rate = results_row$target_control_rate
      )
    } else if (type == "source") {
      data <- list(
        treatment_effect_estimate = results_row$source_treatment_effect_estimate,
        standard_error = results_row$source_standard_error,
        sample_size_control = results_row$source_sample_size_control,
        sample_size_treatment = results_row$source_sample_size_treatment,
        equivalent_source_sample_size_per_arm = results_row$equivalent_source_sample_size_per_arm,
        treatment_rate = results_row$source_treatment_rate,
        control_rate = results_row$source_control_rate
      )

      if (is.null(data$equivalent_source_sample_size_per_arm) ||
          any(is.na(data$equivalent_source_sample_size_per_arm))) {
        data$equivalent_source_sample_size_per_arm <- 2 * data$sample_size_control * data$sample_size_treatment / (data$sample_size_control + data$sample_size_treatment)
      }
    }
  }
  return(data)
}

# Function to create boolean filter
create_boolean_filter <- function(df, conditions, exclude_key = NULL) {
  if (!is.null(exclude_key)) {
    conditions <- conditions[!names(conditions) %in% exclude_key]
  }

  # Convert all columns to numeric where possible
  df <- data.frame(lapply(df, function(x)
    as.numeric(x)))


  filter <- rep(TRUE, nrow(df))
  for (col in names(conditions)) {
    condition <- conditions[[col]]
    filter <- (df[[col]] == condition) & filter
  }
  return(filter)
}


check_confidence_intervals <- function(df, metrics) {
  # Loop through each metric and check the confidence intervals
  for (metric in metrics) {
    upper_col <- paste0("conf_int_", metric, "_upper")
    lower_col <- paste0("conf_int_", metric, "_lower")

    # Check if the metric and its CI bounds exist in the dataframe
    if (all(c(metric, lower_col, upper_col) %in% names(df))) {

      # Check if the lower bound is indeed lower than the upper bound
      invalid_bounds <- which(df[[lower_col]] > df[[upper_col]])

      if (length(invalid_bounds) > 0) {
        issue <- paste0("Lower bound of the CI is larger than the upper bound for ", metric)
        warning(issue)
      }

      # Check if the metric value lies within the confidence interval
      out_of_bounds <- which(df[[metric]] < df[[lower_col]] | df[[metric]] > df[[upper_col]])
      if (length(out_of_bounds) > 0) {
        issue <- paste0("Mean value outside the CI bounds for ", metric)
        warning(issue)
      }
    }
  }
}


# Used to extract parameters as nested list
extract_nested_parameter <- function(parameters){
  # Initialize an empty list to store the nested list
  nested_list <- list()

  # Iterate over the column names of the parameters to construct the nested list
  for (colname in colnames(parameters)) {

    # Split the column name by period (".") to find the hierarchy
    split_names <- strsplit(colname, "\\.")[[1]]

    if (length(split_names) == 2) {
      if (!split_names[1] %in% names(nested_list)) {
        nested_list[[split_names[1]]] <- list()
      }

      x <- parameters[[colname]]
      x_numeric <- suppressWarnings(as.numeric(x))

      # Replace NA values with the original input
      x_clean <- ifelse(is.na(x_numeric), x, x_numeric)

      # Assign values dynamically to the second level (e.g., family or std_dev)
      nested_list[[split_names[1]]][[split_names[2]]] <- x_clean

      # For 'initial_prior', assign the value directly
    } else {
      nested_list[colname] <- parameters[[colname]]
    }
  }
  return(nested_list)
}

# Function to log errors globally
global_error_handler <- function() {
  err <- geterrmessage()  # Get the error message
  futile.logger::flog.error("Global error occurred: %s", err)

  # Optionally log other debugging info such as the call stack
  futile.logger::flog.error("Call stack:\n%s", paste(deparse(sys.calls()), collapse = "\n"))

  # Log additional variables of interest (if needed)
  # For instance, if you're in a loop or a function with certain variables
  # futile.logger::flog.error("Variable state at error - x: %s, y: %s", x, y)  # Customize as needed
}

generate_log_filename <- function(base_name = "error_log.log", suffix_type = "timestamp") {
  if (!file.exists(base_name)) {
    return(base_name)  # Return the base name if no file exists
  }

  # If the file exists, create a new filename with a suffix
  if (suffix_type == "timestamp") {
    # Add a timestamp to the filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    new_name <- sub(".log$", paste0("_", timestamp, ".log"), base_name)
  } else if (suffix_type == "counter") {
    # Use a counter to create unique filenames
    counter <- 1
    repeat {
      new_name <- sub(".log$", paste0("_", counter, ".log"), base_name)
      if (!file.exists(new_name)) break
      counter <- counter + 1
    }
  }

  return(new_name)
}

save_state <- function(iteration, scenario, worker_id = NULL, env) {
  if (is.null(worker_id)){
    file = paste0("./logs/", env, "/checkpoints/checkpoint_iter_", iteration, ".RData")
  } else {
    file = paste0("./logs/", env, "/checkpoints/checkpoint_iter_", worker_id, "_iter_", iteration, ".RData")
  }
  # Extract the directory path from the file path
  dir_path <- dirname(file)

  # Check if the directory exists, and if not, create it
  if (!file.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  save(scenario, file = file)
}

#' Print a function's source code
#'
#' @description Used by the vignettes to display the implementation of key
#'   functions inline, next to their explanation.
#'
#' @param function_obj The function to print the source of.
#'
#' @return `NULL`, invisibly; prints the function's source code as a side effect.
#'
#' @export
read_function_code <- function(function_obj){
  function_name <- substitute(function_obj)

  # Use deparse() to get the function's source code
  function_code <- deparse(function_obj)

  # Prepend the function name and assignment
  cat(paste0(function_name, " <- ", paste(function_code, collapse = "\n"), sep = ""))
}



check_simulation_completeness <- function(results_dir = results_dir, ocs_filename = ocs_filename, scenarios_config, config_dir) {
  for (case_study in scenarios_config$case_studies){
    case_study_config <- yaml::yaml.load_file(system.file(
      paste0("conf/case_studies/", case_study, ".yml"),
      package = "RBExT"
    ))
    for (method in scenarios_config$method){
      file_path <- paste0(results_dir, 'frequentist/', case_study, '/', method, '/', ocs_filename)

      if (file.exists(file_path)) {
        results_df <- readr::read_csv(file_path, col_types = frequentist_col_types, col_names = TRUE)
      } else {
        # File doesn't exist, continue with the rest of the code
        message("File does not exist, continuing...")
        next
      }

      null_space <- case_study_config$null_space
      theta_0 <- case_study_config$theta_0

      source_denominator_change_factors <- unique(results_df$source_denominator_change_factor)

      if (!setequal(scenarios_config$denominator_change_factor, source_denominator_change_factors) && !(case_study %in% c("botox", "dapagliflozin", "aprepitant"))){
        warning(paste0("Simulaton is incomplete, all denominator change factors are not included. Method: ", method, ", Case study : ", case_study))
      }

      for (source_denominator_change_factor in source_denominator_change_factors) {
        results_df_1 <- results_df %>%
          dplyr::filter(
            source_denominator_change_factor == !!source_denominator_change_factor  |
              is.na(source_denominator_change_factor)
          )
        target_to_source_std_ratio_range <- unique(results_df_1$target_to_source_std_ratio)

        if (!setequal(scenarios_config$target_to_source_std_ratio_range, target_to_source_std_ratio_range) && (case_study %in% c("botox", "dapagliflozin"))){
          warning(paste0("Simulaton is incomplete, all target to source std ratios are not included. Method: ", method, ", Case study : ", case_study))
        }

        for (target_to_source_std_ratio in target_to_source_std_ratio_range) {
          results_df_2 <- results_df_1 %>%
            dplyr::filter(target_to_source_std_ratio == !!target_to_source_std_ratio |
                            is.na(target_to_source_std_ratio))


          if (nrow(results_df_2) == 0) {
            stop("Dataframe is empty")
          }

          theta_0 <- case_study_config$theta_0

          target_sample_sizes <- unique(results_df_2$target_sample_size_per_arm)

          if (!(length(scenarios_config$sample_size_factors) == length(target_sample_sizes))){
            warning(paste0("Simulaton is incomplete, all sample size factors are not included. Method: ", method, ", Case study : ", case_study))
          }

          for (target_sample_size_per_arm in target_sample_sizes) {
            results_df_3 <- results_df_2 %>%
              dplyr::filter(target_sample_size_per_arm == !!target_sample_size_per_arm)

            mandatory_drift_values <- important_drift_values(unique(results_df_3$source_treatment_effect_estimate), case_study_config)

            # Make sure the mandatory drift values are in the drift values
            drift_values <- unique(results_df_3$drift)

            if (sum(!(mandatory_drift_values %in% drift_values))>1){
              stop(paste0("Simulaton is incomplete, some mandatory drift values are not included. Method: ", method, ", Case study : ", case_study))
            }

            drift_values <- setdiff(drift_values, mandatory_drift_values)
            if (length(drift_values) < (scenarios_config$ndrift - length(mandatory_drift_values))){
              stop(paste0("Simulaton is incomplete, all drift values are not included. Method: ", method, ", Case study : ", case_study))
            }
          }
        }
      }
    }
  }
}


is_approx_equal <- function(x, y, tolerance = .Machine$double.eps^0.5) {
  abs(x - y) < tolerance
}

# Function to compare rows, ignoring NAs
compare_ignore_na <- function(row, ref_row) {
  # Only compare non-NA elements in the reference row
  non_na_indices <- !is.na(ref_row)
  all(row[non_na_indices] == ref_row[non_na_indices])
}

#' Summarise the posterior draws consumed by the simulation
#'
#' Computes the moments, the credible interval bounds and the convergence
#' diagnostics in a single pass over the draws. The estimators are the ones the
#' simulation reported when these quantities were collected separately:
#' `bayesplot::rhat()` is `posterior::rhat()`, and `bayesplot::neff_ratio()`
#' multiplied back up by the number of draws is `posterior::ess_basic()`.
#'
#' @param draws Posterior draws, restricted to the variables of interest.
#'
#' @return A summary data frame with one row per variable.
#' @noRd
summarise_posterior_draws <- function(draws) {
  posterior::summarise_draws(
    draws,
    posterior::default_summary_measures(),
    extra_quantiles = ~ posterior::quantile2(., probs = c(0.025, 0.975)),
    rhat = posterior::rhat,
    n_eff = posterior::ess_basic
  )
}

#' Check that a value is a single number
#'
#' A like-for-like replacement for assertions::assert_number() in code that
#' builds an object per scenario or per result row. It accepts exactly what
#' assert_number() accepts - one numeric value, NA and Inf included - but
#' costs about 0.4us against 190us, because it does not inspect its own call
#' or dispatch over a list of validators. The argument is only deparsed when
#' the check fails, so the message still names the offending value.
#'
#' @param x Value to check.
#' @param arg_name Name to report; defaults to the deparsed argument.
#'
#' @return `x`, invisibly. Called for the error it raises.
#' @noRd
assert_single_number <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.numeric(x) || length(x) != 1L) {
    detail <- if (!is.numeric(x)) {
      paste0("class is ", class(x)[1], ", not numeric")
    } else {
      paste0("length is ", length(x), ", not 1")
    }
    stop(sprintf("'%s' is not a number! (%s)", arg_name, detail), call. = FALSE)
  }

  invisible(x)
}

#' Unwrap the length-1 list columns that rbind() over lists leaves behind
#'
#' `do.call(rbind, list_of_lists)` returns a matrix of mode list, so
#' `data.frame()` on it gives a frame whose every column is a list of
#' length-1 values rather than a vector of scalars. Columns whose cells are
#' all scalars are unwrapped into the vector they stand for; a column that
#' genuinely holds several values per row, or one wrapped in I(), is left
#' alone.
#'
#' @param df A dataframe.
#'
#' @return `df`, with its scalar list columns replaced by plain vectors.
#' @noRd
unwrap_scalar_list_columns <- function(df) {
  df[] <- lapply(df, function(column) {
    if (is.list(column) && !inherits(column, "AsIs") && all(lengths(column) == 1L)) {
      unlist(column, use.names = FALSE)
    } else {
      column
    }
  })

  df
}
