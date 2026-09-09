#' Predicates used to check a configuration entry against a declared type
#'
#' @description The vocabulary [validate_config()] accepts. YAML reads a list
#'   of scalars as a list rather than an atomic vector, so the vector types
#'   accept both.
#'
#' @keywords internal
config_type_predicates <- list(
  probability = function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0 && x <= 1
  },
  count = function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && x > 0 && x == round(x)
  },
  nonnegative_count = function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0 && x == round(x)
  },
  number = function(x) is.numeric(x) && length(x) == 1 && !is.na(x),
  flag = function(x) is.logical(x) && length(x) == 1 && !is.na(x),
  string = function(x) is.character(x) && length(x) == 1 && !is.na(x),
  numeric_vector = function(x) {
    length(x) > 0 && all(vapply(x, function(e) is.numeric(e) && length(e) == 1, logical(1)))
  },
  character_vector = function(x) {
    length(x) > 0 && all(vapply(x, function(e) is.character(e) && length(e) == 1, logical(1)))
  },
  # `parallelization` is either a single flag or a list of method names.
  flag_or_list = function(x) {
    (is.logical(x) && length(x) == 1 && !is.na(x)) || is.list(x)
  }
)


#' Describe what a configuration type requires, for error messages
#'
#' @keywords internal
config_type_descriptions <- c(
  probability = "a single number between 0 and 1",
  count = "a single positive whole number",
  nonnegative_count = "a single whole number that is zero or more",
  number = "a single number",
  flag = "a single TRUE or FALSE",
  string = "a single string",
  numeric_vector = "a non-empty list of numbers",
  character_vector = "a non-empty list of strings",
  flag_or_list = "a single TRUE or FALSE, or a list"
)


#' Validate a configuration read from YAML
#'
#' @description YAML enters the package as an untyped nested list, so a typo in
#'   a key or a value of the wrong shape otherwise surfaces as a `NULL`
#'   propagating into arithmetic far from the file that caused it. This checks a
#'   config against a schema at the point it is read, so the error names the
#'   file, the key and what was expected.
#'
#' @param config The list returned by [yaml::read_yaml()].
#' @param schema A named list mapping keys to a type in
#'   [config_type_predicates]. Keys absent from the schema are not checked.
#' @param context The name of the configuration file, used in the error.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
validate_config <- function(config, schema, context) {
  # A trailing "?" marks a key the code supplies a default for.
  optional <- grepl("\\?$", unlist(schema))
  bare_types <- sub("\\?$", "", unlist(schema))
  names(optional) <- names(schema)
  names(bare_types) <- names(schema)

  unknown_types <- setdiff(bare_types, names(config_type_predicates))
  if (length(unknown_types) > 0) {
    stop(paste(
      "The schema names types that cannot be checked:",
      paste(unknown_types, collapse = ", ")
    ))
  }

  problems <- character(0)
  for (key in names(schema)) {
    expected_type <- bare_types[[key]]

    if (!key %in% names(config)) {
      if (!optional[[key]]) {
        problems <- c(problems, paste0(key, " is missing"))
      }
      next
    }

    value <- config[[key]]
    if (!config_type_predicates[[expected_type]](value)) {
      problems <- c(problems, paste0(
        key, " should be ", config_type_descriptions[[expected_type]]
      ))
    }
  }

  if (length(problems) > 0) {
    stop(paste0(
      context, " is not valid: ", paste(problems, collapse = "; ")
    ), call. = FALSE)
  }

  invisible(NULL)
}


#' Read a YAML configuration and validate it against a schema
#'
#' @param path Path to the YAML file.
#' @param schema The schema to validate against, as taken by
#'   [validate_config()].
#'
#' @return The parsed configuration.
#'
#' @keywords internal
read_config <- function(path, schema) {
  config <- yaml::read_yaml(path)
  validate_config(config, schema, path)
  config
}


#' Schema for `simulation_config.yml`
#'
#' @keywords internal
simulation_config_schema <- list(
  critical_value = "probability",
  confidence_level = "probability",
  seed = "number",
  n_samples_design_prior = "count",
  n_samples_mixture_approx = "count",
  n_samples_quantiles_estimation = "count",
  compute_bayesian_ocs_mc = "flag",
  compute_frequentist_ocs = "flag",
  delete_old_results = "flag"
)


#' Schema for `mcmc_config.yml`
#'
#' @keywords internal
mcmc_config_schema <- list(
  num_chains = "count",
  parallel_chains = "count",
  tune = "count",
  target_accept = "probability",
  chain_length = "count",
  max_chain_length = "count?",
  target_ess = "count",
  rhat_threshold = "number",
  max_divergence_rate = "probability"
)


#' Schema for `scenarios_config.yml`
#'
#' @keywords internal
scenarios_config_schema <- list(
  n_replicates = "count",
  ndrift = "nonnegative_count",
  parallelization = "flag_or_list?",
  denominator_change_factor = "numeric_vector",
  sample_size_factors = "numeric_vector",
  target_to_source_std_ratio_range = "numeric_vector?",
  case_studies = "character_vector",
  methods = "character_vector"
)


#' Check the arguments a model is created from
#'
#' @description `Model$create()` is the single entry point every method class
#'   is built through, and it reads its configuration out of untyped lists. A
#'   key that is absent or of the wrong shape otherwise surfaces as a comparison
#'   against `NULL` further down, so the failure names the branch that tripped
#'   rather than the argument that was wrong.
#'
#' @param case_study_config A list describing the case study.
#' @param method The name of the method being created.
#' @param method_parameters A list of parameters for that method.
#' @param mcmc_config An optional list configuring MCMC.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
check_model_create_args <- function(case_study_config,
                                    method,
                                    method_parameters,
                                    mcmc_config = NULL) {
  if (!is.list(case_study_config)) {
    stop(paste0(
      "Model$create(): case_study_config should be a list, not ",
      paste(class(case_study_config), collapse = "/"), "."
    ), call. = FALSE)
  }

  validate_config(
    case_study_config,
    list(null_space = "string", summary_measure_likelihood = "string"),
    "Model$create(): case_study_config"
  )

  if (!is.character(method) || length(method) != 1 || is.na(method)) {
    stop("Model$create(): method should be a single string.", call. = FALSE)
  }

  if (!is.list(method_parameters)) {
    stop(paste0(
      "Model$create(): method_parameters should be a list, not ",
      paste(class(method_parameters), collapse = "/"), "."
    ), call. = FALSE)
  }

  if (!is.null(mcmc_config) && !is.list(mcmc_config)) {
    stop(paste0(
      "Model$create(): mcmc_config should be a list or NULL, not ",
      paste(class(mcmc_config), collapse = "/"), "."
    ), call. = FALSE)
  }

  invisible(NULL)
}
