## Shared helpers for the BExTE Shiny app.
##
## Convention: every path here is relative to the repository root, matching
## the convention already used by inst/scripts/main.R ("./results/<env>/",
## "./logs/<env>/"). run_bexte_app() is responsible for making sure the app
## (and any callr background process it spawns) runs with that as its
## working directory.

USER_CONFIGS_DIR <- "user_configs"
USER_CASE_STUDIES_DIR <- file.path(USER_CONFIGS_DIR, "case_studies")

`%||%` <- function(a, b) if (is.null(a)) b else a

BEXTE_NAME_PATTERN <- "^[a-z0-9][a-z0-9_-]*$"

BEXTE_METHOD_LABELS <- c(
  RMP = "Robust mixture prior (RMP)",
  NPP = "Normalized power prior (NPP)",
  separate = "Separate analysis",
  pooling = "Pooled analysis",
  conditional_power_prior = "Conditional power prior",
  p_value_based_PP = "P-value-based power prior",
  PDCCPP = "Prior-data conflict calibrated power prior (PDCCPP)",
  EB_PP = "Empirical Bayes power prior",
  test_then_pool_difference = "Test then pool (difference)",
  test_then_pool_equivalence = "Test then pool (equivalence)",
  commensurate_power_prior = "Commensurate power prior"
)

BEXTE_METRIC_LABELS <- c(
  success_proba = "Study success probability",
  tie = "Type I error",
  coverage = "95% credible interval coverage",
  mse = "Mean squared error",
  bias = "Bias",
  precision = "95% credible interval half-width",
  average_tie = "Average type I error",
  average_power = "Average power",
  prior_proba_no_benefit = "Prior probability of no benefit",
  prior_proba_benefit = "Prior probability of benefit",
  prepost_proba_FP = "Pre-posterior false-positive probability",
  prepost_proba_TP = "Pre-posterior true-positive probability",
  upper_bound_proba_FP = "Upper bound on false-positive probability",
  prior_proba_success = "Prior probability of study success"
)

bexte_method_label <- function(method) {
  label <- unname(BEXTE_METHOD_LABELS[method])
  if (length(label) == 0 || is.na(label)) method else label
}

bexte_method_choices <- function(methods) {
  stats::setNames(methods, vapply(methods, bexte_method_label, character(1)))
}

bexte_metric_label <- function(metric, fallback = metric) {
  label <- unname(BEXTE_METRIC_LABELS[metric])
  if (length(label) == 0 || is.na(label)) fallback else label
}

bexte_column_label <- function(name) {
  labels <- c(
    case_study = "Case study", method = "Method",
    target_sample_size_per_arm = "Target sample size per arm",
    source_denominator_change_factor = "Source denominator change factor",
    target_to_source_std_ratio = "Target/source SD ratio",
    parameters = "Method parameters"
  )
  explicit <- unname(labels[name])
  if (length(explicit) > 0 && !is.na(explicit)) return(explicit)
  metric <- unname(BEXTE_METRIC_LABELS[name])
  if (length(metric) > 0 && !is.na(metric)) return(metric)
  if (startsWith(name, "mcse_")) {
    metric_name <- sub("^mcse_", "", name)
    return(paste("Monte Carlo SE for", bexte_metric_label(metric_name, metric_name)))
  }
  if (startsWith(name, "conf_int_")) {
    metric_name <- sub("^conf_int_", "", name)
    bound <- if (endsWith(metric_name, "_lower")) "lower" else if (endsWith(metric_name, "_upper")) "upper" else ""
    metric_name <- sub("_(lower|upper)$", "", metric_name)
    return(trimws(paste("95% CI", bound, "for", bexte_metric_label(metric_name, metric_name))))
  }
  label <- tools::toTitleCase(gsub("_", " ", name))
  for (token in c("Mse", "Ess", "Mcse", "Ci", "Fp", "Tp")) {
    label <- gsub(paste0("\\b", token, "\\b"), toupper(token), label)
  }
  label
}

bexte_validate_name <- function(value, label) {
  if (is.null(value) || length(value) != 1 || !nzchar(value)) {
    stop(sprintf("Give the %s a name.", label), call. = FALSE)
  }
  if (!grepl(BEXTE_NAME_PATTERN, value)) {
    stop(sprintf(
      "%s must start with a lowercase letter or number and use only lowercase letters, numbers, hyphens or underscores.",
      tools::toTitleCase(label)
    ), call. = FALSE)
  }
  invisible(value)
}

bexte_validate_number <- function(value, label, min = -Inf, max = Inf,
                                  whole = FALSE, strict_min = FALSE) {
  if (length(value) != 1 || is.null(value) || is.na(value) || !is.finite(value)) {
    stop(sprintf("%s must be a number.", label), call. = FALSE)
  }
  if (whole && value != floor(value)) {
    stop(sprintf("%s must be a whole number.", label), call. = FALSE)
  }
  below <- if (strict_min) value <= min else value < min
  if (below || value > max) {
    lower <- if (strict_min) sprintf("greater than %s", min) else sprintf("at least %s", min)
    upper <- if (is.finite(max)) sprintf(" and no more than %s", max) else ""
    stop(sprintf("%s must be %s%s.", label, lower, upper), call. = FALSE)
  }
  invisible(value)
}

bexte_parse_number_list <- function(text, label, positive = TRUE) {
  if (is.null(text) || length(text) != 1 || !nzchar(trimws(text))) {
    stop(sprintf("%s needs at least one value.", label), call. = FALSE)
  }
  if (grepl("(^|,)\\s*(,|$)", text)) {
    stop(sprintf("%s must be a comma-separated list of numbers.", label), call. = FALSE)
  }
  pieces <- trimws(strsplit(text, ",", fixed = TRUE)[[1]])
  values <- suppressWarnings(as.numeric(pieces))
  if (any(!nzchar(pieces)) || anyNA(values) || any(!is.finite(values))) {
    stop(sprintf("%s must be a comma-separated list of numbers.", label), call. = FALSE)
  }
  if (positive && any(values <= 0)) {
    stop(sprintf("Every %s value must be greater than zero.", tolower(label)), call. = FALSE)
  }
  values
}

bexte_has_results <- function(env) {
  dir <- file.path("results", env)
  dir.exists(dir) && length(list.files(dir, all.files = TRUE, no.. = TRUE)) > 0
}

bexte_ensure_user_dirs <- function() {
  dir.create(USER_CASE_STUDIES_DIR, showWarnings = FALSE, recursive = TRUE)
}

## ---- Case studies ---------------------------------------------------------

#' List case studies available to the app: package-shipped ones (read-only)
#' and user-authored ones (in user_configs/case_studies/). If a user-authored
#' case study shares a name with a package one, the user's version wins.
#'
#' A file can exist in user_configs/case_studies/ without being user-authored:
#' ensure_case_studies_snapshot() copies package case studies there so a run
#' has a single case_studies_config_dir to read from. Such a copy is only
#' labelled "user" once its content actually diverges from the package
#' original - otherwise it's still shown as "package".
list_case_studies <- function() {
  pkg_dir <- system.file("conf/case_studies", package = "BExTE")
  pkg_files <- list.files(pkg_dir, pattern = "\\.yml$", full.names = FALSE)
  user_files <- if (dir.exists(USER_CASE_STUDIES_DIR)) {
    list.files(USER_CASE_STUDIES_DIR, pattern = "\\.yml$", full.names = FALSE)
  } else {
    character(0)
  }

  pkg_names <- tools::file_path_sans_ext(pkg_files)
  user_names <- tools::file_path_sans_ext(user_files)

  is_unmodified_snapshot <- vapply(user_names, function(name) {
    if (!(name %in% pkg_names)) {
      return(FALSE)
    }
    pkg_path <- file.path(pkg_dir, paste0(name, ".yml"))
    user_path <- file.path(USER_CASE_STUDIES_DIR, paste0(name, ".yml"))
    identical(
      tools::md5sum(pkg_path)[[1]],
      tools::md5sum(user_path)[[1]]
    )
  }, logical(1))

  user_names_authored <- user_names[!is_unmodified_snapshot]

  data.frame(
    name = c(setdiff(pkg_names, user_names_authored), user_names_authored),
    source = c(
      rep("package", length(setdiff(pkg_names, user_names_authored))),
      rep("user", length(user_names_authored))
    ),
    stringsAsFactors = FALSE
  )
}

case_study_path <- function(name) {
  user_path <- file.path(USER_CASE_STUDIES_DIR, paste0(name, ".yml"))
  if (file.exists(user_path)) {
    return(user_path)
  }
  system.file(file.path("conf/case_studies", paste0(name, ".yml")), package = "BExTE")
}

read_case_study <- function(name) {
  path <- case_study_path(name)
  if (path == "" || !file.exists(path)) {
    return(NULL)
  }
  yaml::read_yaml(path)
}

#' Build a case study list (matching the schema used by
#' inst/conf/case_studies/*.yml) for a binary or continuous endpoint, and
#' write it to user_configs/case_studies/<name>.yml.
build_and_save_case_study <- function(name, control_arm_name, endpoint, null_space,
                                       target_control_n, target_treatment_n,
                                       source_control_n, source_treatment_n,
                                       target_control_responses = NULL, target_treatment_responses = NULL,
                                       source_control_responses = NULL, source_treatment_responses = NULL,
                                       target_treatment_effect = NULL, target_standard_error = NULL,
                                       source_treatment_effect = NULL, source_standard_error = NULL,
                                       theta_0 = 0) {
  bexte_validate_name(name, "case study")
  if (is.null(control_arm_name) || !nzchar(trimws(control_arm_name))) {
    stop("Control arm name cannot be empty.", call. = FALSE)
  }
  if (!(endpoint %in% c("binary", "continuous"))) {
    stop("Endpoint must be binary or continuous.", call. = FALSE)
  }
  if (!(null_space %in% c("left", "right"))) {
    stop("Null space must be left or right.", call. = FALSE)
  }
  bexte_validate_number(theta_0, "Null hypothesis boundary")
  bexte_validate_number(target_control_n, "Target control sample size", min = 1, whole = TRUE)
  bexte_validate_number(target_treatment_n, "Target treatment sample size", min = 1, whole = TRUE)
  bexte_validate_number(source_control_n, "Source control sample size", min = 1, whole = TRUE)
  bexte_validate_number(source_treatment_n, "Source treatment sample size", min = 1, whole = TRUE)

  bexte_ensure_user_dirs()

  if (endpoint == "binary") {
    bexte_validate_number(target_control_responses, "Target control responses", min = 0,
                          max = target_control_n, whole = TRUE)
    bexte_validate_number(target_treatment_responses, "Target treatment responses", min = 0,
                          max = target_treatment_n, whole = TRUE)
    bexte_validate_number(source_control_responses, "Source control responses", min = 0,
                          max = source_control_n, whole = TRUE)
    bexte_validate_number(source_treatment_responses, "Source treatment responses", min = 0,
                          max = source_treatment_n, whole = TRUE)
    summary_measure_likelihood <- "binomial"

    target_treatment_effect <- target_treatment_responses / target_treatment_n -
      target_control_responses / target_control_n
    p1 <- target_treatment_responses / target_treatment_n
    p2 <- target_control_responses / target_control_n
    target_standard_error <- sqrt(p1 * (1 - p1) / target_treatment_n + p2 * (1 - p2) / target_control_n)

    source_treatment_effect <- source_treatment_responses / source_treatment_n -
      source_control_responses / source_control_n
    p1s <- source_treatment_responses / source_treatment_n
    p2s <- source_control_responses / source_control_n
    source_standard_error <- sqrt(p1s * (1 - p1s) / source_treatment_n + p2s * (1 - p2s) / source_control_n)

    target <- list(
      control = target_control_n,
      treatment = target_treatment_n,
      total = target_control_n + target_treatment_n,
      responses = list(control = target_control_responses, treatment = target_treatment_responses),
      treatment_effect = target_treatment_effect,
      standard_error = target_standard_error
    )
    source <- list(
      control = source_control_n,
      treatment = source_treatment_n,
      total = source_control_n + source_treatment_n,
      responses = list(control = source_control_responses, treatment = source_treatment_responses),
      treatment_effect = source_treatment_effect,
      standard_error = source_standard_error
    )
  } else if (endpoint == "continuous") {
    bexte_validate_number(target_treatment_effect, "Target treatment effect")
    bexte_validate_number(source_treatment_effect, "Source treatment effect")
    bexte_validate_number(target_standard_error, "Target standard error", min = 0, strict_min = TRUE)
    bexte_validate_number(source_standard_error, "Source standard error", min = 0, strict_min = TRUE)
    summary_measure_likelihood <- "normal"

    target <- list(
      control = target_control_n,
      treatment = target_treatment_n,
      total = target_control_n + target_treatment_n,
      treatment_effect = target_treatment_effect,
      standard_error = target_standard_error
    )
    source <- list(
      control = source_control_n,
      treatment = source_treatment_n,
      total = source_control_n + source_treatment_n,
      treatment_effect = source_treatment_effect,
      standard_error = source_standard_error
    )
  } else {
    stop("The case study builder only supports 'binary' and 'continuous' endpoints. ",
         "For time_to_event/recurrent_event case studies, write the YAML by hand ",
         "into user_configs/case_studies/ following the schema of an existing one ",
         "(e.g. inst/conf/case_studies/teriflunomide.yml), then it will show up here.")
  }

  case_study <- list(
    name = name,
    control = control_arm_name,
    summary_measure_likelihood = summary_measure_likelihood,
    theta_0 = theta_0,
    endpoint = endpoint,
    null_space = null_space,
    sampling_approximation = FALSE,
    target = target,
    source = source
  )

  path <- file.path(USER_CASE_STUDIES_DIR, paste0(name, ".yml"))
  yaml::write_yaml(case_study, path)
  invisible(path)
}

## ---- Environments ----------------------------------------------------------

#' List environments available to run: package-shipped (inst/conf/<env>/,
#' read-only source, cloned on save) and user-authored (user_configs/<env>/).
list_environments <- function() {
  pkg_conf_dir <- system.file("conf", package = "BExTE")
  pkg_envs <- list.dirs(pkg_conf_dir, full.names = FALSE, recursive = FALSE)
  pkg_envs <- pkg_envs[file.exists(file.path(pkg_conf_dir, pkg_envs, "scenarios_config.yml"))]

  user_envs <- character(0)
  if (dir.exists(USER_CONFIGS_DIR)) {
    candidates <- list.dirs(USER_CONFIGS_DIR, full.names = FALSE, recursive = FALSE)
    candidates <- setdiff(candidates, "case_studies")
    user_envs <- candidates[file.exists(file.path(USER_CONFIGS_DIR, candidates, "scenarios_config.yml"))]
  }

  data.frame(
    name = c(setdiff(pkg_envs, user_envs), user_envs),
    source = c(rep("package", length(setdiff(pkg_envs, user_envs))), rep("user", length(user_envs))),
    stringsAsFactors = FALSE
  )
}

#' Directory (trailing slash included) holding scenarios_config.yml,
#' mcmc_config.yml and methods_config.R for a given environment name. User
#' environments take precedence over a same-named package one.
env_config_dir <- function(env) {
  user_dir <- file.path(USER_CONFIGS_DIR, env)
  if (file.exists(file.path(user_dir, "scenarios_config.yml"))) {
    return(paste0(user_dir, "/"))
  }
  paste0(system.file(file.path("conf", env), package = "BExTE"), "/")
}

#' Copy every case study an environment references into
#' user_configs/case_studies/ (skipping any already there) so that a single
#' case_studies_config_dir - always user_configs/case_studies/ for anything
#' run through the app - covers the whole environment, whether its case
#' studies are package-shipped, user-authored, or a mix.
ensure_case_studies_snapshot <- function(env) {
  bexte_ensure_user_dirs()
  config_dir <- env_config_dir(env)
  scenarios_config <- yaml::read_yaml(file.path(config_dir, "scenarios_config.yml"))

  for (case_study in scenarios_config$case_studies) {
    dst <- file.path(USER_CASE_STUDIES_DIR, paste0(case_study, ".yml"))
    if (!file.exists(dst)) {
      src <- case_study_path(case_study)
      if (src != "" && file.exists(src)) {
        file.copy(src, dst)
      }
    }
  }
  invisible(NULL)
}

#' Write the scenarios_config.yml / mcmc_config.yml / methods_config.R triad
#' for a new user-authored environment, and snapshot the case studies it
#' references (see ensure_case_studies_snapshot()).
save_environment <- function(env, scenarios_config, mcmc_config, methods_dict_selected) {
  bexte_validate_name(env, "environment")
  bexte_ensure_user_dirs()
  dir <- file.path(USER_CONFIGS_DIR, env)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  yaml::write_yaml(scenarios_config, file.path(dir, "scenarios_config.yml"))
  yaml::write_yaml(mcmc_config, file.path(dir, "mcmc_config.yml"))

  methods_config_text <- paste0("methods_dict <- ", paste(deparse(methods_dict_selected), collapse = "\n"), "\n")
  writeLines(methods_config_text, file.path(dir, "methods_config.R"))

  ensure_case_studies_snapshot(env)

  invisible(dir)
}

## ---- Methods template ------------------------------------------------------

#' Load the canonical methods_dict template (parameter metadata: label,
#' notation, type, important_values, display - everything except the actual
#' `range` a user wants to test), from the package's most complete
#' environment. Sourced into a throwaway environment so it doesn't leak into
#' .GlobalEnv (unlike when the simulation itself runs and needs it there).
read_methods_template <- function() {
  e <- new.env()
  source(system.file("conf/full/methods_config.R", package = "BExTE"), local = e)
  e$methods_dict
}

## ---- Results ---------------------------------------------------------------

#' List results/<env> directories that contain the frequentist result file the
#' Analyze tab requires, most recently modified first.
list_results_dirs <- function() {
  if (!dir.exists("results")) {
    return(character(0))
  }
  dirs <- list.dirs("results", full.names = TRUE, recursive = FALSE)
  has_results <- file.exists(file.path(dirs, "results_frequentist.csv"))
  dirs <- dirs[has_results]
  mtimes <- file.info(dirs)$mtime
  dirs[order(mtimes, decreasing = TRUE)]
}

#' Estimate the number of scenario rows and Monte Carlo evaluations represented
#' by the current Configure form. The exact run can differ slightly when a
#' binary drift grid loses inadmissible rate combinations, so the UI labels the
#' result as an estimate.
estimate_configured_workload <- function(case_studies, methods_dict, ndrift,
                                         sample_size_factors,
                                         denominator_change_factor,
                                         target_to_source_std_ratio_range,
                                         n_replicates) {
  if (length(case_studies) == 0 || length(methods_dict) == 0) {
    return(NULL)
  }

  method_rows <- sum(vapply(methods_dict, function(params) {
    if (length(params) == 0) return(1)
    prod(vapply(params, function(p) length(p$range), integer(1)))
  }, numeric(1)))

  case_rows <- sum(vapply(case_studies, function(name) {
    config <- read_case_study(name)
    if (is.null(config)) return(0)
    drift_rows <- ndrift + 3L
    denominator_rows <- if (config$endpoint %in% c("time_to_event", "recurrent_event") ||
                            (identical(config$endpoint, "binary") &&
                             identical(config$summary_measure_likelihood, "normal"))) {
      length(denominator_change_factor)
    } else 1L
    ratio_rows <- if (identical(config$endpoint, "continuous")) {
      length(target_to_source_std_ratio_range)
    } else 1L
    drift_rows * denominator_rows * ratio_rows * length(sample_size_factors)
  }, numeric(1)))

  scenarios <- as.double(method_rows) * as.double(case_rows)
  list(scenarios = scenarios, evaluations = scenarios * as.double(n_replicates))
}

#' Progress a run has reported for an environment, or NULL when there is
#' none to trust.
#'
#' A run writes logs/<env>/progress.json as it simulates each scenario (see
#' run_progress_tracker() in R/run_progress.R). That is finer-grained than
#' counting result rows, because a method that runs in parallel collects its
#' scenarios in the master and writes them all at once when it is finished.
#'
#' `since` drops a file left by an earlier run, the same way count_result_rows()
#' drops earlier results. Anything unreadable is reported as no progress
#' rather than raised: the file is written by another process, and a readout
#' that falls back to counting rows beats one that errors.
read_run_progress <- function(env, since = NULL) {
  ## Spelled out rather than taken from run_progress_path(), which is
  ## internal to the package: the app only ever sees what BExTE exports. The
  ## last test in test-shiny_app_run_progress.R reads what the package's own
  ## tracker writes, so the two cannot drift apart unnoticed.
  path <- file.path("logs", env, "progress.json")
  if (!file.exists(path)) {
    return(NULL)
  }
  if (!is.null(since)) {
    mtime <- file.mtime(path)
    if (is.na(mtime) || mtime < since) {
      return(NULL)
    }
  }

  snapshot <- tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  done <- suppressWarnings(as.integer(snapshot$done))
  total <- suppressWarnings(as.integer(snapshot$total))
  if (length(done) != 1 || length(total) != 1 || is.na(done) || is.na(total)) {
    return(NULL)
  }
  list(done = done, total = total)
}

#' Number of rows a results csv holds, header excluded.
#'
#' Counting physical lines would overcount severalfold: rng_state and
#' parameters are written as quoted fields whose values contain newlines, so
#' one scenario row spans several lines. A newline only ends a record when it
#' falls outside a quoted field, and a field is quoted exactly when an odd
#' number of quote characters precedes it (the doubled quotes RFC 4180 uses
#' for a literal quote cancel out in that count).
count_csv_records <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  if (length(lines) == 0) {
    return(0L)
  }
  quotes <- vapply(gregexpr('"', lines, fixed = TRUE), function(m) sum(m > 0L), integer(1))
  max(sum(cumsum(quotes) %% 2L == 0L) - 1L, 0L)
}

#' Best-effort count of scenario rows already written for an environment
#' (used as the running total for the progress bar), tolerant of the results
#' file(s) not existing yet.
#'
#' The simulation writes one file per case study and method while it works
#' (results/<env>/frequentist/<case study>/<method>/) and only concatenates
#' them into results/<env>/results_frequentist.csv once the pass is over, so
#' the per-method files are what to watch for live progress; the concatenated
#' file stands in for a run whose per-method files are already gone.
#'
#' `since` drops files older than the moment the run was launched.
#' run_simulation_env() does empty results/<env>/ itself, but not until the
#' background process has loaded the package and read its configuration -
#' seconds after the launch button was pressed - so relaunching an
#' environment would otherwise read as instantly complete on the previous
#' run's results.
count_result_rows <- function(env, since = NULL) {
  results_dir <- file.path("results", env)

  count_written_since <- function(paths) {
    paths <- paths[file.exists(paths)]
    if (!is.null(since)) {
      mtimes <- file.mtime(paths)
      paths <- paths[!is.na(mtimes) & mtimes >= since]
    }
    sum(vapply(paths, count_csv_records, integer(1)))
  }

  filenames <- c(frequentist = "results_frequentist.csv", bayesian = "results_bayesian_mc.csv")
  n <- 0L
  for (pass in names(filenames)) {
    filename <- filenames[[pass]]
    in_progress <- list.files(
      file.path(results_dir, pass),
      pattern = paste0("^", gsub(".", "\\.", filename, fixed = TRUE), "$"),
      recursive = TRUE,
      full.names = TRUE
    )
    n <- n + if (length(in_progress) > 0) {
      count_written_since(in_progress)
    } else {
      count_written_since(file.path(results_dir, filename))
    }
  }
  as.integer(max(n, 0L))
}

#' Estimate the total number of scenario rows an environment will produce
#' (frequentist pass), used as the progress bar's denominator. Best-effort:
#' returns NA if the scenario/methods configuration can't be resolved.
estimate_total_scenarios <- function(env) {
  config_dir <- env_config_dir(env)
  scenarios_config <- yaml::read_yaml(file.path(config_dir, "scenarios_config.yml"))

  tryCatch({
    ensure_case_studies_snapshot(env)
    case_studies_config_dir <- paste0(USER_CASE_STUDIES_DIR, "/")
    cases <- simulation_scenarios(config_dir = config_dir, scenarios_config = scenarios_config, case_studies_config_dir = case_studies_config_dir)
    nrow(cases)
  }, error = function(e) NA_integer_)
}

## ---- Plot globals -----------------------------------------------------------

analyze_globals_ready <- new.env()

ensure_plot_globals <- function() {
  if (isTRUE(analyze_globals_ready$done)) {
    return(invisible(NULL))
  }
  source(system.file("conf/plots_config.R", package = "BExTE"))
  source(system.file("conf/methods_plots_config.R", package = "BExTE"))
  source(system.file("conf/metrics_config.R", package = "BExTE"))
  analyze_globals_ready$done <- TRUE
  invisible(NULL)
}

#' Point every global the plot_*() functions expect at the right place for
#' this results directory, best-effort: if the env's own methods_config.R /
#' case studies can be found (because it was run through this app, or its
#' config folder happens to sit alongside), use them for correct parameter
#' labels; otherwise fall back to the package's default template so the
#' plots still render, with generic labels.
prepare_plot_globals_for_env <- function(env, figures_dir) {
  ensure_plot_globals()
  assign("figures_dir", figures_dir, envir = .GlobalEnv)
  assign("remake_figures", TRUE, envir = .GlobalEnv)

  methods_config_env <- new.env()
  used_fallback <- TRUE
  config_dir <- tryCatch(env_config_dir(env), error = function(e) "")
  methods_config_path <- if (nzchar(config_dir)) file.path(config_dir, "methods_config.R") else ""
  if (nzchar(methods_config_path) && file.exists(methods_config_path)) {
    tryCatch({
      source(methods_config_path, local = methods_config_env)
      used_fallback <- is.null(methods_config_env$methods_dict)
    }, error = function(e) NULL)
  }
  if (used_fallback) {
    assign("methods_dict", read_methods_template(), envir = .GlobalEnv)
  } else {
    assign("methods_dict", methods_config_env$methods_dict, envir = .GlobalEnv)
  }

  tryCatch(ensure_case_studies_snapshot(env), error = function(e) NULL)
  assign("case_studies_config_dir", paste0(USER_CASE_STUDIES_DIR, "/"), envir = .GlobalEnv)

  invisible(used_fallback)
}

## ---- Simulation runs -------------------------------------------------------

#' Start a simulation environment in a background process
#'
#' @description Shared by the Run page and the Replicate paper page. Loads the
#'   package with devtools::load_all() in the child process rather than
#'   requiring BExTE to be installed, so the app works from a source checkout.
#'
#' @param env An environment name known to `list_environments()`.
#'
#' @return A started `callr` process handle.
launch_simulation_run <- function(env) {
  ensure_case_studies_snapshot(env)
  config_dir <- env_config_dir(env)
  case_studies_config_dir <- paste0(USER_CASE_STUDIES_DIR, "/")

  analysis_config <- yaml::read_yaml(system.file("conf/analysis_config.yml", package = "BExTE"))
  simulation_config <- yaml::read_yaml(system.file("conf/simulation_config.yml", package = "BExTE"))
  metrics_env <- new.env()
  source(system.file("conf/metrics_config.R", package = "BExTE"), local = metrics_env)

  callr::r_bg(
    func = function(pkg_root, wd, env, config_dir, case_studies_config_dir,
                    simulation_config, analysis_config, frequentist_metrics,
                    inference_metrics) {
      setwd(wd)
      devtools::load_all(pkg_root, quiet = TRUE)
      run_simulation_env(
        env = env,
        config_dir = config_dir,
        case_studies_config_dir = case_studies_config_dir,
        simulation_config = simulation_config,
        analysis_config = analysis_config,
        frequentist_metrics = frequentist_metrics,
        inference_metrics = inference_metrics
      )
    },
    args = list(
      pkg_root = find.package("BExTE"),
      wd = getwd(),
      env = env,
      config_dir = config_dir,
      case_studies_config_dir = case_studies_config_dir,
      simulation_config = simulation_config,
      analysis_config = analysis_config,
      frequentist_metrics = metrics_env$frequentist_metrics,
      inference_metrics = metrics_env$inference_metrics
    ),
    stdout = tempfile(fileext = ".out"),
    stderr = tempfile(fileext = ".err")
  )
}
