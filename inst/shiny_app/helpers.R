## Shared helpers for the RBExT Shiny app.
##
## Convention: every path here is relative to the repository root, matching
## the convention already used by inst/scripts/main.R ("./results/<env>/",
## "./logs/<env>/"). run_rbext_app() is responsible for making sure the app
## (and any callr background process it spawns) runs with that as its
## working directory.

USER_CONFIGS_DIR <- "user_configs"
USER_CASE_STUDIES_DIR <- file.path(USER_CONFIGS_DIR, "case_studies")

`%||%` <- function(a, b) if (is.null(a)) b else a

rbext_ensure_user_dirs <- function() {
  dir.create(USER_CASE_STUDIES_DIR, showWarnings = FALSE, recursive = TRUE)
}

## ---- Case studies ---------------------------------------------------------

#' List case studies available to the app: package-shipped ones (read-only)
#' and user-authored ones (in user_configs/case_studies/). If a user-authored
#' case study shares a name with a package one, the user's version wins.
list_case_studies <- function() {
  pkg_dir <- system.file("conf/case_studies", package = "RBExT")
  pkg_files <- list.files(pkg_dir, pattern = "\\.yml$", full.names = FALSE)
  user_files <- if (dir.exists(USER_CASE_STUDIES_DIR)) {
    list.files(USER_CASE_STUDIES_DIR, pattern = "\\.yml$", full.names = FALSE)
  } else {
    character(0)
  }

  pkg_names <- tools::file_path_sans_ext(pkg_files)
  user_names <- tools::file_path_sans_ext(user_files)

  data.frame(
    name = c(setdiff(pkg_names, user_names), user_names),
    source = c(rep("package", length(setdiff(pkg_names, user_names))), rep("user", length(user_names))),
    stringsAsFactors = FALSE
  )
}

case_study_path <- function(name) {
  user_path <- file.path(USER_CASE_STUDIES_DIR, paste0(name, ".yml"))
  if (file.exists(user_path)) {
    return(user_path)
  }
  system.file(file.path("conf/case_studies", paste0(name, ".yml")), package = "RBExT")
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
  rbext_ensure_user_dirs()

  if (endpoint == "binary") {
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
  pkg_conf_dir <- system.file("conf", package = "RBExT")
  pkg_envs <- list.dirs(pkg_conf_dir, full.names = FALSE, recursive = FALSE)
  pkg_envs <- pkg_envs[file.exists(file.path(pkg_conf_dir, pkg_envs, "scenarios_config.yml"))]

  user_envs <- character(0)
  if (dir.exists(USER_CONFIGS_DIR)) {
    candidates <- list.dirs(USER_CONFIGS_DIR, full.names = FALSE, recursive = FALSE)
    candidates <- setdiff(candidates, "case_studies")
    user_envs <- candidates[file.exists(file.path(USER_CONFIGS_DIR, candidates, "scenarios_config.yml"))]
  }

  data.frame(
    name = c(pkg_envs, user_envs),
    source = c(rep("package", length(pkg_envs)), rep("user", length(user_envs))),
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
  paste0(system.file(file.path("conf", env), package = "RBExT"), "/")
}

#' Copy every case study an environment references into
#' user_configs/case_studies/ (skipping any already there) so that a single
#' case_studies_config_dir - always user_configs/case_studies/ for anything
#' run through the app - covers the whole environment, whether its case
#' studies are package-shipped, user-authored, or a mix.
ensure_case_studies_snapshot <- function(env) {
  rbext_ensure_user_dirs()
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
  rbext_ensure_user_dirs()
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
  source(system.file("conf/full/methods_config.R", package = "RBExT"), local = e)
  e$methods_dict
}

## ---- Results ---------------------------------------------------------------

#' List results/<env> directories that look like simulation output (contain
#' at least one *.csv), most recently modified first.
list_results_dirs <- function() {
  if (!dir.exists("results")) {
    return(character(0))
  }
  dirs <- list.dirs("results", full.names = TRUE, recursive = FALSE)
  has_csv <- vapply(dirs, function(d) length(list.files(d, pattern = "\\.csv$", recursive = TRUE)) > 0, logical(1))
  dirs <- dirs[has_csv]
  mtimes <- file.info(dirs)$mtime
  dirs[order(mtimes, decreasing = TRUE)]
}

#' Best-effort count of scenario rows already written for an environment
#' (used as the running total for the progress bar), tolerant of the results
#' file(s) not existing yet.
count_result_rows <- function(env) {
  results_dir <- file.path("results", env)
  freq_path <- file.path(results_dir, "results_frequentist.csv")
  bayes_path <- file.path(results_dir, "results_bayesian_mc.csv")
  n <- 0L
  if (file.exists(freq_path)) {
    n <- n + tryCatch(length(readLines(freq_path)) - 1L, error = function(e) 0L)
  }
  if (file.exists(bayes_path)) {
    n <- n + tryCatch(length(readLines(bayes_path)) - 1L, error = function(e) 0L)
  }
  max(n, 0L)
}

#' Estimate the total number of scenario rows an environment will produce
#' (frequentist pass), used as the progress bar's denominator. Best-effort:
#' returns NA if the scenario/methods configuration can't be resolved.
estimate_total_scenarios <- function(env) {
  config_dir <- env_config_dir(env)
  scenarios_config <- yaml::read_yaml(file.path(config_dir, "scenarios_config.yml"))

  tryCatch({
    ensure_case_studies_snapshot(env)
    assign("case_studies_config_dir", paste0(USER_CASE_STUDIES_DIR, "/"), envir = .GlobalEnv)
    cases <- simulation_scenarios(config_dir = config_dir, scenarios_config = scenarios_config)
    nrow(cases)
  }, error = function(e) NA_integer_)
}
