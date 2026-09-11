#' Run a full simulation for one environment
#'
#' @description Runs the frequentist and/or Bayesian Monte Carlo operating
#'   characteristics simulation for a single environment. This mirrors the
#'   per-environment body of the driver loop in `inst/scripts/main.R`, extracted
#'   into a callable function so it can be invoked directly (e.g. from a
#'   background process launched by the Shiny app).
#'
#' @param env The environment name (used to name the `./logs/<env>/` and
#'   `./results/<env>/` directories).
#' @param config_dir Directory containing `scenarios_config.yml`,
#'   `mcmc_config.yml` and `methods_config.R` for this environment (must end
#'   with a trailing slash).
#' @param case_studies_config_dir Directory containing the case study YAML
#'   files referenced by this environment (must end with a trailing slash).
#' @param simulation_config Simulation configuration list (as read from
#'   `simulation_config.yml`).
#' @param analysis_config Analysis configuration list (as read from
#'   `analysis_config.yml`).
#' @param frequentist_metrics List of frequentist metrics, as defined by
#'   sourcing `metrics_config.R`.
#' @param inference_metrics List of inference metrics, as defined by sourcing
#'   `metrics_config.R`.
#' @param results_dir Directory the results are written to. Defaults to
#'   `./results/<env>/`.
#' @param check_results_completeness Reserved for a future completeness
#'   check; currently unused, mirroring `inst/scripts/main.R` where the
#'   corresponding call is commented out.
#'
#' @return `TRUE`, invisibly, once frequentist and/or Bayesian OCs have been
#'   computed and concatenated according to `simulation_config`.
#' @export
run_simulation_env <- function(env,
                               config_dir,
                               case_studies_config_dir,
                               simulation_config,
                               analysis_config,
                               frequentist_metrics,
                               inference_metrics,
                               results_dir = paste0("./results/", env, "/"),
                               check_results_completeness = TRUE) {
  # Set up the log file location
  dir.create(paste0("./logs/", env, "/checkpoints/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("./logs/", env, "/error_logs/"), showWarnings = FALSE, recursive = TRUE)
  # Empty the directory
  unlink(paste0("./logs/", env, "/checkpoints/*"), recursive = TRUE)
  unlink(paste0("./logs/", env, "/error_logs/*"), recursive = TRUE)

  LOGGING_FILE_PATH <- generate_log_filename(base_name = paste0("./logs/", env, "/error_logs/error_log.log"), suffix_type = "timestamp")

  scenarios_config <- read_config(paste0(config_dir, "scenarios_config.yml"), scenarios_config_schema)

  # Define the parallel logger
  if (scenarios_config$parallelization){
    # Parallel logger
    ParallelLogger::registerLogger(ParallelLogger::createLogger(
      name = "ParLogger",
      threshold = "INFO",
      appenders = list(
        ParallelLogger::createConsoleAppender(
          layout =  ParallelLogger::layoutSimple
        ),
        ParallelLogger::createFileAppender(
          layout =  ParallelLogger::layoutParallel,
          fileName = LOGGING_FILE_PATH
        )
      )
    ))


    ParallelLogger::registerLogger(ParallelLogger::createLogger(name = "DEFAULT_ERRORREPORT_LOGGER",
                                threshold = "FATAL",
                                appenders = list(ParallelLogger::createFileAppender(layout =  ParallelLogger::layoutErrorReport,
                                                                    fileName = paste0("./logs/", env, "/error_report.txt"),
                                                                    overwrite = TRUE,
                                                                    expirationTime = 60))))
  } else {
    # Set up the new log file
    futile.logger::flog.appender(futile.logger::appender.file(LOGGING_FILE_PATH))

    # Set the global error handler
    options(error = global_error_handler)
  }

  outputs_config <- yaml::read_yaml(system.file("conf/outputs_config.yml", package = "BExTE"))
  ocs_filename <- outputs_config$frequentist_ocs_results_filename

  if (simulation_config$compute_frequentist_ocs == TRUE) {
    if (simulation_config$delete_old_results) {
      unlink(results_dir, recursive = TRUE, force = TRUE)
      dir.create(results_dir)  # This recreates the empty directory
    }

    simulation_frequentist_ocs(env = env,
    simulation_config = simulation_config,
    scenarios_config = scenarios_config,
    analysis_config = analysis_config,
    config_dir = config_dir,
    case_studies_config_dir = case_studies_config_dir,
    logging_file_path = LOGGING_FILE_PATH,
    frequentist_metrics = frequentist_metrics,
    inference_metrics = inference_metrics)

    # Concatenate case_study/method results into a single file for the environment.
    concatenate_simulation_results(results_dir = results_dir, ocs_filename = ocs_filename)

    # frequentist_power_at_equivalent_tie()/frequentist_power_at_nominal_tie()
    # (in R/analysis_operating_characteristics.R) call load_data() without
    # threading case_studies_config_dir through, so they only resolve
    # package-shipped case studies. Skip them for a case study outside the
    # package rather than crash; sweet_spot and bayesian_ocs (which do
    # thread case_studies_config_dir through) still run either way.
    all_case_studies_shipped <- all(vapply(scenarios_config$case_studies, function(cs) {
      nzchar(system.file(file.path("conf", "case_studies", paste0(cs, ".yml")), package = "BExTE"))
    }, logical(1)))
    analysis_to_compute <- if (all_case_studies_shipped) {
      c("frequentist_power_at_equivalent_tie", "frequentist_power_at_nominal_tie", "sweet_spot", "bayesian_ocs")
    } else {
      c("sweet_spot", "bayesian_ocs")
    }

    # The analysis is performed on the results concatenated at the level of the environment.
    simulation_analysis(env, analysis_config, config_dir, frequentist_metrics,
                        to_compute = analysis_to_compute,
                        case_studies_config_dir = case_studies_config_dir,
                        parallelization = scenarios_config$parallelization)
  }

  if (simulation_config$compute_bayesian_ocs_mc == TRUE) {
    if (simulation_config$delete_old_results) {
      folder_dir <- file.path(results_dir, "bayesian")
      if (file.exists(folder_dir)) {
        unlink(folder_dir, recursive = TRUE)
      }
    }

    simulation_bayesian_ocs(
      env = env,
      simulation_config = simulation_config,
      scenarios_config = scenarios_config,
      analysis_config = analysis_config,
      config_dir = config_dir,
      case_studies_config_dir = case_studies_config_dir
    )
    ocs_filename <- outputs_config$bayesian_ocs_mc_results_filename
    concatenate_simulation_results(results_dir = results_dir, ocs_filename = ocs_filename)
  }

  invisible(TRUE)
}
