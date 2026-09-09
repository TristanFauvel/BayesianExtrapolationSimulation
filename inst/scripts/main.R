rm(list = ls())

library(RBExT)

devtools::load_all() # FIXME

# If the envs variable is not defined as an environment variable, define it.
envs <- ifelse(Sys.getenv("envs") != "", Sys.getenv("envs"), c("fast_cases_config"))

envs <- c("aprepitant_mcmc_config_light")
# envs = c("fast_cases_config")
#envs = c("tests")

options(readr.show_col_types = FALSE) #  FALSE : prevent the column specification message from appearing every time read_csv() is used.

concat_all_results <- TRUE
delete_stan_files <- FALSE
check_results_completeness <- TRUE

case_studies_config_dir <- paste0(system.file("conf/case_studies", package = "RBExT"), "/")

analysis_config <- yaml::read_yaml(system.file("conf/analysis_config.yml", package = "RBExT"))

simulation_config <- yaml::read_yaml(system.file("conf/simulation_config.yml", package = "RBExT"))

source(system.file(paste0("conf/metrics_config.R"), package = "RBExT"))

closeAllConnections()

if (delete_stan_files == TRUE) {
  # Delete all Stan files (to make sure models are recompiled)
  stan_files <- list.files(path = system.file("stan", package = "RBExT"), pattern = "\\.stan$", full.names = TRUE)
  exe_files <- list.files(path = system.file("stan", package = "RBExT"), pattern = "\\.exe$", full.names = TRUE)

  # Combine the lists of .stan and .exe files and remove them
  files_to_delete <- c(stan_files, exe_files)
  file.remove(files_to_delete)
}

for (env in envs) {
  run_simulation_env(
    env = env,
    config_dir = paste0(system.file(paste0("conf/", env), package = "RBExT"), "/"),
    case_studies_config_dir = case_studies_config_dir,
    simulation_config = simulation_config,
    analysis_config = analysis_config,
    frequentist_metrics = frequentist_metrics,
    inference_metrics = inference_metrics,
    check_results_completeness = check_results_completeness
  )
}
#
#
# if (concat_all_results == TRUE) {
#   closeAllConnections()
#
#   envs <-  c("aprepitant_mcmc_config", "commensurate_config", "fast_cases_config", "commensurate_config", "aprepitant_approx_config", "npp_config")
#   file_names <- c(outputs_config$frequentist_ocs_results_filename, "sweet_spot.csv", outputs_config$bayesian_ocs_deterministic_results_filename)
#
#   results_dirs <- lapply(envs, function(env) paste0("./results/", env, "/"))
#
#   # Concatenate and save each file type
#   lapply(file_names, concatenate_files, results_dirs, "./results/combined/")
# }
#
