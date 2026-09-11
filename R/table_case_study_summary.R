## Supplementary tables S1 and S7. Both are derived from the case study YAMLs
## rather than from simulation output.

#' Total target-study sample sizes considered for each case study (table S1)
#'
#' @param case_studies Character vector of case study names.
#' @param sample_size_factors Numeric vector of sample size factors.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#' @param tables_dir Directory to write into.
#'
#' @return The path of the written `.tex` file.
#'
#' @export
table_target_sample_sizes <- function(case_studies, sample_size_factors,
                                      case_studies_config_dir, tables_dir) {
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

  rows <- lapply(case_studies, function(case_study) {
    config <- yaml::read_yaml(file.path(case_studies_config_dir, paste0(case_study, ".yml")))
    per_arm <- vapply(
      sample_size_factors,
      function(factor) paper_sample_size_per_arm(case_study, factor, case_studies_config_dir),
      numeric(1)
    )
    row <- as.data.frame(as.list(2 * per_arm))
    ## LaTeX treats a bare "_" outside math mode as an error, so escape it -
    ## export_table() only escapes "%" in column names.
    names(row) <- paste0("N\\_T (factor ", sample_size_factors, ")")
    cbind(data.frame(`Case study` = config$name, check.names = FALSE), row)
  })

  table_data <- do.call(rbind, rows)
  file_path <- file.path(tables_dir, "target_sample_sizes")

  export_table(
    data_table = table_data,
    title = "Total target-study sample sizes considered for each case study.",
    file_path = file_path,
    longtable = FALSE
  )

  paste0(file_path, ".tex")
}

#' Summary of the clinical case studies (table S7)
#'
#' @param case_studies Character vector of case study names.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#' @param tables_dir Directory to write into.
#'
#' @return The path of the written `.tex` file.
#'
#' @export
table_case_study_summary <- function(case_studies, case_studies_config_dir, tables_dir) {
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

  rows <- lapply(case_studies, function(case_study) {
    config <- yaml::read_yaml(file.path(case_studies_config_dir, paste0(case_study, ".yml")))
    data.frame(
      `Case study` = config$name,
      `Control` = config$control,
      `Endpoint` = config$endpoint,
      `Summary measure` = config$summary_measure_likelihood,
      ## control + treatment, not the `total:` field, which is stale for
      ## aprepitant - see R/simulation_scenarios.R.
      `Source N` = config$source$control + config$source$treatment,
      `Source effect` = round(config$source$treatment_effect, 4),
      `Source SE` = round(config$source$standard_error, 4),
      `Target N` = config$target$control + config$target$treatment,
      `Target effect` = round(config$target$treatment_effect, 4),
      `Target SE` = round(config$target$standard_error, 4),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  table_data <- do.call(rbind, rows)
  file_path <- file.path(tables_dir, "case_study_summary")

  export_table(
    data_table = table_data,
    title = "Summary of the clinical case studies used to construct the simulation-study design.",
    file_path = file_path,
    longtable = FALSE
  )

  paste0(file_path, ".tex")
}
