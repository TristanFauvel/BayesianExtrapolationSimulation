## Deriving the smallest simulation config that still reproduces a chosen set
## of paper figures, and checking an existing results directory against them.

## Every method appears in the forest plots, so a selection never narrows this.
PAPER_METHODS <- c(
  "RMP", "separate", "pooling", "conditional_power_prior",
  "test_then_pool_equivalence", "test_then_pool_difference",
  "p_value_based_PP", "EB_PP", "PDCCPP", "NPP", "commensurate_power_prior"
)

#' Minimal simulation config for a set of paper figures
#'
#' @description Folds the selected manifest entries into a `scenarios_config`
#'   covering exactly the case studies and sample size factors they need. The
#'   fidelity settings do not scale with the selection: `ndrift` stays at 30
#'   because `forest_plot()` selects the three principal treatment-effect
#'   scenarios by nearest grid point, so a coarser grid would quietly plot
#'   different drift values rather than failing.
#'
#'   No paper figure varies the source denominator change factor or the
#'   target-to-source standard deviation ratio, so both are pinned to 1.
#'
#' @param ids Manifest ids to cover.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#'
#' @return A `scenarios_config` list, ready for `save_environment()`.
#'
#' @export
paper_replication_requirements <- function(ids, case_studies_config_dir) {
  entries <- lapply(ids, paper_manifest_entry)
  entries <- Filter(function(entry) !is.na(entry$case_study), entries)

  case_studies <- unique(vapply(entries, function(e) e$case_study, character(1)))
  factors <- unique(vapply(entries, function(e) e$sample_size_factor, numeric(1)))

  list(
    n_replicates = 10000,
    ndrift = 30,
    parallelization = TRUE,
    denominator_change_factor = 1,
    sample_size_factors = sort(factors),
    target_to_source_std_ratio_range = 1,
    case_studies = case_studies,
    methods = PAPER_METHODS
  )
}

#' Check a results frame against a set of paper figures
#'
#' @param results_df A frequentist results frame.
#' @param ids Manifest ids to check.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#'
#' @return A data frame with columns `id`, `covered` and `reason`.
#'
#' @export
paper_replication_coverage <- function(results_df, ids, case_studies_config_dir) {
  rows <- lapply(ids, function(id) {
    entry <- paper_manifest_entry(id)

    if (is.na(entry$case_study)) {
      return(data.frame(id = id, covered = TRUE, reason = "", stringsAsFactors = FALSE))
    }

    if (!entry$case_study %in% results_df$case_study) {
      return(data.frame(
        id = id, covered = FALSE,
        reason = paste0("no ", entry$case_study, " rows"),
        stringsAsFactors = FALSE
      ))
    }

    per_arm <- paper_sample_size_per_arm(
      entry$case_study, entry$sample_size_factor, case_studies_config_dir
    )
    slice <- results_df[results_df$case_study == entry$case_study &
                          results_df$target_sample_size_per_arm == per_arm, , drop = FALSE]
    if (nrow(slice) == 0) {
      return(data.frame(
        id = id, covered = FALSE,
        reason = paste0("no rows at ", per_arm, " per arm"),
        stringsAsFactors = FALSE
      ))
    }

    if (!is.na(entry$metric) && !entry$metric %in% names(results_df)) {
      return(data.frame(
        id = id, covered = FALSE,
        reason = paste0("no ", entry$metric, " column"),
        stringsAsFactors = FALSE
      ))
    }

    data.frame(id = id, covered = TRUE, reason = "", stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}
