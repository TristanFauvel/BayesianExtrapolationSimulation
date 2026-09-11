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

## ---- Export ------------------------------------------------------------

## Source the plot style/config globals the generators expect.
##
## The plot_*() functions (and forest_plot()) resolve several inputs as free
## variables out of .GlobalEnv rather than taking them as arguments - style
## constants such as font and textwidth, the metric lookup tables
## frequentist_metrics/inference_metrics, the drift-axis helper xvars, and the
## method labelling tables methods_dict/methods_labels. This is exactly how
## inst/scripts/plots.R has always driven them, and how
## inst/shiny_app/modules/mod_analyze.R's ensure_plot_globals()/
## prepare_plot_globals_for_env() sets them up for the Analyze page. This
## mirrors that: source the three config files that define the style
## constants and metric tables, then set methods_dict from the package's
## canonical template (mirroring read_methods_template() in
## inst/shiny_app/helpers.R), since the exporter has no results-environment
## methods_config.R of its own to prefer.
ensure_paper_plot_globals <- function() {
  source(system.file("conf/plots_config.R", package = "RBExT"))
  source(system.file("conf/methods_plots_config.R", package = "RBExT"))
  source(system.file("conf/metrics_config.R", package = "RBExT"))

  methods_template_env <- new.env()
  source(system.file("conf/full/methods_config.R", package = "RBExT"), local = methods_template_env)
  assign("methods_dict", methods_template_env$methods_dict, envir = .GlobalEnv)

  invisible(NULL)
}

#' Build the context one manifest entry's generator receives
#'
#' @description The generators take the slice already narrowed to their own
#'   case study, sample size, denominator factor and standard deviation ratio,
#'   mirroring what the loop functions in inst/scripts/plots.R pass them.
#'
#' @keywords internal
paper_entry_context <- function(entry, results_df, results_dir, tables_dir,
                                case_studies_config_dir, case_studies,
                                sample_size_factors, analysis_config) {
  ctx <- list(
    results_dir = results_dir,
    tables_dir = tables_dir,
    case_studies_config_dir = case_studies_config_dir,
    case_studies = case_studies,
    sample_size_factors = sample_size_factors,
    analysis_config = analysis_config
  )

  if (is.na(entry$case_study)) {
    return(ctx)
  }

  per_arm <- paper_sample_size_per_arm(
    entry$case_study, entry$sample_size_factor, case_studies_config_dir
  )
  case_config <- yaml::read_yaml(
    file.path(case_studies_config_dir, paste0(entry$case_study, ".yml"))
  )

  slice <- results_df[
    results_df$case_study == entry$case_study &
      results_df$target_sample_size_per_arm == per_arm &
      (results_df$source_denominator_change_factor == 1 |
         is.na(results_df$source_denominator_change_factor)) &
      (results_df$target_to_source_std_ratio == 1 |
         is.na(results_df$target_to_source_std_ratio)),
    ,
    drop = FALSE
  ]
  if (nrow(slice) == 0) {
    stop(
      "No rows for ", entry$case_study, " at ", per_arm,
      " per arm with denominator factor 1 and standard deviation ratio 1."
    )
  }

  ctx$df <- slice
  ctx$target_sample_size_per_arm <- per_arm
  ctx$theta_0 <- case_config$theta_0
  ctx
}

#' Produce the paper's figures and tables
#'
#' @description Runs each selected manifest entry's generator against
#'   `results_dir` and writes the results under `figures_dir` and `tables_dir`,
#'   keeping the generators' own filenames. `manifest.csv` records which paper
#'   item each file belongs to.
#'
#'   One entry failing does not abort the batch: it is recorded as `failed`
#'   with its error message and the run continues.
#'
#' @param results_dir A `results/<env>/` directory.
#' @param figures_dir Output directory for figures, trailing slash included -
#'   the plot generators append their own `<case_study>/` below it.
#' @param tables_dir Output directory for tables and the manifest.
#' @param ids Manifest ids to produce.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#' @param progress Optional `function(index, total, id)` progress callback.
#'
#' @return A status data frame, invisibly.
#'
#' @export
export_paper_outputs <- function(results_dir, figures_dir, tables_dir, ids,
                                 case_studies_config_dir, progress = NULL) {
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

  results_df <- readr::read_csv(
    file.path(results_dir, "results_frequentist.csv"),
    show_col_types = FALSE
  )
  analysis_config <- yaml::read_yaml(
    system.file("conf/analysis_config.yml", package = "RBExT")
  )

  entries <- lapply(ids, paper_manifest_entry)
  ## as.character()/as.numeric() strip the attributes na.omit() leaves behind.
  all_case_studies <- vapply(entries, function(e) e$case_study, character(1))
  case_studies <- unique(as.character(all_case_studies[!is.na(all_case_studies)]))
  all_factors <- vapply(entries, function(e) e$sample_size_factor, numeric(1))
  sample_size_factors <- sort(unique(as.numeric(all_factors[!is.na(all_factors)])))

  ## The plot generators resolve figures_dir, remake_figures and a set of
  ## style/config constants (font, textwidth, methods_dict, ...) as free
  ## variables out of .GlobalEnv - see inst/scripts/plots.R and
  ## ensure_paper_plot_globals() above, modelled on
  ## inst/shiny_app/modules/mod_analyze.R's ensure_plot_globals()/
  ## prepare_plot_globals_for_env().
  ensure_paper_plot_globals()

  previous_figures_dir <- if (exists("figures_dir", envir = .GlobalEnv)) {
    get("figures_dir", envir = .GlobalEnv)
  } else {
    NULL
  }
  assign("figures_dir", figures_dir, envir = .GlobalEnv)
  assign("remake_figures", TRUE, envir = .GlobalEnv)
  on.exit({
    if (is.null(previous_figures_dir)) {
      suppressWarnings(rm("figures_dir", envir = .GlobalEnv))
    } else {
      assign("figures_dir", previous_figures_dir, envir = .GlobalEnv)
    }
  }, add = TRUE)

  rows <- lapply(seq_along(entries), function(index) {
    entry <- entries[[index]]
    if (!is.null(progress)) {
      progress(index, length(entries), entry$id)
    }

    before <- c(
      list.files(figures_dir, recursive = TRUE, full.names = TRUE),
      list.files(tables_dir, recursive = TRUE, full.names = TRUE)
    )

    result <- tryCatch({
      ctx <- paper_entry_context(
        entry, results_df, results_dir, tables_dir, case_studies_config_dir,
        case_studies, sample_size_factors, analysis_config
      )
      entry$generator(ctx)
      list(status = "ok", message = "")
    }, error = function(e) {
      list(status = "failed", message = conditionMessage(e))
    })

    after <- c(
      list.files(figures_dir, recursive = TRUE, full.names = TRUE),
      list.files(tables_dir, recursive = TRUE, full.names = TRUE)
    )
    written <- setdiff(after, before)

    data.frame(
      id = entry$id,
      kind = entry$kind,
      caption = entry$caption,
      case_study = entry$case_study,
      target_sample_size_per_arm = if (is.na(entry$case_study)) {
        NA_real_
      } else {
        paper_sample_size_per_arm(entry$case_study, entry$sample_size_factor,
                                  case_studies_config_dir)
      },
      metric = entry$metric,
      status = result$status,
      message = result$message,
      outputs = paste(written, collapse = "; "),
      results_dir = results_dir,
      stringsAsFactors = FALSE
    )
  })

  status <- do.call(rbind, rows)
  readr::write_csv(status, file.path(tables_dir, "manifest.csv"))

  writeLines(
    c(
      "# Paper figures and tables",
      "",
      paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             " from ", results_dir, "."),
      "",
      "Filenames are the generators' own; `manifest.csv` maps each one to its",
      "paper figure or table number, caption and scenario.",
      "",
      "Not produced here: tables S3-S6 (design priors and the definitions of",
      "the Bayesian operating characteristics) are hand-authored in the",
      "manuscript, and table S8 is out of scope. Figure S8 is produced.",
      "",
      paste0(sum(status$status == "ok"), " of ", nrow(status),
             " items produced successfully.")
    ),
    file.path(tables_dir, "README.md")
  )

  invisible(status)
}
