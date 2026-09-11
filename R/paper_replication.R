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
  source(system.file("conf/plots_config.R", package = "BExTE"))
  source(system.file("conf/methods_plots_config.R", package = "BExTE"))
  source(system.file("conf/metrics_config.R", package = "BExTE"))

  methods_template_env <- new.env()
  source(system.file("conf/full/methods_config.R", package = "BExTE"), local = methods_template_env)
  assign("methods_dict", methods_template_env$methods_dict, envir = .GlobalEnv)

  invisible(NULL)
}

## Snapshot every name currently bound in .GlobalEnv (and the current ggplot
## theme), so a later call to paper_restore_globals() can put the caller's
## session back exactly as it was. Unlike mod_analyze.R's
## ensure_plot_globals() - which is allowed to leave these set because it
## lives inside a single long-running Shiny session - export_paper_outputs()
## is an exported library function that a script or interactive session can
## call directly, so it must not leak font/textwidth/methods_dict/... (or a
## replaced ggplot2::theme_set()) into the caller's .GlobalEnv.
paper_snapshot_globals <- function() {
  names_before <- ls(envir = .GlobalEnv, all.names = TRUE)
  list(
    names_before = names_before,
    values_before = mget(names_before, envir = .GlobalEnv),
    theme_before = ggplot2::theme_get()
  )
}

## Restore .GlobalEnv (and the ggplot theme) to what paper_snapshot_globals()
## recorded: reassign every name that already existed back to its old value,
## and remove every name that ensure_paper_plot_globals()/figures_dir/
## remake_figures newly introduced. Determining "newly introduced" via
## setdiff(ls(.GlobalEnv), snapshot$names_before) - rather than a hard-coded
## list of names - means this keeps working if the conf/*.R files start
## defining (or stop defining) globals.
paper_restore_globals <- function(snapshot) {
  names_now <- ls(envir = .GlobalEnv, all.names = TRUE)
  new_names <- setdiff(names_now, snapshot$names_before)
  if (length(new_names) > 0) {
    suppressWarnings(rm(list = new_names, envir = .GlobalEnv))
  }
  for (name in snapshot$names_before) {
    assign(name, snapshot$values_before[[name]], envir = .GlobalEnv)
  }
  ggplot2::theme_set(snapshot$theme_before)
  invisible(NULL)
}

## A snapshot of every figure/table file already on disk, with enough to tell
## whether a later write is a genuinely new file or an overwrite of an
## existing one - see paper_outputs_written() below.
paper_output_snapshot <- function(figures_dir, tables_dir) {
  paths <- c(
    list.files(figures_dir, recursive = TRUE, full.names = TRUE),
    list.files(tables_dir, recursive = TRUE, full.names = TRUE)
  )
  info <- file.info(paths, extra_cols = FALSE)
  data.frame(
    path = paths,
    mtime = info$mtime,
    size = info$size,
    stringsAsFactors = FALSE
  )
}

## Which paths in `after` (a paper_output_snapshot()) were written since
## `before` was taken: either the path did not exist before, or its mtime or
## size changed. A plain before/after path-list diff (the brief's approach)
## mis-attributes provenance on a second export into the same directories -
## with remake_figures TRUE, every path from the first run is already present
## in "before" on a second run, so nothing would ever look new. Comparing
## mtime (checked at microsecond resolution on this filesystem - verified
## with back-to-back writes carrying no sleep in between, see the task-5 fix
## report) plus size catches an overwrite that reproduces the same bytes but
## still counts as "produced by this run".
paper_outputs_written <- function(before, after) {
  match_index <- match(after$path, before$path)
  is_new <- is.na(match_index)
  changed <- !is_new & (
    after$mtime != before$mtime[match_index] |
      after$size != before$size[match_index]
  )
  after$path[is_new | changed]
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
    system.file("conf/analysis_config.yml", package = "BExTE")
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
  ## prepare_plot_globals_for_env(). Unlike that Shiny module, this is an
  ## exported function a caller can invoke from their own script or
  ## interactive session, so every name it is about to set is snapshotted
  ## first and restored via on.exit(), including the ggplot theme
  ## conf/plots_config.R replaces with theme_set().
  globals_snapshot <- paper_snapshot_globals()
  on.exit(paper_restore_globals(globals_snapshot), add = TRUE)

  ensure_paper_plot_globals()
  assign("figures_dir", figures_dir, envir = .GlobalEnv)
  assign("remake_figures", TRUE, envir = .GlobalEnv)

  rows <- lapply(seq_along(entries), function(index) {
    entry <- entries[[index]]
    if (!is.null(progress)) {
      progress(index, length(entries), entry$id)
    }

    before <- paper_output_snapshot(figures_dir, tables_dir)

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

    after <- paper_output_snapshot(figures_dir, tables_dir)
    written <- paper_outputs_written(before, after)

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
