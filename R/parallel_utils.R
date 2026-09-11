#' Determine a safe number of parallel workers
#'
#' @param max_workers Maximum number of workers to use.
#' @param detected_cores Number of detected logical CPU cores.
#'
#' @return A positive integer worker count.
#' @noRd
get_parallel_worker_count <- function(
  max_workers = Inf,
  detected_cores = parallel::detectCores()
) {
  if (length(detected_cores) != 1L ||
      is.na(detected_cores) ||
      detected_cores < 2L) {
    return(1L)
  }

  as.integer(min(detected_cores - 1L, max_workers))
}

#' Cap chain-level parallelism when scenarios already run in parallel
#'
#' When the scenario loop is parallelised, every worker would otherwise start
#' `parallel_chains` CmdStan processes of its own, oversubscribing the machine by
#' the product of the two. Running the chains sequentially inside each worker
#' keeps the total number of processes equal to the number of workers.
#'
#' @param mcmc_config MCMC configuration read from `mcmc_config.yml`.
#' @param parallelization Whether scenarios are simulated in parallel.
#'
#' @return The MCMC configuration, with `parallel_chains` capped at 1 when
#'   scenarios run in parallel.
#' @noRd
limit_mcmc_chain_parallelism <- function(mcmc_config, parallelization) {
  if (isTRUE(parallelization)) {
    mcmc_config$parallel_chains <- 1L
  }

  return(mcmc_config)
}

#' Whether one method's scenarios run in parallel
#'
#' `parallelization` in the scenarios configuration is either a single logical,
#' which applies to every method in the run, or a list of method names, which
#' parallelises only those. The per-method form exists because the cost profiles
#' differ: a method left on the replicate loop gains the full worker count from
#' scenario-level parallelism, while a vectorised one already holds a large
#' posterior mixture in memory and multiplies that footprint by every worker.
#'
#' @param parallelization The `parallelization` entry of the scenarios config.
#' @param method Method name.
#'
#' @return A single logical.
#' @noRd
method_runs_in_parallel <- function(parallelization, method) {
  if (is.null(parallelization)) {
    return(FALSE)
  }

  if (is.logical(parallelization)) {
    if (length(parallelization) != 1L || is.na(parallelization)) {
      stop(
        "parallelization must be a single TRUE or FALSE, or a list of method names.",
        call. = FALSE
      )
    }
    return(parallelization)
  }

  methods <- unlist(parallelization, use.names = FALSE)
  if (!is.character(methods) || length(methods) == 0L) {
    stop(
      "parallelization must be a single TRUE or FALSE, or a list of method names.",
      call. = FALSE
    )
  }

  method %in% methods
}

#' Whether the post-processing analysis runs in parallel
#'
#' `parallelization` is either a single logical or a list of method names (see
#' [method_runs_in_parallel()]). The analysis loops over every result row at
#' once rather than one method at a time, so the per-method form collapses to
#' "run in parallel if any method does".
#'
#' @param parallelization The `parallelization` entry of the scenarios config.
#'
#' @return A single logical.
#' @noRd
analysis_runs_in_parallel <- function(parallelization) {
  if (is.null(parallelization)) {
    return(FALSE)
  }

  if (is.logical(parallelization)) {
    if (length(parallelization) != 1L || is.na(parallelization)) {
      stop(
        "parallelization must be a single TRUE or FALSE, or a list of method names.",
        call. = FALSE
      )
    }
    return(parallelization)
  }

  methods <- unlist(parallelization, use.names = FALSE)
  if (!is.character(methods) || length(methods) == 0L) {
    stop(
      "parallelization must be a single TRUE or FALSE, or a list of method names.",
      call. = FALSE
    )
  }

  TRUE
}

#' Result rows below which the analysis is not worth a cluster
#'
#' Standing the cluster up costs around 12 seconds on a source checkout,
#' because every worker loads the package with devtools::load_all(), while a
#' row costs about 0.07 seconds to analyse. Parallelism only pays for itself
#' past roughly `startup / (row_cost * (1 - 1 / workers))` rows - about 185
#' on a 12-core machine - and below that the cluster is pure overhead: a
#' 108-row environment takes 7.6s sequentially against 27.5s on 11 workers.
#' @noRd
ANALYSIS_PARALLEL_MIN_ROWS <- 200L

#' Whether to analyse this many rows on a cluster
#'
#' @param parallelization Whether the caller asked for parallelism.
#' @param n_rows Number of result rows to analyse.
#' @param min_rows Row count below which the cluster costs more than it saves.
#'
#' @return A single logical.
#' @noRd
analysis_uses_cluster <- function(parallelization, n_rows,
                                  min_rows = ANALYSIS_PARALLEL_MIN_ROWS) {
  isTRUE(parallelization) && n_rows >= min_rows
}

#' Make BExTE (and any other packages) available in every worker of a cluster
#'
#' BExTE is not necessarily an installed package: both `inst/scripts/main.R`
#' and the Shiny app's background process `devtools::load_all()` it from
#' source, and a worker that only calls `library(BExTE)` fails outright there.
#' Each worker therefore falls back to loading the same source tree.
#'
#' @param cl A cluster from `parallel::makeCluster()`.
#' @param packages Additional package names to attach in each worker.
#'
#' @return `NULL`, invisibly.
#' @noRd
load_bexte_in_workers <- function(cl, packages = character()) {
  paths <- .libPaths()
  pkg_root <- find.package("BExTE")

  parallel::clusterExport(
    cl,
    varlist = c("paths", "pkg_root", "packages"),
    envir = environment()
  )

  parallel::clusterEvalQ(cl, {
    .libPaths(paths)
    if (requireNamespace("BExTE", quietly = TRUE)) {
      library(BExTE)
    } else {
      devtools::load_all(pkg_root, quiet = TRUE)
    }
    for (package in packages) {
      library(package, character.only = TRUE)
    }
    NULL
  })

  invisible(NULL)
}

#' Register the parallel backend simulations run their scenarios on
#'
#' doSNOW rather than doParallel: the simulation loops pass a per-scenario
#' progress callback as `.options.snow$progress`, and only doSNOW calls it as
#' each result comes back. doParallel reads `preschedule` and
#' `attachExportEnv` out of those options and warns that it is ignoring the
#' rest, which leaves the console progress bar and the progress file the
#' Shiny app reads frozen until a whole case study/method block is finished.
#'
#' @param cl A cluster from `parallel::makeCluster()`.
#'
#' @return The backend registration's return value, invisibly.
#' @noRd
register_parallel_backend <- function(cl) {
  invisible(doSNOW::registerDoSNOW(cl))
}
