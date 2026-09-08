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
