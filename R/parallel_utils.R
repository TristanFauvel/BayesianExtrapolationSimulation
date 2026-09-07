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
