#' Cache of replicate analyses
#'
#' @description
#' A binary endpoint reaches the analysis only through the number of responders
#' in each arm, so replicates that landed on the same counts have the same
#' posterior and the same reported results. The counts repeat often: there are
#' far fewer plausible pairs than there are replicates. They also do not depend
#' on the drift, which moves the sampling distribution of the counts rather than
#' the posterior any particular pair implies, so scenarios that differ only in
#' drift can share the work.
#'
#' The store lives in the package rather than on a model because a model is
#' built afresh for each scenario. It is keyed so that only analyses that would
#' return the same answer can meet: the scenario identity covers everything the
#' analysis reads apart from the observed data, and the observation covers the
#' generated row itself.
#'
#' @keywords internal
inference_cache_store <- new.env(parent = emptyenv())

#' Empty the analysis cache
#'
#' @description Called between runs that must not share results, and by tests.
#' @return `NULL`, invisibly.
#' @export
inference_cache_reset <- function() {
  rm(
    list = ls(envir = inference_cache_store, all.names = TRUE),
    envir = inference_cache_store
  )
  invisible(NULL)
}

#' Number of analyses held in the cache
#'
#' @return The number of distinct analyses stored.
#' @export
inference_cache_size <- function() {
  length(ls(envir = inference_cache_store, all.names = TRUE))
}

#' Value identity of an object
#'
#' @description
#' Drops the functions and environments an R6 object carries, so that two
#' separately built objects describing the same study hash alike. Hashing them
#' whole would key on the identity of each instance instead, and a model is
#' rebuilt for every scenario.
#'
#' @param x Object to reduce to its values.
#' @return The same structure with functions and environments removed.
#' @keywords internal
inference_cache_identity <- function(x) {
  if (is.function(x) || is.environment(x)) {
    return(NULL)
  }

  if (is.list(x)) {
    return(lapply(x, inference_cache_identity))
  }

  x
}

#' Key for one replicate analysis
#'
#' @param scope Scenario identity, or `NULL` when the model is not cacheable.
#' @param sample One generated replicate, as a single row data frame.
#' @return A key, or `NULL` when the analysis must not be cached.
#' @keywords internal
inference_cache_key <- function(scope, sample) {
  if (is.null(scope)) {
    return(NULL)
  }

  # as.list() on a data frame yields its columns, dropping the row name the
  # replicate index would otherwise contribute and make every key unique.
  rlang::hash(list(scope = scope, observation = as.list(sample)))
}

#' Read one analysis from the cache
#'
#' @param key Key returned by [inference_cache_key()].
#' @return The stored analysis, or `NULL`.
#' @keywords internal
inference_cache_get <- function(key) {
  if (is.null(key) || !exists(key, envir = inference_cache_store, inherits = FALSE)) {
    return(NULL)
  }

  get(key, envir = inference_cache_store, inherits = FALSE)
}

#' Store one analysis in the cache
#'
#' @param key Key returned by [inference_cache_key()].
#' @param value The analysis to store.
#' @return `NULL`, invisibly.
#' @keywords internal
inference_cache_set <- function(key, value) {
  if (!is.null(key)) {
    assign(key, value, envir = inference_cache_store)
  }

  invisible(NULL)
}
