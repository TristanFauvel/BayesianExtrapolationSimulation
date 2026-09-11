## The Run tab's progress readout counts the scenario rows an environment has
## written so far. Two things make that harder than counting lines in
## results/<env>/results_frequentist.csv:
##
##  - a scenario row spans several physical lines, because rng_state and
##    parameters are written as quoted fields holding embedded newlines;
##  - relaunching an environment leaves the previous run's files on disk.
##    run_simulation_env() does empty results/<env>/ itself, but only once the
##    background process has loaded the package and parsed its configuration -
##    seconds after the launch button was pressed - and the concatenated file
##    it eventually writes is only produced at the very end of a pass, so the
##    per-case-study/method files are what actually grow while a run works.

## Mirrors what write.csv() produces for a results row: rng_state is a quoted
## field whose value spans several physical lines.
write_results_csv <- function(path, n_rows) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(
      drift = seq_len(n_rows),
      rng_state = rep("10403\n624\n1\n-1938302103", n_rows),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )
}

## count_result_rows() reads paths relative to the repository root, the way
## the app itself runs (see the comment at the top of helpers.R).
local_results_root <- function(env = parent.frame()) {
  root <- file.path(tempfile("bexte-results"))
  dir.create(root, recursive = TRUE)
  old <- setwd(root)
  withr::defer(setwd(old), envir = env)
  root
}

test_that("count_result_rows counts records, not the lines one record spans", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  write_results_csv("results/demo/results_frequentist.csv", 3)

  expect_identical(count_result_rows("demo"), 3L)
})

test_that("count_result_rows counts the per-method files a run writes as it goes", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  write_results_csv("results/demo/frequentist/botox/separate/results_frequentist.csv", 5)
  write_results_csv("results/demo/frequentist/botox/pooling/results_frequentist.csv", 4)

  expect_identical(count_result_rows("demo"), 9L)
})

test_that("count_result_rows does not count concatenated rows twice", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  write_results_csv("results/demo/frequentist/botox/separate/results_frequentist.csv", 5)
  write_results_csv("results/demo/frequentist/botox/pooling/results_frequentist.csv", 4)
  write_results_csv("results/demo/results_frequentist.csv", 9)

  expect_identical(count_result_rows("demo"), 9L)
})

test_that("count_result_rows ignores results left over from an earlier run", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  stale <- c(
    "results/demo/results_frequentist.csv",
    "results/demo/frequentist/botox/separate/results_frequentist.csv"
  )
  for (path in stale) {
    write_results_csv(path, 7)
  }
  launched <- Sys.time()
  for (path in stale) {
    Sys.setFileTime(path, launched - 60)
  }

  expect_identical(count_result_rows("demo", since = launched), 0L)
})

test_that("count_result_rows counts the rows written since the run was launched", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  stale <- "results/demo/results_frequentist.csv"
  fresh <- "results/demo/frequentist/botox/separate/results_frequentist.csv"
  write_results_csv(stale, 7)
  write_results_csv(fresh, 2)

  ## Both timestamps are set relative to `launched` rather than left to the
  ## clock: a file written microseconds after Sys.time() was read is not
  ## reliably stamped after it, which is a race in the test, not in the app -
  ## a real run takes seconds to load the package before it writes anything.
  launched <- Sys.time()
  Sys.setFileTime(stale, launched - 60)
  Sys.setFileTime(fresh, launched + 1)

  expect_identical(count_result_rows("demo", since = launched), 2L)
})

test_that("count_result_rows reports no rows for an environment that has never run", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  expect_identical(count_result_rows("demo"), 0L)
})

## A run reports its own progress to logs/<env>/progress.json (see
## run_progress_tracker() in R/run_progress.R), which is finer-grained than
## counting result rows: a method that runs in parallel writes all of its
## rows at once when it finishes. The Run tab prefers that file and falls
## back to counting rows when it is missing or belongs to an earlier run.

write_progress_json <- function(env, done, total) {
  dir.create(file.path("logs", env), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(
      done = done, total = total,
      case_study = "botox", method = "separate",
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    ),
    file.path("logs", env, "progress.json"),
    auto_unbox = TRUE
  )
}

test_that("read_run_progress reports the scenarios a run has simulated", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  write_progress_json("demo", done = 17, total = 108)

  expect_identical(read_run_progress("demo"), list(done = 17L, total = 108L))
})

test_that("read_run_progress ignores progress left over from an earlier run", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  write_progress_json("demo", done = 108, total = 108)
  launched <- Sys.time()
  Sys.setFileTime(file.path("logs", "demo", "progress.json"), launched - 60)

  expect_null(read_run_progress("demo", since = launched))
})

test_that("read_run_progress reports nothing for a run that has not written yet", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  expect_null(read_run_progress("demo"))
})

test_that("read_run_progress reports nothing rather than failing on an unreadable file", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  dir.create(file.path("logs", "demo"), recursive = TRUE, showWarnings = FALSE)
  writeLines('{"done": 17, "total"', file.path("logs", "demo", "progress.json"))

  expect_null(read_run_progress("demo"))
})

test_that("the app reads the progress a run's own tracker writes", {
  source(system.file("shiny_app/helpers.R", package = "BExTE"))
  local_results_root()

  tracker <- run_progress_tracker("demo", total = 108)
  tracker$starting("botox", "separate")
  tracker$tick(17)

  expect_identical(read_run_progress("demo"), list(done = 17L, total = 108L))
})
