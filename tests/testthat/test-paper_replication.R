## Folding a selection down to the smallest simulation config that still
## reproduces it is the whole point of the page: the full `combined` env is
## HPC-scale and most of its grid is never plotted.

config_dir <- function() {
  paste0(system.file("conf/case_studies", package = "RBExT"), "/")
}

test_that("the four main figures need only botox at factors 2 and 4", {
  requirements <- paper_replication_requirements(c("1", "2", "3", "4"), config_dir())

  expect_equal(requirements$case_studies, "botox")
  expect_setequal(requirements$sample_size_factors, c(2, 4))
})

test_that("the whole manifest needs all six case studies and factors 2, 4 and 6", {
  requirements <- paper_replication_requirements(paper_manifest_ids(), config_dir())

  expect_setequal(
    requirements$case_studies,
    c("botox", "belimumab", "dapagliflozin", "mepolizumab", "teriflunomide", "aprepitant")
  )
  expect_setequal(requirements$sample_size_factors, c(2, 4, 6))
  ## Factor 1 is never plotted in the paper.
  expect_false(1 %in% requirements$sample_size_factors)
})

test_that("fidelity settings are fixed regardless of how little is selected", {
  requirements <- paper_replication_requirements("1", config_dir())

  ## forest_plot() picks the three principal scenarios by nearest grid point,
  ## so a coarser drift grid silently plots different drift values.
  expect_equal(requirements$ndrift, 30)
  expect_equal(requirements$n_replicates, 10000)
  expect_equal(requirements$denominator_change_factor, 1)
  expect_equal(requirements$target_to_source_std_ratio_range, 1)
})

test_that("every method is requested, because the forest plots compare all of them", {
  requirements <- paper_replication_requirements("1", config_dir())

  expect_length(requirements$methods, 11)
  expect_true("separate" %in% requirements$methods)
})

test_that("coverage reports a case study missing from the results frame", {
  df <- readRDS(testthat::test_path("fixtures", "forest_plot_freq.rds"))

  coverage <- paper_replication_coverage(df, c("1", "S16"), config_dir())

  ## The fixture is botox at 234 per arm, so neither entry is covered: figure 1
  ## wants 58 per arm and S16 wants belimumab entirely.
  expect_false(coverage$covered[coverage$id == "S16"])
  expect_match(coverage$reason[coverage$id == "S16"], "belimumab")
  expect_false(coverage$covered[coverage$id == "1"])
  expect_match(coverage$reason[coverage$id == "1"], "58")
})

test_that("coverage accepts a slice that is present", {
  df <- readRDS(testthat::test_path("fixtures", "forest_plot_freq.rds"))
  ## Relabel the fixture as the factor-4 botox slice figure 1 asks for.
  df$target_sample_size_per_arm <- 58

  coverage <- paper_replication_coverage(df, "1", config_dir())

  expect_true(coverage$covered[coverage$id == "1"])
})

test_that("export_paper_outputs records a row per entry and writes a manifest", {
  df <- readRDS(testthat::test_path("fixtures", "forest_plot_freq.rds"))
  df$target_sample_size_per_arm <- 58

  withr::with_tempdir({
    results_dir <- file.path(getwd(), "results")
    dir.create(results_dir)
    readr::write_csv(df, file.path(results_dir, "results_frequentist.csv"))

    figures_dir <- file.path(getwd(), "figures", "")
    tables_dir <- file.path(getwd(), "tables")

    status <- export_paper_outputs(
      results_dir = results_dir,
      figures_dir = figures_dir,
      tables_dir = tables_dir,
      ids = c("1", "TS1"),
      case_studies_config_dir = config_dir()
    )

    expect_equal(nrow(status), 2)
    expect_true(all(c("id", "status", "outputs") %in% names(status)))
    expect_true(file.exists(file.path(tables_dir, "manifest.csv")))
    expect_true(file.exists(file.path(tables_dir, "README.md")))

    ## The manifest recording two rows is not proof anything was actually
    ## generated: without the plot globals sourced into .GlobalEnv, entry "1"'s
    ## generator (forest_plot()) would abort with "object 'font' not found" (or
    ## similar) and be recorded as "failed" while TS1 alone still produces a
    ## manifest with the expected shape. So assert directly that figure 1 wrote
    ## a real figure file under figures_dir.
    expect_equal(status$status[status$id == "1"], "ok")
    figure_files <- list.files(figures_dir, recursive = TRUE, full.names = TRUE)
    expect_true(any(grepl("\\.(pdf|png)$", figure_files)))
  })
})

test_that("export_paper_outputs marks an entry failed rather than aborting the batch", {
  df <- readRDS(testthat::test_path("fixtures", "forest_plot_freq.rds"))

  withr::with_tempdir({
    results_dir <- file.path(getwd(), "results")
    dir.create(results_dir)
    readr::write_csv(df, file.path(results_dir, "results_frequentist.csv"))

    status <- export_paper_outputs(
      results_dir = results_dir,
      figures_dir = file.path(getwd(), "figures", ""),
      tables_dir = file.path(getwd(), "tables"),
      ## S16 wants belimumab, which the botox fixture cannot supply.
      ids = c("TS1", "S16"),
      case_studies_config_dir = config_dir()
    )

    expect_equal(status$status[status$id == "TS1"], "ok")
    expect_equal(status$status[status$id == "S16"], "failed")
    expect_true(nzchar(status$message[status$id == "S16"]))
  })
})

test_that("export_paper_outputs restores the caller's .GlobalEnv and ggplot theme", {
  df <- readRDS(testthat::test_path("fixtures", "forest_plot_freq.rds"))
  df$target_sample_size_per_arm <- 58

  ## Guard against a previous run's globals still being set (which would
  ## itself be evidence of the leak this test exists to catch), so the test
  ## reflects a clean caller session regardless of test order.
  if (exists("textwidth", envir = .GlobalEnv, inherits = FALSE)) {
    rm("textwidth", envir = .GlobalEnv)
  }
  had_font_before <- exists("font", envir = .GlobalEnv, inherits = FALSE)
  previous_font <- if (had_font_before) get("font", envir = .GlobalEnv) else NULL
  assign("font", "SENTINEL", envir = .GlobalEnv)
  previous_theme <- ggplot2::theme_get()

  on.exit({
    if (had_font_before) {
      assign("font", previous_font, envir = .GlobalEnv)
    } else if (exists("font", envir = .GlobalEnv, inherits = FALSE)) {
      rm("font", envir = .GlobalEnv)
    }
    ggplot2::theme_set(previous_theme)
  }, add = TRUE)

  withr::with_tempdir({
    results_dir <- file.path(getwd(), "results")
    dir.create(results_dir)
    readr::write_csv(df, file.path(results_dir, "results_frequentist.csv"))

    export_paper_outputs(
      results_dir = results_dir,
      figures_dir = file.path(getwd(), "figures", ""),
      tables_dir = file.path(getwd(), "tables"),
      ids = c("1", "TS1"),
      case_studies_config_dir = config_dir()
    )
  })

  ## "font" is a name the conf files also define - the caller's sentinel
  ## value must come back exactly as they left it, not conf/plots_config.R's
  ## "CMU Serif".
  expect_equal(get("font", envir = .GlobalEnv), "SENTINEL")
  ## "textwidth" did not exist before the call, but conf/plots_config.R
  ## defines it - it must not linger in .GlobalEnv afterwards.
  expect_false(exists("textwidth", envir = .GlobalEnv, inherits = FALSE))
  ## conf/plots_config.R calls ggplot2::theme_set(); that must not leak either.
  expect_identical(ggplot2::theme_get(), previous_theme)
})

test_that("export_paper_outputs still attributes outputs on a re-export into the same directories", {
  df <- readRDS(testthat::test_path("fixtures", "forest_plot_freq.rds"))
  df$target_sample_size_per_arm <- 58

  withr::with_tempdir({
    results_dir <- file.path(getwd(), "results")
    dir.create(results_dir)
    readr::write_csv(df, file.path(results_dir, "results_frequentist.csv"))

    figures_dir <- file.path(getwd(), "figures", "")
    tables_dir <- file.path(getwd(), "tables")

    first <- export_paper_outputs(
      results_dir = results_dir, figures_dir = figures_dir, tables_dir = tables_dir,
      ids = c("1", "TS1"), case_studies_config_dir = config_dir()
    )
    expect_true(nzchar(first$outputs[first$id == "1"]))

    ## Re-export into the same directories: remake_figures is TRUE, so this
    ## overwrites the exact same paths the first run wrote. A before/after
    ## path-list diff would find every path already present in "before" (it
    ## was written by the first run) and report an empty outputs column here -
    ## the failure mode this test exists to catch.
    second <- export_paper_outputs(
      results_dir = results_dir, figures_dir = figures_dir, tables_dir = tables_dir,
      ids = c("1", "TS1"), case_studies_config_dir = config_dir()
    )
    expect_true(nzchar(second$outputs[second$id == "1"]))
  })
})
