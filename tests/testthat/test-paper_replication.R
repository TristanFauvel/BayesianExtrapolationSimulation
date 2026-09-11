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
