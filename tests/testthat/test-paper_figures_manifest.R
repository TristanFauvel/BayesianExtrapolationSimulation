## The manifest is the only place that records which generator call produces a
## given paper figure. These tests pin the two things a reader cannot verify by
## eye: that the ids are complete and unique, and that every sample-size factor
## resolves to the per-arm size the published caption states.

test_that("the manifest covers every paper item exactly once", {
  ids <- paper_manifest_ids()

  expect_length(ids, 42)
  expect_length(unique(ids), 42)
  expect_true(all(c("1", "2", "3", "4") %in% ids))
  expect_true(all(paste0("S", 3:37) %in% ids))
  expect_true(all(c("TS1", "TS2", "TS7") %in% ids))
  ## Table S8 is out of scope; figure S8 is not.
  expect_false("TS8" %in% ids)
  expect_true("S8" %in% ids)
})

test_that("every entry is well formed and its generator is callable", {
  for (entry in paper_manifest()) {
    expect_true(is.character(entry$id) && nzchar(entry$id))
    expect_true(entry$kind %in% c("figure", "table"))
    expect_true(is.character(entry$caption) && nzchar(entry$caption))
    expect_true(entry$needs %in% c("frequentist", "configs", "run_artifact"))
    expect_true(is.function(entry$generator))
  }
})

test_that("every case study named in the manifest has a config", {
  for (entry in paper_manifest()) {
    if (is.na(entry$case_study)) next
    path <- system.file(
      file.path("conf/case_studies", paste0(entry$case_study, ".yml")),
      package = "BExTE"
    )
    expect_true(nzchar(path), info = entry$id)
  }
})

test_that("sample size factors resolve to the per-arm sizes the captions state", {
  config_dir <- paste0(system.file("conf/case_studies", package = "BExTE"), "/")

  expected <- list(
    list("botox", 2, 117), list("botox", 4, 58),
    list("belimumab", 4, 140), list("belimumab", 6, 93),
    list("dapagliflozin", 2, 66), list("dapagliflozin", 4, 33),
    list("mepolizumab", 4, 68), list("mepolizumab", 6, 45),
    list("teriflunomide", 4, 185), list("teriflunomide", 6, 123),
    list("aprepitant", 2, 143), list("aprepitant", 4, 71)
  )

  for (case in expected) {
    expect_equal(
      paper_sample_size_per_arm(case[[1]], case[[2]], config_dir),
      case[[3]],
      info = paste(case[[1]], "factor", case[[2]])
    )
  }
})

test_that("the headline figures name the slice their captions describe", {
  fig1 <- paper_manifest_entry("1")
  expect_equal(fig1$case_study, "botox")
  expect_equal(fig1$sample_size_factor, 4)
  expect_equal(fig1$metric, "success_proba")

  fig3 <- paper_manifest_entry("3")
  expect_equal(fig3$case_study, "botox")
  expect_equal(fig3$sample_size_factor, 2)
  expect_equal(fig3$metric, "mse")
})

test_that("paper_manifest_entry rejects an unknown id", {
  expect_error(paper_manifest_entry("S99"), "S99")
})
