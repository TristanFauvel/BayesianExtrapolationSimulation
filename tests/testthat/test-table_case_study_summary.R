## Tables S1 and S7 are derived from the case study YAMLs, not from simulation
## output, so they can be generated before any run finishes.

config_dir <- function() {
  paste0(system.file("conf/case_studies", package = "RBExT"), "/")
}

test_that("table_target_sample_sizes lists one row per case study and one column per factor", {
  withr::with_tempdir({
    path <- table_target_sample_sizes(
      c("botox", "belimumab"), c(2, 4, 6), config_dir(), getwd()
    )

    expect_true(file.exists(path))
    contents <- paste(readLines(path), collapse = " ")
    ## The per-arm sizes the captions state.
    expect_match(contents, "117")
    expect_match(contents, "58")
    expect_match(contents, "140")
    expect_match(contents, "93")
  })
})

test_that("table_case_study_summary reports the source and target arms", {
  withr::with_tempdir({
    path <- table_case_study_summary(c("botox"), config_dir(), getwd())

    expect_true(file.exists(path))
    contents <- paste(readLines(path), collapse = " ")
    expect_match(contents, "Botox")
    expect_match(contents, "Placebo")
    ## Source arms: 235 control + 233 treatment.
    expect_match(contents, "468")
  })
})
