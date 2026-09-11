## Tables S1 and S7 are derived from the case study YAMLs, not from simulation
## output, so they can be generated before any run finishes.

config_dir <- function() {
  paste0(system.file("conf/case_studies", package = "RBExT"), "/")
}

## kableExtra's "striped" option wraps every cell of shaded rows in
## \cellcolor{gray!10}{...}; strip that so row assertions below don't have to
## depend on which rows happen to be shaded.
strip_shading <- function(contents) {
  gsub("\\\\cellcolor\\{gray!10\\}\\{([^}]*)\\}", "\\1", contents)
}

test_that("table_target_sample_sizes lists one row per case study, as the two-arm N_T total, one column per factor", {
  withr::with_tempdir({
    path <- table_target_sample_sizes(
      c("botox", "belimumab"), c(2, 4, 6), config_dir(), getwd()
    )

    expect_true(file.exists(path))
    contents <- strip_shading(paste(readLines(path), collapse = " "))

    ## Column header names the factor.
    expect_match(contents, "N\\_T (factor 2)", fixed = TRUE)

    ## N_T is the two-arm total (the paper's captions write "N_T/2 = 58" for
    ## the per-arm size), so each cell is 2 * floor(source_total / factor / 2).
    ## Botox: 2*117=234, 2*58=116, 2*39=78. Belimumab: 2*281=562, 2*140=280,
    ## 2*93=186. Note 2*58 = 116, not 117 - the per-arm value is floored
    ## before doubling, so the doubled total is the number actually
    ## simulated (two arms of 58), not half of (control+treatment)/factor.
    ##
    ## Match each case study's row as a unit, cells in column order, so a
    ## value landing in the wrong column - or getting halved - fails this.
    expect_match(contents, "Botox & 234 & 116 & 78", fixed = TRUE)
    expect_match(contents, "Belimumab & 562 & 280 & 186", fixed = TRUE)
  })
})

test_that("table_case_study_summary reports the source and target arms, not the stale total field", {
  withr::with_tempdir({
    path <- table_case_study_summary(c("botox", "aprepitant"), config_dir(), getwd())

    expect_true(file.exists(path))
    contents <- strip_shading(paste(readLines(path), collapse = " "))

    ## Column headers.
    expect_match(contents, "\\textbf{Source N}", fixed = TRUE)
    expect_match(contents, "\\textbf{Target N}", fixed = TRUE)

    ## Match each row as a unit, with Source N and Target N pinned to their
    ## column position (Endpoint/Summary measure/effect/SE cells wildcarded,
    ## since they aren't what this test is about) - so a value landing in
    ## the wrong column doesn't slip past a bare substring match.
    ##
    ## Botox source: 235 control + 233 treatment = 468. Target: 130 + 126 = 256.
    expect_match(contents, "Botox & Placebo & [^&]* & [^&]* & 468 & [^&]* & [^&]* & 256")

    ## Aprepitant source: 293 control + 280 treatment = 573 - this must come
    ## from control + treatment, NOT from the YAML's `source.total: 673`
    ## field, which is stale for aprepitant. Botox's own total (468) happens
    ## to agree with control + treatment, so it can't catch this on its own;
    ## aprepitant is the case where the two disagree.
    expect_match(contents, "Aprepitant & Ondansetron & [^&]* & [^&]* & 573 & [^&]* & [^&]* & 107")
    expect_false(grepl("673", contents, fixed = TRUE))
  })
})
