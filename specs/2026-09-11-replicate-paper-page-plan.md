# Replicate Paper Page Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a "Replicate paper" page to the BExTE Shiny app that records which generator call produces each paper figure and table, derives and runs the minimal simulation config a chosen subset needs, and exports that subset to a dedicated folder with a manifest.

**Architecture:** A declarative manifest (`R/paper_figures_manifest.R`) maps each paper item to a direct call on an existing leaf plot/table generator — the leaf functions already take the scenario coordinates explicitly, so no plotting logic is reimplemented. `R/paper_replication.R` folds a selection into a minimal `scenarios_config`, checks an existing results directory for coverage, and runs the generators. `inst/shiny_app/modules/mod_replicate.R` is a thin three-step UI over those functions, reusing the Run page's background launcher.

**Tech Stack:** R 3.5+, R6, Shiny + bslib, ggplot2, testthat 3e, yaml, callr, renv.

**Spec:** `specs/2026-09-11-replicate-paper-page-design.md` — read it before starting. It carries the figure→generator table, the sample-size resolution table, and the author's caption corrections.

## Global Constraints

- **Never run `devtools::document()` and commit the result wholesale.** `DESCRIPTION` pins `RoxygenNote: 7.3.2` but the installed roxygen2 is 8.1.0, so a full regeneration rewrites all 42 `man/*.Rd` files and bumps the pin. When a task adds a roxygen block, run `devtools::document()`, then `git add` **only** the newly created `man/*.Rd` files and `git checkout -- DESCRIPTION man/` for everything else. Verify with `git status --short` before committing.
- **Preserve CRLF line endings.** Several files in this repo use CRLF. Scripted edits that rewrite a whole file silently normalise them and turn a 3-line change into a whole-file diff. After every edit, check `git diff --stat <file>` shows only the lines you meant to touch.
- **Report divergent or undefined quantities as `NA`/`Inf`, never a finite surrogate.** This is a house rule. In this plan it applies to `scale_by_separate()`: a zero or missing denominator drops the row with a warning rather than substituting a value.
- `tests/testthat/test-simulation_analysis.R:61` **already fails on a clean checkout.** It is unrelated to this work. Do not attempt to fix it; do not treat it as a regression.
- Run tests with `Rscript -e "devtools::test(stop_on_failure = TRUE)"`; a single file with `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-NAME.R")'`.
- All app code addresses its output with paths relative to the **workspace** — `./results/<env>/`, `./logs/<env>/`, `./user_configs/<env>/`. `run_bexte_app()` picks it (`bexte_workspace()`: the checkout when there is one, `tools::R_user_dir("BExTE", "data")` otherwise), records it as `options(bexte.workspace=)`, and `app.R` restores it. Write the new page's outputs with workspace-relative paths, the same way, never an absolute path or one built from `system.file()`.
- **Work happens directly on `main`**, based on 3faa2f1 ("Make the installed package work outside a source checkout"), which is where the workspace contract above comes from. Every `git add` in this plan names its paths explicitly; never `git add -A`, and never commit an unrelated file you find modified. If a rebase or merge is needed, stop and ask.
- `renv` reports the project as out-of-sync. That is pre-existing; ignore the warning on every `Rscript` call.

## File Structure

| File | Responsibility |
|---|---|
| `R/plot_methods_comparison_forestplot.R` (modify) | Add `scale_by_separate()` and the `relative_to_separate` / `reference_line` arguments |
| `R/paper_figures_manifest.R` (create) | The 42 manifest entries and the accessors over them |
| `R/paper_replication.R` (create) | Requirement folding, coverage check, `export_paper_outputs()` |
| `R/table_case_study_summary.R` (create) | Table S1 and Table S7 generators |
| `inst/shiny_app/helpers.R` (modify) | Host the shared `launch_simulation_run()` and `prepare_plot_globals_for_env()` |
| `inst/shiny_app/modules/mod_run.R` (modify) | Call the extracted launcher |
| `inst/shiny_app/modules/mod_analyze.R` (modify) | Drop its local copy of `prepare_plot_globals_for_env()` |
| `inst/shiny_app/modules/mod_replicate.R` (create) | The new page |
| `inst/shiny_app/app.R` (modify) | Source and register the page |

---

### Task 1: Ratio-to-separate on the forest plot

**Files:**
- Modify: `R/plot_methods_comparison_forestplot.R` (`forest_subplot()` at :86, `forest_plot()` at :427)
- Test: `tests/testthat/test-forest_plot_relative.R` (create)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  - `scale_by_separate(df, metric_columns)` → data frame. `df` is a frequentist results frame; `metric_columns` is a character vector of columns to divide. Returns `df` with those columns divided by the `separate` method's value in the matching scenario, original column order preserved, rows with a zero/missing denominator dropped.
  - `forest_plot(results_freq_df, x_metric, panels = TRUE, palette = NULL, relative_to_separate = FALSE)` — the new fifth argument.
  - `forest_subplot(..., reference_line = NULL)` — new trailing argument.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-forest_plot_relative.R`:

```r
## Figures S9, S14, S17, S27, S32 and S37 show each method's operating
## characteristic as a ratio to the separate (no-borrowing) analysis in the
## same scenario. scale_by_separate() is that division; it matches on the full
## scenario key rather than collapsing across drift, and it refuses to invent a
## finite value when the denominator is zero or missing.

relative_fixture <- function() {
  readRDS(testthat::test_path("fixtures", "forest_plot_freq.rds"))
}

test_that("scale_by_separate divides by the separate row in the same scenario", {
  df <- relative_fixture()

  out <- scale_by_separate(df, "success_proba")

  expect_identical(names(out), names(df))
  ## Every separate row becomes exactly 1.
  separate_rows <- out[out$method == "separate", ]
  expect_equal(separate_rows$success_proba, rep(1, nrow(separate_rows)))

  ## A pooling row equals its own scenario's ratio, not a cross-drift average.
  one_drift <- df$drift[df$method == "pooling"][1]
  numerator <- df$success_proba[df$method == "pooling" & df$drift == one_drift]
  denominator <- df$success_proba[df$method == "separate" & df$drift == one_drift]
  expect_equal(
    out$success_proba[out$method == "pooling" & out$drift == one_drift],
    numerator / denominator
  )
})

test_that("scale_by_separate scales the confidence bounds by the same denominator", {
  df <- relative_fixture()

  out <- scale_by_separate(
    df,
    c("success_proba", "conf_int_success_proba_lower", "conf_int_success_proba_upper")
  )

  one_drift <- df$drift[df$method == "pooling"][1]
  denominator <- df$success_proba[df$method == "separate" & df$drift == one_drift]
  expect_equal(
    out$conf_int_success_proba_lower[out$method == "pooling" & out$drift == one_drift],
    df$conf_int_success_proba_lower[df$method == "pooling" & df$drift == one_drift] / denominator
  )
})

test_that("scale_by_separate drops rows whose denominator is zero, never returning Inf", {
  df <- relative_fixture()
  zero_drift <- df$drift[df$method == "separate"][1]
  df$success_proba[df$method == "separate" & df$drift == zero_drift] <- 0

  expect_warning(out <- scale_by_separate(df, "success_proba"), "zero or missing")

  expect_false(any(is.infinite(out$success_proba)))
  expect_equal(sum(out$drift == zero_drift), 0)
})

test_that("scale_by_separate errors when there is no separate analysis to divide by", {
  df <- relative_fixture()
  df <- df[df$method != "separate", ]

  expect_error(scale_by_separate(df, "success_proba"), "separate")
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-forest_plot_relative.R")'`
Expected: FAIL — `could not find function "scale_by_separate"`.

- [ ] **Step 3: Implement `scale_by_separate()`**

Add to `R/plot_methods_comparison_forestplot.R`, immediately above `forest_subplot()`:

```r
#' Express a metric as a ratio to the separate analysis
#'
#' @description Divides each row's metric by the value the `separate` (no
#'   borrowing) method takes in the same scenario. The scenario key is the full
#'   set of coordinates that identify a simulated configuration, so the ratio
#'   never collapses across drift or sample size.
#'
#'   A zero or missing denominator makes the ratio undefined. Those rows are
#'   dropped with a warning rather than reported as `Inf` or as some finite
#'   stand-in.
#'
#' @param df A frequentist results frame containing `separate` method rows.
#' @param metric_columns Character vector of columns to divide - typically the
#'   metric and its two confidence bounds.
#'
#' @return `df` with `metric_columns` divided by the separate analysis's value,
#'   in the original column order.
#'
#' @export
scale_by_separate <- function(df, metric_columns) {
  key <- c(
    "case_study", "target_sample_size_per_arm", "drift",
    "target_to_source_std_ratio", "source_denominator_change_factor"
  )
  missing_columns <- setdiff(c(key, metric_columns), names(df))
  if (length(missing_columns) > 0) {
    stop(
      "scale_by_separate() needs these columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  baseline <- df[df$method == "separate", c(key, metric_columns[1]), drop = FALSE]
  if (nrow(baseline) == 0) {
    stop("No `separate` method rows to use as the denominator.")
  }
  names(baseline)[names(baseline) == metric_columns[1]] <- ".separate_value"
  baseline <- unique(baseline)

  original_columns <- names(df)
  out <- merge(df, baseline, by = key, all.x = TRUE, sort = FALSE)

  usable <- !is.na(out$.separate_value) & out$.separate_value != 0
  if (any(!usable)) {
    warning(
      sum(!usable),
      " row(s) dropped: the separate analysis's value is zero or missing, ",
      "so the ratio is undefined."
    )
    out <- out[usable, , drop = FALSE]
  }

  for (column in metric_columns) {
    out[[column]] <- out[[column]] / out$.separate_value
  }

  out[, original_columns, drop = FALSE]
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-forest_plot_relative.R")'`
Expected: PASS, 4 tests.

- [ ] **Step 5: Add the `reference_line` argument to `forest_subplot()`**

In `R/plot_methods_comparison_forestplot.R`, change the signature at :86 from
`palette = NULL) {` to `palette = NULL,\n                           reference_line = NULL) {`,
add the roxygen line `#' @param reference_line x position of a dotted vertical reference line, or NULL for none.`
to its block, and insert immediately after `refs <- forest_reference_colours(palette)`:

```r
  ## A ratio plot needs its own reference at 1; the metric-name-driven
  ## reference lines further down do not cover it.
  extra_reference <- reference_line
```

then insert just before the final `return(plt)` of `forest_subplot()`:

```r
  if (!is.null(extra_reference)) {
    plt <- plt + geom_vline(
      xintercept = extra_reference,
      linetype = "dotted",
      color = refs$ink
    )
  }
```

- [ ] **Step 6: Add the `relative_to_separate` argument to `forest_plot()`**

Change the signature at :427 to:

```r
forest_plot <- function(results_freq_df, x_metric, panels = TRUE, palette = NULL,
                        relative_to_separate = FALSE) {
```

Add to its roxygen block:

```r
#' @param relative_to_separate When TRUE, divide the metric and its confidence
#'   bounds by the separate analysis's value in the same scenario, label the
#'   axis accordingly, and draw a reference line at 1. Used by supplementary
#'   figures S9, S14, S17, S27, S32 and S37.
```

Insert immediately after the `x_metric_label <- metric$label` line:

```r
  if (relative_to_separate) {
    results_freq_df <- scale_by_separate(
      results_freq_df,
      c(metric$name, as.character(x_metric_uncertainty_lower),
        as.character(x_metric_uncertainty_upper))
    )
    x_metric_label <- paste0(x_metric_label, " relative to a separate analysis")
  }
```

In the `filename <- paste0(...)` block, insert immediately after it:

```r
  if (relative_to_separate) {
    filename <- paste0(filename, "_relative_to_separate")
  }
```

Pass the reference through to all three `forest_subplot()` calls by adding
`reference_line = if (relative_to_separate) 1 else NULL,` immediately before
each call's `palette = palette` argument.

- [ ] **Step 7: Test that the plot path works end to end and leaves the default untouched**

Append to `tests/testthat/test-forest_plot_relative.R`:

```r
setup_relative_plot_globals <- function(figures_dir) {
  source(system.file("conf/plots_config.R", package = "BExTE"))
  source(system.file("conf/methods_plots_config.R", package = "BExTE"))
  source(system.file("conf/metrics_config.R", package = "BExTE"))
  methods_env <- new.env()
  source(system.file("conf/full/methods_config.R", package = "BExTE"), local = methods_env)
  assign("methods_dict", methods_env$methods_dict, envir = .GlobalEnv)
  assign("figures_dir", figures_dir, envir = .GlobalEnv)
  assign("remake_figures", TRUE, envir = .GlobalEnv)
  invisible(NULL)
}

test_that("forest_plot writes a distinctly named file when relative_to_separate is TRUE", {
  df <- relative_fixture()
  withr::with_tempdir({
    setup_relative_plot_globals(file.path(getwd(), ""))

    forest_plot(df, "success_proba", relative_to_separate = TRUE)

    written <- list.files(getwd(), pattern = "\\.png$", recursive = TRUE)
    expect_true(any(grepl("relative_to_separate", written)))
  })
})

test_that("forest_plot's default output is unchanged by the new argument", {
  df <- relative_fixture()
  withr::with_tempdir({
    setup_relative_plot_globals(file.path(getwd(), ""))

    forest_plot(df, "success_proba")

    written <- list.files(getwd(), pattern = "\\.png$", recursive = TRUE)
    expect_length(written, 1)
    expect_false(grepl("relative_to_separate", written))
  })
})
```

- [ ] **Step 8: Run the full test file**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-forest_plot_relative.R")'`
Expected: PASS, 6 tests.

- [ ] **Step 9: Confirm no existing forest-plot test regressed**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-forest_plot_theming.R")'`
Expected: PASS, same count as before the change.

- [ ] **Step 10: Regenerate only the new docs and commit**

```bash
Rscript -e 'devtools::document()'
git checkout -- DESCRIPTION
git status --short          # expect: man/scale_by_separate.Rd new, man/forest_plot.Rd modified
git checkout -- $(git diff --name-only man/ | grep -v scale_by_separate | grep -v forest_plot)
git add R/plot_methods_comparison_forestplot.R tests/testthat/test-forest_plot_relative.R man/scale_by_separate.Rd man/forest_plot.Rd
git commit -m "Express forest plot metrics as a ratio to the separate analysis

Supplementary figures S9, S14, S17, S27, S32 and S37 report each method
relative to the no-borrowing analysis. scale_by_separate() matches on the
full scenario key, and drops rows with a zero or missing denominator rather
than reporting a ratio that does not exist.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: The paper figure manifest

**Files:**
- Create: `R/paper_figures_manifest.R`
- Test: `tests/testthat/test-paper_figures_manifest.R`

**Interfaces:**
- Consumes: `scale_by_separate()` and `forest_plot(..., relative_to_separate=)` from Task 1.
- Produces:
  - `paper_manifest()` → list of 42 entries. Each entry is a list with `id` (character), `kind` (`"figure"` or `"table"`), `caption` (character), `case_study` (character or `NA`), `sample_size_factor` (numeric or `NA`), `metric` (character or `NA`), `needs` (`"frequentist"`, `"configs"` or `"run_artifact"`), and `generator` (a `function(ctx)`).
  - `paper_manifest_ids()` → character vector of the 42 ids in paper order.
  - `paper_manifest_entry(id)` → one entry, erroring on an unknown id.
  - `paper_sample_size_per_arm(case_study, factor, case_studies_config_dir)` → integer per-arm size.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-paper_figures_manifest.R`:

```r
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
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-paper_figures_manifest.R")'`
Expected: FAIL — `could not find function "paper_manifest_ids"`.

- [ ] **Step 3: Implement the sample-size resolver and the manifest scaffolding**

Create `R/paper_figures_manifest.R` starting with:

```r
## Which generator call produces each figure and table in the paper.
##
## The leaf plot and table functions already take the scenario coordinates
## explicitly, so every entry here is a direct call rather than a
## reimplementation. The loop functions in inst/scripts/plots.R emit every
## combination in the results frame under generated filenames; this manifest is
## what lets a caller ask for "Figure 3" instead.
##
## Captions are quoted from the manuscript, with two author-confirmed
## corrections recorded in specs/2026-09-11-replicate-paper-page-design.md:
## figure S4's "lambda = 20" is a typo for lambda = 0.5, and figure S5 is the
## partially consistent treatment effect.

#' Resolve a sample size factor to a target sample size per arm
#'
#' @description Mirrors the arithmetic the simulation itself uses
#'   (R/simulation_scenarios.R): the total target sample size is the source
#'   study's arm sizes summed and divided by the factor, and the per-arm size
#'   is half of that, floored. Note this uses `control + treatment` rather than
#'   the `total:` field, which is stale for aprepitant.
#'
#' @param case_study Case study name.
#' @param factor Sample size factor.
#' @param case_studies_config_dir Directory holding the case study YAMLs,
#'   trailing slash included.
#'
#' @return The target sample size per arm, as an integer.
#'
#' @export
paper_sample_size_per_arm <- function(case_study, factor, case_studies_config_dir) {
  config <- yaml::read_yaml(file.path(case_studies_config_dir, paste0(case_study, ".yml")))
  source_total <- config$source$control + config$source$treatment
  floor((source_total / factor) / 2)
}
```

- [ ] **Step 4: Implement the entry constructors**

Append to `R/paper_figures_manifest.R`:

```r
## Internal constructors. Each returns one manifest entry; `ctx` is the list
## export_paper_outputs() builds - see R/paper_replication.R - carrying `df`
## (the slice already filtered to this entry's case study, sample size,
## denominator factor and std ratio), `case_studies_config_dir` and
## `results_dir`.

manifest_forest <- function(id, caption, case_study, factor, metric,
                            relative = FALSE) {
  list(
    id = id, kind = "figure", caption = caption,
    case_study = case_study, sample_size_factor = factor, metric = metric,
    needs = "frequentist",
    generator = function(ctx) {
      forest_plot(ctx$df, metric, relative_to_separate = relative)
    }
  )
}

manifest_vs_tie <- function(id, caption, case_study, factor, metric,
                            treatment_effect) {
  list(
    id = id, kind = "figure", caption = caption,
    case_study = case_study, sample_size_factor = factor, metric = metric,
    needs = "frequentist",
    generator = function(ctx) {
      operating_characteristic_vs_tie(
        ctx$df,
        case_study = case_study,
        target_sample_size_per_arm = ctx$target_sample_size_per_arm,
        treatment_effect = treatment_effect,
        operating_characteristic = frequentist_metrics[[metric]],
        source_denominator_change_factor = 1,
        target_to_source_std_ratio = 1
      )
    }
  )
}
```

- [ ] **Step 5: Implement the forest-plot and vs-TIE entries**

Append the entry list. The `relative = TRUE` entries are S9, S14, S17, S27, S32, S37.

```r
paper_manifest_figures_forest <- function() {
  list(
    manifest_forest("1", "Probability of success for the three principal treatment-effect scenarios in the Botox case study (N_T/2 = 58).", "botox", 4, "success_proba"),
    manifest_forest("3", "Mean squared error (MSE) for the three principal treatment-effect scenarios in the Botox case study (N_T/2 = 117).", "botox", 2, "mse"),
    manifest_forest("S6", "Moment-based ESS across methods for the three principal treatment-effect scenarios in the Botox case study (N_T/2 = 117).", "botox", 2, "ess_moment"),
    manifest_forest("S7", "Bias and associated 95% confidence intervals for the three principal treatment-effect scenarios in the Botox case study (N_T/2 = 117).", "botox", 2, "bias"),
    manifest_forest("S9", "Type I error and power relative to a separate analysis for the three principal treatment-effect scenarios in the Botox case study (N_T/2 = 58).", "botox", 4, "success_proba", relative = TRUE),
    manifest_forest("S11", "Coverage probability of the 95% credible interval for the three principal treatment-effect scenarios in the Dapagliflozin case study (N_T/2 = 66).", "dapagliflozin", 2, "coverage"),
    manifest_forest("S12", "MSE for the three principal treatment-effect scenarios in the Dapagliflozin case study (N_T/2 = 66).", "dapagliflozin", 2, "mse"),
    manifest_forest("S13", "Probability of study success for the three principal treatment-effect scenarios in the Dapagliflozin case study (N_T/2 = 66).", "dapagliflozin", 2, "success_proba"),
    manifest_forest("S14", "Probability of study success relative to a separate analysis for the three principal treatment-effect scenarios in the Dapagliflozin case study (N_T/2 = 66).", "dapagliflozin", 2, "success_proba", relative = TRUE),
    manifest_forest("S16", "Probability of study success for the three principal treatment-effect scenarios in the Belimumab case study (N_T/2 = 140).", "belimumab", 4, "success_proba"),
    manifest_forest("S17", "Probability of study success relative to a separate analysis for the three principal treatment-effect scenarios in the Belimumab case study (N_T/2 = 140).", "belimumab", 4, "success_proba", relative = TRUE),
    manifest_forest("S18", "MSE for the three principal treatment-effect scenarios in the Belimumab case study (N_T/2 = 140).", "belimumab", 4, "mse"),
    manifest_forest("S19", "Precision, measured by the mean half-width of the 95% credible interval, for the three principal treatment-effect scenarios in the Belimumab case study, with 140 participants per arm.", "belimumab", 4, "precision"),
    manifest_forest("S21", "Coverage probability of the 95% credible interval for the three principal treatment-effect scenarios in the Belimumab case study, with 140 participants per arm.", "belimumab", 4, "coverage"),
    manifest_forest("S24", "MSE for the three principal treatment-effect scenarios in the Mepolizumab case study (N_T/2 = 68).", "mepolizumab", 4, "mse"),
    manifest_forest("S25", "Empirical coverage probability for the three principal treatment-effect scenarios in the Mepolizumab case study (N_T/2 = 68).", "mepolizumab", 4, "coverage"),
    manifest_forest("S26", "Probability of study success for the three principal treatment-effect scenarios in the Mepolizumab case study (N_T/2 = 45).", "mepolizumab", 6, "success_proba"),
    manifest_forest("S27", "Probability of study success relative to a separate analysis for the three principal treatment-effect scenarios in the Mepolizumab case study (N_T/2 = 68).", "mepolizumab", 4, "success_proba", relative = TRUE),
    manifest_forest("S30", "MSE for the three principal treatment-effect scenarios in the Teriflunomide case study (N_T/2 = 185).", "teriflunomide", 4, "mse"),
    manifest_forest("S31", "Probability of study success for the three principal treatment-effect scenarios in the Teriflunomide case study (N_T/2 = 123).", "teriflunomide", 6, "success_proba"),
    manifest_forest("S32", "Probability of study success relative to a separate analysis for the three principal treatment-effect scenarios in the Teriflunomide case study (N_T/2 = 123).", "teriflunomide", 6, "success_proba", relative = TRUE),
    manifest_forest("S34", "MSE for the three principal treatment-effect scenarios in the Aprepitant case study, with 143 participants per arm.", "aprepitant", 2, "mse"),
    manifest_forest("S35", "Empirical coverage of the 95% credible interval for the three principal treatment-effect scenarios in the Aprepitant case study, with 143 participants per arm.", "aprepitant", 2, "coverage"),
    manifest_forest("S36", "Probability of study success for the three principal treatment-effect scenarios in the Aprepitant case study (N_T/2 = 143).", "aprepitant", 2, "success_proba"),
    manifest_forest("S37", "Probability of study success relative to a separate analysis for the three principal treatment-effect scenarios in the Aprepitant case study (N_T/2 = 143).", "aprepitant", 2, "success_proba", relative = TRUE)
  )
}

paper_manifest_figures_vs_tie <- function() {
  list(
    manifest_vs_tie("2", "Probability of success versus type I error rate in the Botox case study, with 58 participants per arm and a partially consistent treatment effect.", "botox", 4, "success_proba", "partially_consistent"),
    manifest_vs_tie("4", "MSE versus type I error rate in the Botox case study, with 117 participants per arm and a partially consistent treatment effect.", "botox", 2, "mse", "partially_consistent"),
    manifest_vs_tie("S8", "Coverage of the 95% interval versus type I error rate in the Botox case study, with 117 participants per arm, no treatment effect, and a target-to-source standard-deviation ratio of 1.", "botox", 2, "coverage", "no_effect"),
    manifest_vs_tie("S10", "MSE versus type I error rate in the Dapagliflozin case study, with 33 participants per arm and a partially consistent treatment effect.", "dapagliflozin", 4, "mse", "partially_consistent"),
    manifest_vs_tie("S15", "MSE versus type I error rate in the Belimumab case study, with 140 participants per arm and a partially consistent treatment effect.", "belimumab", 4, "mse", "partially_consistent"),
    manifest_vs_tie("S22", "MSE versus type I error rate in the Mepolizumab case study (N_T/2 = 68), with a partially consistent treatment effect.", "mepolizumab", 4, "mse", "partially_consistent"),
    manifest_vs_tie("S23", "Coverage of the 95% interval versus type I error rate in the Mepolizumab case study, with 68 participants per arm and no treatment effect.", "mepolizumab", 4, "coverage", "no_effect"),
    manifest_vs_tie("S28", "MSE versus type I error rate in the Teriflunomide case study (N_T/2 = 123), with a partially consistent treatment effect.", "teriflunomide", 6, "mse", "partially_consistent"),
    manifest_vs_tie("S29", "Coverage of the 95% interval versus type I error rate in the Teriflunomide case study, with 123 participants per arm and no treatment effect.", "teriflunomide", 6, "coverage", "no_effect"),
    manifest_vs_tie("S33", "MSE versus type I error rate in the Aprepitant case study (N_T/2 = 71), with a partially consistent treatment effect.", "aprepitant", 4, "mse", "partially_consistent")
  )
}
```

- [ ] **Step 6: Implement the four one-off figure entries**

Append:

```r
## S3, S4, S5 and S20 each have their own generator rather than sharing one of
## the two shapes above.
paper_manifest_figures_special <- function() {
  list(
    list(
      id = "S3", kind = "figure",
      caption = "Probability of success versus treatment-effect drift for the Conditional Power Prior (gamma = 0.25) in the Belimumab case study, with 93 participants per arm; includes comparisons with t-tests at nominal and matched type I error rates.",
      case_study = "belimumab", sample_size_factor = 6, metric = "success_proba",
      needs = "frequentist",
      generator = function(ctx) {
        plot_success_proba_vs_drift(
          metric = "success_proba",
          results_metrics_df = ctx$df,
          theta_0 = ctx$theta_0,
          case_study = "belimumab",
          method = "conditional_power_prior",
          target_sample_size_per_arm = ctx$target_sample_size_per_arm,
          target_to_source_std_ratio = 1,
          source_denominator_change_factor = 1,
          parameters_combinations = data.frame(power_parameter = 0.25),
          xvars = xvars,
          join_points = TRUE,
          baseline_success_proba = c("at_equivalent_TIE", "at_nominal_TIE")
        )
      }
    ),
    list(
      id = "S4", kind = "figure",
      ## The manuscript prints "lambda = 20"; the author confirmed this is a
      ## typo for lambda = 0.5, which is what the configs actually simulate.
      caption = "Probability of success versus treatment-effect drift for the p-value-based Power Prior (k = 20, lambda = 0.5) in the Botox case study, with 58 participants per arm; includes comparisons with t-tests at nominal and matched type I error rates.",
      case_study = "botox", sample_size_factor = 4, metric = "success_proba",
      needs = "frequentist",
      generator = function(ctx) {
        plot_success_proba_vs_drift(
          metric = "success_proba",
          results_metrics_df = ctx$df,
          theta_0 = ctx$theta_0,
          case_study = "botox",
          method = "p_value_based_PP",
          target_sample_size_per_arm = ctx$target_sample_size_per_arm,
          target_to_source_std_ratio = 1,
          source_denominator_change_factor = 1,
          parameters_combinations = data.frame(shape_parameter = 20, equivalence_margin = 0.5),
          xvars = xvars,
          join_points = TRUE,
          baseline_success_proba = c("at_equivalent_TIE", "at_nominal_TIE")
        )
      }
    ),
    list(
      id = "S5", kind = "figure",
      caption = "MSE versus mean moment-based effective sample size (ESS) in the Botox case study, with 117 participants per arm.",
      case_study = "botox", sample_size_factor = 2, metric = "mse",
      needs = "frequentist",
      generator = function(ctx) {
        plot_metric_vs_ess(
          results_metrics_df = ctx$df,
          case_study = "botox",
          target_sample_size_per_arm = ctx$target_sample_size_per_arm,
          treatment_effect = "partially_consistent",
          ess_method = inference_metrics$ess_moment,
          metric = frequentist_metrics$mse,
          source_denominator_change_factor = 1,
          target_to_source_std_ratio = 1
        )
      }
    ),
    list(
      id = "S20", kind = "figure",
      caption = "MSE versus treatment-effect drift for the Robust Mixture Prior in the Belimumab case study, with 93 participants per arm, across different informative-component weights w.",
      case_study = "belimumab", sample_size_factor = 6, metric = "mse",
      needs = "frequentist",
      generator = function(ctx) {
        plot_metric_vs_drift(
          metric = "mse",
          results_metrics_df = ctx$df,
          theta_0 = ctx$theta_0,
          case_study = "belimumab",
          method = "RMP",
          category = "parameters",
          control_drift = FALSE,
          target_sample_size_per_arm = ctx$target_sample_size_per_arm,
          parameters_combinations = NULL,
          xvars = xvars,
          target_to_source_std_ratio = 1,
          source_denominator_change_factor = 1,
          analysis_config = ctx$analysis_config
        )
      }
    )
  )
}
```

- [ ] **Step 7: Implement the table entries and the public accessors**

Append:

```r
paper_manifest_tables <- function() {
  list(
    list(
      id = "TS1", kind = "table",
      caption = "Total target-study sample sizes considered for each case study.",
      case_study = NA_character_, sample_size_factor = NA_real_, metric = NA_character_,
      needs = "configs",
      generator = function(ctx) {
        table_target_sample_sizes(ctx$case_studies, ctx$sample_size_factors,
                                  ctx$case_studies_config_dir, ctx$tables_dir)
      }
    ),
    list(
      id = "TS2", kind = "table",
      caption = "Treatment-effect drift ranges considered for each case study.",
      case_study = NA_character_, sample_size_factor = NA_real_, metric = NA_character_,
      ## Written by R/simulation_scenarios.R during the run itself.
      needs = "run_artifact",
      generator = function(ctx) {
        source_path <- file.path(ctx$results_dir, "drift_ranges.tex")
        if (!file.exists(source_path)) {
          stop("drift_ranges.tex is not in ", ctx$results_dir,
               " - it is written by the simulation run, not by the exporter.")
        }
        destination <- file.path(ctx$tables_dir, "drift_ranges.tex")
        file.copy(source_path, destination, overwrite = TRUE)
        destination
      }
    ),
    list(
      id = "TS7", kind = "table",
      caption = "Summary of the clinical case studies used to construct the simulation-study design.",
      case_study = NA_character_, sample_size_factor = NA_real_, metric = NA_character_,
      needs = "configs",
      generator = function(ctx) {
        table_case_study_summary(ctx$case_studies, ctx$case_studies_config_dir,
                                 ctx$tables_dir)
      }
    )
  )
}

#' The paper figure and table manifest
#'
#' @description Every figure and generated table in the paper, in publication
#'   order, each paired with the generator call that produces it. Table ids are
#'   prefixed `TS` so they never collide with a figure of the same number -
#'   figure S8 and table S8 are different objects, and only the figure is in
#'   scope.
#'
#' @return A list of manifest entries.
#'
#' @export
paper_manifest <- function() {
  entries <- c(
    paper_manifest_figures_forest(),
    paper_manifest_figures_vs_tie(),
    paper_manifest_figures_special(),
    paper_manifest_tables()
  )
  order_key <- function(entry) {
    if (entry$kind == "table") {
      return(1000 + as.numeric(sub("^TS", "", entry$id)))
    }
    if (startsWith(entry$id, "S")) {
      return(100 + as.numeric(sub("^S", "", entry$id)))
    }
    as.numeric(entry$id)
  }
  entries[order(vapply(entries, order_key, numeric(1)))]
}

#' Ids of every paper item in the manifest
#'
#' @return A character vector of ids in publication order.
#'
#' @export
paper_manifest_ids <- function() {
  vapply(paper_manifest(), function(entry) entry$id, character(1))
}

#' Look up one manifest entry by id
#'
#' @param id A manifest id, e.g. "1", "S20" or "TS7".
#'
#' @return The matching manifest entry.
#'
#' @export
paper_manifest_entry <- function(id) {
  entries <- paper_manifest()
  match_index <- which(vapply(entries, function(entry) entry$id, character(1)) == id)
  if (length(match_index) == 0) {
    stop("No paper manifest entry with id ", id, ".")
  }
  entries[[match_index]]
}
```

- [ ] **Step 8: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-paper_figures_manifest.R")'`
Expected: PASS, 6 tests. If the id count is not 42, recount against the spec's entry list before changing the test.

- [ ] **Step 9: Regenerate only the new docs and commit**

```bash
Rscript -e 'devtools::document()'
git checkout -- DESCRIPTION
git status --short
git checkout -- $(git diff --name-only man/ 2>/dev/null)
git add R/paper_figures_manifest.R tests/testthat/test-paper_figures_manifest.R man/paper_manifest.Rd man/paper_manifest_ids.Rd man/paper_manifest_entry.Rd man/paper_sample_size_per_arm.Rd
git commit -m "Record which generator call produces each paper figure and table

Nothing mapped a paper figure number to a call before this; plots.R only ran
loops that emitted every combination under generated filenames. Table ids are
prefixed TS so figure S8 and table S8 stay distinct.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: Requirement folding and coverage

**Files:**
- Create: `R/paper_replication.R`
- Test: `tests/testthat/test-paper_replication.R`

**Interfaces:**
- Consumes: `paper_manifest()`, `paper_manifest_entry()`, `paper_sample_size_per_arm()` from Task 2.
- Produces:
  - `paper_replication_requirements(ids, case_studies_config_dir)` → a `scenarios_config` list with elements `n_replicates`, `ndrift`, `parallelization`, `denominator_change_factor`, `sample_size_factors`, `target_to_source_std_ratio_range`, `case_studies`, `methods`.
  - `paper_replication_coverage(results_df, ids, case_studies_config_dir)` → a data frame with columns `id`, `covered` (logical), `reason` (character).

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-paper_replication.R`:

```r
## Folding a selection down to the smallest simulation config that still
## reproduces it is the whole point of the page: the full `combined` env is
## HPC-scale and most of its grid is never plotted.

config_dir <- function() {
  paste0(system.file("conf/case_studies", package = "BExTE"), "/")
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
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-paper_replication.R")'`
Expected: FAIL — `could not find function "paper_replication_requirements"`.

- [ ] **Step 3: Implement the two functions**

Create `R/paper_replication.R`:

```r
## Deriving the smallest simulation config that still reproduces a chosen set
## of paper figures, and checking an existing results directory against them.

## Every method appears in the forest plots, so a selection never narrows this.
PAPER_METHODS <- c(
  "RMP", "separate", "pooling", "conditional_power_prior",
  "test_then_pool_equivalence", "test_then_pool_difference",
  "p_value_based_PP", "EB_PP", "PDCCPP", "NPP", "commensurate_power_prior"
)

#' Minimal simulation config for a set of paper figures
#'
#' @description Folds the selected manifest entries into a `scenarios_config`
#'   covering exactly the case studies and sample size factors they need. The
#'   fidelity settings do not scale with the selection: `ndrift` stays at 30
#'   because `forest_plot()` selects the three principal treatment-effect
#'   scenarios by nearest grid point, so a coarser grid would quietly plot
#'   different drift values rather than failing.
#'
#'   No paper figure varies the source denominator change factor or the
#'   target-to-source standard deviation ratio, so both are pinned to 1.
#'
#' @param ids Manifest ids to cover.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#'
#' @return A `scenarios_config` list, ready for `save_environment()`.
#'
#' @export
paper_replication_requirements <- function(ids, case_studies_config_dir) {
  entries <- lapply(ids, paper_manifest_entry)
  entries <- Filter(function(entry) !is.na(entry$case_study), entries)

  case_studies <- unique(vapply(entries, function(e) e$case_study, character(1)))
  factors <- unique(vapply(entries, function(e) e$sample_size_factor, numeric(1)))

  list(
    n_replicates = 10000,
    ndrift = 30,
    parallelization = TRUE,
    denominator_change_factor = 1,
    sample_size_factors = sort(factors),
    target_to_source_std_ratio_range = 1,
    case_studies = case_studies,
    methods = PAPER_METHODS
  )
}

#' Check a results frame against a set of paper figures
#'
#' @param results_df A frequentist results frame.
#' @param ids Manifest ids to check.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#'
#' @return A data frame with columns `id`, `covered` and `reason`.
#'
#' @export
paper_replication_coverage <- function(results_df, ids, case_studies_config_dir) {
  rows <- lapply(ids, function(id) {
    entry <- paper_manifest_entry(id)

    if (is.na(entry$case_study)) {
      return(data.frame(id = id, covered = TRUE, reason = "", stringsAsFactors = FALSE))
    }

    if (!entry$case_study %in% results_df$case_study) {
      return(data.frame(
        id = id, covered = FALSE,
        reason = paste0("no ", entry$case_study, " rows"),
        stringsAsFactors = FALSE
      ))
    }

    per_arm <- paper_sample_size_per_arm(
      entry$case_study, entry$sample_size_factor, case_studies_config_dir
    )
    slice <- results_df[results_df$case_study == entry$case_study &
                          results_df$target_sample_size_per_arm == per_arm, , drop = FALSE]
    if (nrow(slice) == 0) {
      return(data.frame(
        id = id, covered = FALSE,
        reason = paste0("no rows at ", per_arm, " per arm"),
        stringsAsFactors = FALSE
      ))
    }

    if (!is.na(entry$metric) && !entry$metric %in% names(results_df)) {
      return(data.frame(
        id = id, covered = FALSE,
        reason = paste0("no ", entry$metric, " column"),
        stringsAsFactors = FALSE
      ))
    }

    data.frame(id = id, covered = TRUE, reason = "", stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-paper_replication.R")'`
Expected: PASS, 6 tests.

- [ ] **Step 5: Regenerate docs and commit**

```bash
Rscript -e 'devtools::document()'
git checkout -- DESCRIPTION
git status --short
git add R/paper_replication.R tests/testthat/test-paper_replication.R man/paper_replication_requirements.Rd man/paper_replication_coverage.Rd
git commit -m "Derive the minimal simulation config a paper figure selection needs

Ticking only the four main figures reduces the grid roughly 70x against the
combined env; ticking everything reduces it about 8x. ndrift and n_replicates
stay fixed because the forest plots pick their three scenarios by nearest grid
point.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: The case-study tables (S1 and S7)

**Files:**
- Create: `R/table_case_study_summary.R`
- Test: `tests/testthat/test-table_case_study_summary.R`

**Interfaces:**
- Consumes: `paper_sample_size_per_arm()` from Task 2, `export_table()` from `R/tables_utils.R`.
- Produces:
  - `table_target_sample_sizes(case_studies, sample_size_factors, case_studies_config_dir, tables_dir)` → the written `.tex` path.
  - `table_case_study_summary(case_studies, case_studies_config_dir, tables_dir)` → the written `.tex` path.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-table_case_study_summary.R`:

```r
## Tables S1 and S7 are derived from the case study YAMLs, not from simulation
## output, so they can be generated before any run finishes.

config_dir <- function() {
  paste0(system.file("conf/case_studies", package = "BExTE"), "/")
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
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-table_case_study_summary.R")'`
Expected: FAIL — `could not find function "table_target_sample_sizes"`.

- [ ] **Step 3: Implement both generators**

Create `R/table_case_study_summary.R`:

```r
## Supplementary tables S1 and S7. Both are derived from the case study YAMLs
## rather than from simulation output.

#' Total target-study sample sizes considered for each case study (table S1)
#'
#' @param case_studies Character vector of case study names.
#' @param sample_size_factors Numeric vector of sample size factors.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#' @param tables_dir Directory to write into.
#'
#' @return The path of the written `.tex` file.
#'
#' @export
table_target_sample_sizes <- function(case_studies, sample_size_factors,
                                      case_studies_config_dir, tables_dir) {
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

  rows <- lapply(case_studies, function(case_study) {
    config <- yaml::read_yaml(file.path(case_studies_config_dir, paste0(case_study, ".yml")))
    per_arm <- vapply(
      sample_size_factors,
      function(factor) paper_sample_size_per_arm(case_study, factor, case_studies_config_dir),
      numeric(1)
    )
    row <- as.data.frame(as.list(2 * per_arm))
    names(row) <- paste0("N_T (factor ", sample_size_factors, ")")
    cbind(data.frame(`Case study` = config$name, check.names = FALSE), row)
  })

  table_data <- do.call(rbind, rows)
  file_path <- file.path(tables_dir, "target_sample_sizes")

  export_table(
    data_table = table_data,
    title = "Total target-study sample sizes considered for each case study.",
    file_path = file_path,
    longtable = FALSE
  )

  paste0(file_path, ".tex")
}

#' Summary of the clinical case studies (table S7)
#'
#' @param case_studies Character vector of case study names.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#' @param tables_dir Directory to write into.
#'
#' @return The path of the written `.tex` file.
#'
#' @export
table_case_study_summary <- function(case_studies, case_studies_config_dir, tables_dir) {
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

  rows <- lapply(case_studies, function(case_study) {
    config <- yaml::read_yaml(file.path(case_studies_config_dir, paste0(case_study, ".yml")))
    data.frame(
      `Case study` = config$name,
      `Control` = config$control,
      `Endpoint` = config$endpoint,
      `Summary measure` = config$summary_measure_likelihood,
      ## control + treatment, not the `total:` field, which is stale for
      ## aprepitant - see R/simulation_scenarios.R.
      `Source N` = config$source$control + config$source$treatment,
      `Source effect` = round(config$source$treatment_effect, 4),
      `Source SE` = round(config$source$standard_error, 4),
      `Target N` = config$target$control + config$target$treatment,
      `Target effect` = round(config$target$treatment_effect, 4),
      `Target SE` = round(config$target$standard_error, 4),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  table_data <- do.call(rbind, rows)
  file_path <- file.path(tables_dir, "case_study_summary")

  export_table(
    data_table = table_data,
    title = "Summary of the clinical case studies used to construct the simulation-study design.",
    file_path = file_path,
    longtable = FALSE
  )

  paste0(file_path, ".tex")
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-table_case_study_summary.R")'`
Expected: PASS, 2 tests. If `export_table()` also tries to build a PDF and fails without LaTeX, assert only on the `.tex` file and note it in the test.

- [ ] **Step 5: Regenerate docs and commit**

```bash
Rscript -e 'devtools::document()'
git checkout -- DESCRIPTION
git status --short
git add R/table_case_study_summary.R tests/testthat/test-table_case_study_summary.R man/table_target_sample_sizes.Rd man/table_case_study_summary.Rd
git commit -m "Generate supplementary tables S1 and S7 from the case study configs

Both are derived from the YAMLs rather than from simulation output, so they
can be produced before any run finishes. Source N uses control + treatment,
which disagrees with the stale total field in aprepitant.yml.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 5: The exporter

**Files:**
- Create: append to `R/paper_replication.R`
- Test: append to `tests/testthat/test-paper_replication.R`

**Interfaces:**
- Consumes: `paper_manifest_entry()` (Task 2), `paper_sample_size_per_arm()` (Task 2), the table generators (Task 4).
- Produces: `export_paper_outputs(results_dir, figures_dir, tables_dir, ids, case_studies_config_dir, progress = NULL)` → a data frame with columns `id`, `kind`, `caption`, `case_study`, `target_sample_size_per_arm`, `metric`, `status`, `message`, `outputs`. Also writes `manifest.csv` and `README.md` into `tables_dir`. `progress` is an optional `function(index, total, id)` callback.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-paper_replication.R`:

```r
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
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-paper_replication.R")'`
Expected: FAIL — `could not find function "export_paper_outputs"`.

- [ ] **Step 3: Implement the exporter**

Append to `R/paper_replication.R`:

```r
#' Build the context one manifest entry's generator receives
#'
#' @description The generators take the slice already narrowed to their own
#'   case study, sample size, denominator factor and standard deviation ratio,
#'   mirroring what the loop functions in inst/scripts/plots.R pass them.
#'
#' @keywords internal
paper_entry_context <- function(entry, results_df, results_dir, tables_dir,
                                case_studies_config_dir, case_studies,
                                sample_size_factors, analysis_config) {
  ctx <- list(
    results_dir = results_dir,
    tables_dir = tables_dir,
    case_studies_config_dir = case_studies_config_dir,
    case_studies = case_studies,
    sample_size_factors = sample_size_factors,
    analysis_config = analysis_config
  )

  if (is.na(entry$case_study)) {
    return(ctx)
  }

  per_arm <- paper_sample_size_per_arm(
    entry$case_study, entry$sample_size_factor, case_studies_config_dir
  )
  case_config <- yaml::read_yaml(
    file.path(case_studies_config_dir, paste0(entry$case_study, ".yml"))
  )

  slice <- results_df[
    results_df$case_study == entry$case_study &
      results_df$target_sample_size_per_arm == per_arm &
      (results_df$source_denominator_change_factor == 1 |
         is.na(results_df$source_denominator_change_factor)) &
      (results_df$target_to_source_std_ratio == 1 |
         is.na(results_df$target_to_source_std_ratio)),
    ,
    drop = FALSE
  ]
  if (nrow(slice) == 0) {
    stop(
      "No rows for ", entry$case_study, " at ", per_arm,
      " per arm with denominator factor 1 and standard deviation ratio 1."
    )
  }

  ctx$df <- slice
  ctx$target_sample_size_per_arm <- per_arm
  ctx$theta_0 <- case_config$theta_0
  ctx
}

#' Produce the paper's figures and tables
#'
#' @description Runs each selected manifest entry's generator against
#'   `results_dir` and writes the results under `figures_dir` and `tables_dir`,
#'   keeping the generators' own filenames. `manifest.csv` records which paper
#'   item each file belongs to.
#'
#'   One entry failing does not abort the batch: it is recorded as `failed`
#'   with its error message and the run continues.
#'
#' @param results_dir A `results/<env>/` directory.
#' @param figures_dir Output directory for figures, trailing slash included -
#'   the plot generators append their own `<case_study>/` below it.
#' @param tables_dir Output directory for tables and the manifest.
#' @param ids Manifest ids to produce.
#' @param case_studies_config_dir Directory holding the case study YAMLs.
#' @param progress Optional `function(index, total, id)` progress callback.
#'
#' @return A status data frame, invisibly.
#'
#' @export
export_paper_outputs <- function(results_dir, figures_dir, tables_dir, ids,
                                 case_studies_config_dir, progress = NULL) {
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

  results_df <- readr::read_csv(
    file.path(results_dir, "results_frequentist.csv"),
    show_col_types = FALSE
  )
  analysis_config <- yaml::read_yaml(
    system.file("conf/analysis_config.yml", package = "BExTE")
  )

  entries <- lapply(ids, paper_manifest_entry)
  ## as.character()/as.numeric() strip the attributes na.omit() leaves behind.
  all_case_studies <- vapply(entries, function(e) e$case_study, character(1))
  case_studies <- unique(as.character(all_case_studies[!is.na(all_case_studies)]))
  all_factors <- vapply(entries, function(e) e$sample_size_factor, numeric(1))
  sample_size_factors <- sort(unique(as.numeric(all_factors[!is.na(all_factors)])))

  ## The plot generators resolve figures_dir and remake_figures as free
  ## variables out of .GlobalEnv - see inst/scripts/plots.R.
  previous_figures_dir <- if (exists("figures_dir", envir = .GlobalEnv)) {
    get("figures_dir", envir = .GlobalEnv)
  } else {
    NULL
  }
  assign("figures_dir", figures_dir, envir = .GlobalEnv)
  assign("remake_figures", TRUE, envir = .GlobalEnv)
  on.exit({
    if (is.null(previous_figures_dir)) {
      suppressWarnings(rm("figures_dir", envir = .GlobalEnv))
    } else {
      assign("figures_dir", previous_figures_dir, envir = .GlobalEnv)
    }
  }, add = TRUE)

  rows <- lapply(seq_along(entries), function(index) {
    entry <- entries[[index]]
    if (!is.null(progress)) {
      progress(index, length(entries), entry$id)
    }

    before <- c(
      list.files(figures_dir, recursive = TRUE, full.names = TRUE),
      list.files(tables_dir, recursive = TRUE, full.names = TRUE)
    )

    result <- tryCatch({
      ctx <- paper_entry_context(
        entry, results_df, results_dir, tables_dir, case_studies_config_dir,
        case_studies, sample_size_factors, analysis_config
      )
      entry$generator(ctx)
      list(status = "ok", message = "")
    }, error = function(e) {
      list(status = "failed", message = conditionMessage(e))
    })

    after <- c(
      list.files(figures_dir, recursive = TRUE, full.names = TRUE),
      list.files(tables_dir, recursive = TRUE, full.names = TRUE)
    )
    written <- setdiff(after, before)

    data.frame(
      id = entry$id,
      kind = entry$kind,
      caption = entry$caption,
      case_study = entry$case_study,
      target_sample_size_per_arm = if (is.na(entry$case_study)) {
        NA_real_
      } else {
        paper_sample_size_per_arm(entry$case_study, entry$sample_size_factor,
                                  case_studies_config_dir)
      },
      metric = entry$metric,
      status = result$status,
      message = result$message,
      outputs = paste(written, collapse = "; "),
      results_dir = results_dir,
      stringsAsFactors = FALSE
    )
  })

  status <- do.call(rbind, rows)
  readr::write_csv(status, file.path(tables_dir, "manifest.csv"))

  writeLines(
    c(
      "# Paper figures and tables",
      "",
      paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             " from ", results_dir, "."),
      "",
      "Filenames are the generators' own; `manifest.csv` maps each one to its",
      "paper figure or table number, caption and scenario.",
      "",
      "Not produced here: tables S3-S6 (design priors and the definitions of",
      "the Bayesian operating characteristics) are hand-authored in the",
      "manuscript, and table S8 is out of scope. Figure S8 is produced.",
      "",
      paste0(sum(status$status == "ok"), " of ", nrow(status),
             " items produced successfully.")
    ),
    file.path(tables_dir, "README.md")
  )

  invisible(status)
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-paper_replication.R")'`
Expected: PASS, 8 tests.

- [ ] **Step 5: Regenerate docs and commit**

```bash
Rscript -e 'devtools::document()'
git checkout -- DESCRIPTION
git status --short
git add R/paper_replication.R tests/testthat/test-paper_replication.R man/export_paper_outputs.Rd man/paper_entry_context.Rd
git commit -m "Export a selection of paper figures and tables with a manifest

Keeps the generators' own filenames and records the paper number in
manifest.csv. One entry failing is recorded rather than aborting the batch, so
a partial results directory still produces everything it can.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 6: Share the run launcher and the plot globals

**Files:**
- Modify: `inst/shiny_app/helpers.R` (append)
- Modify: `inst/shiny_app/modules/mod_run.R:88-140` (the `callr::r_bg` block inside `begin_run()`)
- Modify: `inst/shiny_app/modules/mod_analyze.R:134-160` (remove `prepare_plot_globals_for_env()`)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  - `launch_simulation_run(env)` → a `callr` process handle, started. Moved verbatim from `mod_run.R`.
  - `prepare_plot_globals_for_env(env, figures_dir)` → invisibly `TRUE` if it fell back to the template methods dict. Moved verbatim from `mod_analyze.R`.

This is a pure refactor: no behaviour changes, and the existing app tests must
still pass unchanged.

- [ ] **Step 1: Move `prepare_plot_globals_for_env()` and `ensure_plot_globals()` into `helpers.R`**

Cut these three, unchanged, from `mod_analyze.R` and paste them at the end of `inst/shiny_app/helpers.R` under a new `## ---- Plot globals ----` banner:

- `analyze_globals_ready <- new.env()` (`mod_analyze.R:17`)
- `ensure_plot_globals()` (`mod_analyze.R:111-121`)
- `prepare_plot_globals_for_env()` (`mod_analyze.R:134-160`)

Delete all three from `mod_analyze.R`. `helpers.R` is sourced before every module in `app.R`, so the call sites need no change.

- [ ] **Step 2: Extract `launch_simulation_run()` into `helpers.R`**

Append to `inst/shiny_app/helpers.R`:

```r
## ---- Simulation runs -------------------------------------------------------

#' Start a simulation environment in a background process
#'
#' @description Shared by the Run page and the Replicate paper page. Loads the
#'   package with devtools::load_all() in the child process rather than
#'   requiring BExTE to be installed, so the app works from a source checkout.
#'
#' @param env An environment name known to `list_environments()`.
#'
#' @return A started `callr` process handle.
launch_simulation_run <- function(env) {
  ensure_case_studies_snapshot(env)
  config_dir <- env_config_dir(env)
  case_studies_config_dir <- paste0(USER_CASE_STUDIES_DIR, "/")

  analysis_config <- yaml::read_yaml(system.file("conf/analysis_config.yml", package = "BExTE"))
  simulation_config <- yaml::read_yaml(system.file("conf/simulation_config.yml", package = "BExTE"))
  metrics_env <- new.env()
  source(system.file("conf/metrics_config.R", package = "BExTE"), local = metrics_env)

  callr::r_bg(
    func = function(pkg_root, wd, env, config_dir, case_studies_config_dir,
                    simulation_config, analysis_config, frequentist_metrics,
                    inference_metrics) {
      setwd(wd)
      devtools::load_all(pkg_root, quiet = TRUE)
      run_simulation_env(
        env = env,
        config_dir = config_dir,
        case_studies_config_dir = case_studies_config_dir,
        simulation_config = simulation_config,
        analysis_config = analysis_config,
        frequentist_metrics = frequentist_metrics,
        inference_metrics = inference_metrics
      )
    },
    args = list(
      pkg_root = find.package("BExTE"),
      wd = getwd(),
      env = env,
      config_dir = config_dir,
      case_studies_config_dir = case_studies_config_dir,
      simulation_config = simulation_config,
      analysis_config = analysis_config,
      frequentist_metrics = metrics_env$frequentist_metrics,
      inference_metrics = metrics_env$inference_metrics
    ),
    stdout = tempfile(fileext = ".out"),
    stderr = tempfile(fileext = ".err")
  )
}
```

- [ ] **Step 3: Make `mod_run.R` call the extracted launcher**

In `begin_run()`, replace everything from `ensure_case_studies_snapshot(env)` through the closing `)` of the `callr::r_bg(...)` assignment with:

```r
      state$total <- estimate_total_scenarios(env)
      state$env <- env
      state$start_time <- Sys.time()
      state$done <- FALSE
      state$exit_status <- NULL
      state$error_message <- NULL
      state$log_file <- file.path("logs", env, "error_logs", "error_log.log")

      state$proc <- launch_simulation_run(env)
```

Leave everything after that assignment untouched.

- [ ] **Step 4: Verify the app still starts and the moved functions resolve**

Run:

```bash
Rscript -e '
devtools::load_all()
app_dir <- system.file("shiny_app", package = "BExTE")
source(file.path(app_dir, "helpers.R"))
source(file.path(app_dir, "theme.R"))
source(file.path(app_dir, "modules", "mod_configure.R"))
source(file.path(app_dir, "modules", "mod_run.R"))
source(file.path(app_dir, "modules", "mod_analyze.R"))
stopifnot(is.function(launch_simulation_run))
stopifnot(is.function(prepare_plot_globals_for_env))
cat("app sources cleanly\n")'
```

Expected: `app sources cleanly`, no errors about duplicate or missing definitions.

- [ ] **Step 5: Run the full suite to confirm nothing regressed**

Run: `Rscript -e "devtools::test(stop_on_failure = FALSE)"`
Expected: the same failures as before this task — that is, only `test-simulation_analysis.R:61`. Any other failure is a regression from this refactor.

- [ ] **Step 6: Commit**

```bash
git add inst/shiny_app/helpers.R inst/shiny_app/modules/mod_run.R inst/shiny_app/modules/mod_analyze.R
git commit -m "Share the run launcher and plot globals between app pages

The Replicate paper page needs both. Pure refactor: launch_simulation_run()
and prepare_plot_globals_for_env() move into helpers.R unchanged, and their
former homes call them.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 7: The Replicate paper page

**Files:**
- Create: `inst/shiny_app/modules/mod_replicate.R`
- Modify: `inst/shiny_app/app.R` (source list and `page_navbar`, server body)

**Interfaces:**
- Consumes: `paper_manifest()`, `paper_manifest_ids()` (Task 2); `paper_replication_requirements()`, `paper_replication_coverage()`, `export_paper_outputs()` (Tasks 3, 5); `launch_simulation_run()`, `prepare_plot_globals_for_env()`, `save_environment()`, `estimate_total_scenarios()`, `list_results_dirs()`, `read_methods_template()` (Task 6 and existing helpers).
- Produces: `mod_replicate_ui(id)` and `mod_replicate_server(id, color_mode = NULL)`.

- [ ] **Step 1: Write the UI**

Create `inst/shiny_app/modules/mod_replicate.R`:

```r
## The Replicate paper page: pick which published figures and tables to
## reproduce, run only the simulations they need, then export them.
##
## The figure/table -> generator mapping lives in R/paper_figures_manifest.R;
## this module is a thin UI over it.

replicate_choice_labels <- function() {
  entries <- paper_manifest()
  labels <- vapply(entries, function(entry) {
    prefix <- if (entry$kind == "table") {
      paste0("Table S", sub("^TS", "", entry$id))
    } else {
      paste0("Figure ", entry$id)
    }
    paste0(prefix, " - ", entry$caption)
  }, character(1))
  stats::setNames(vapply(entries, function(entry) entry$id, character(1)), labels)
}

replicate_main_ids <- function() {
  ids <- paper_manifest_ids()
  ids[!startsWith(ids, "S") & !startsWith(ids, "TS")]
}

mod_replicate_ui <- function(id) {
  ns <- shiny::NS(id)
  bexte_page(
    bexte_step(
      1, "Choose what to reproduce",
      note = "Each item names the generator call that produces it. Coverage is checked against the results directory you pick below.",
      bexte_fields(
        shiny::selectInput(ns("results_dir"), "Results directory", choices = NULL),
        shiny::div(
          bexte_action_button(ns("select_all"), "Select all"),
          bexte_action_button(ns("select_main"), "Main figures only"),
          bexte_action_button(ns("select_none"), "Clear")
        )
      ),
      shiny::checkboxGroupInput(ns("items"), NULL, choices = NULL),
      shiny::uiOutput(ns("coverage"))
    ),
    bexte_step(
      2, "Run the simulations the selection needs",
      note = "Only the case studies and sample sizes your selection plots are simulated. Anything the results directory already covers is skipped.",
      shiny::uiOutput(ns("workload")),
      bexte_action_button(ns("run"), "Run required simulations"),
      shiny::uiOutput(ns("run_state")),
      shiny::verbatimTextOutput(ns("run_log"))
    ),
    bexte_step(
      3, "Produce the figures and tables",
      note = "Figures keep their generated filenames; manifest.csv maps each one to its paper number.",
      bexte_action_button(ns("export"), "Produce figures and tables"),
      shiny::uiOutput(ns("export_status")),
      shiny::tableOutput(ns("export_table"))
    )
  )
}
```

- [ ] **Step 2: Write the server**

Append to `inst/shiny_app/modules/mod_replicate.R`:

```r
mod_replicate_server <- function(id, color_mode = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveValues(proc = NULL, env = NULL, export = NULL)

    shiny::updateCheckboxGroupInput(
      session, "items",
      choices = replicate_choice_labels(),
      selected = replicate_main_ids()
    )

    refresh_results_dirs <- function() {
      dirs <- list_results_dirs()
      shiny::updateSelectInput(session, "results_dir", choices = dirs)
    }
    refresh_results_dirs()

    case_studies_dir <- function() {
      paste0(system.file("conf/case_studies", package = "BExTE"), "/")
    }

    shiny::observeEvent(input$select_all, {
      shiny::updateCheckboxGroupInput(session, "items", selected = paper_manifest_ids())
    })
    shiny::observeEvent(input$select_main, {
      shiny::updateCheckboxGroupInput(session, "items", selected = replicate_main_ids())
    })
    shiny::observeEvent(input$select_none, {
      shiny::updateCheckboxGroupInput(session, "items", selected = character(0))
    })

    coverage <- shiny::reactive({
      ids <- input$items
      shiny::req(length(ids) > 0)
      dir <- input$results_dir
      if (is.null(dir) || !nzchar(dir)) {
        return(NULL)
      }
      df <- readr::read_csv(
        file.path(dir, "results_frequentist.csv"), show_col_types = FALSE
      )
      paper_replication_coverage(df, ids, case_studies_dir())
    })

    output$coverage <- shiny::renderUI({
      cov <- coverage()
      if (is.null(cov)) {
        return(bexte_empty("Pick a results directory to check coverage."))
      }
      covered <- sum(cov$covered)
      ## A badge per checkbox row is not reachable through
      ## checkboxGroupInput(), so the uncovered items are named here instead.
      uncovered <- cov[!cov$covered, , drop = FALSE]
      shiny::tagList(
        bexte_status(
          sprintf("%d of %d selected items are covered by this results directory.",
                  covered, nrow(cov)),
          ok = covered == nrow(cov)
        ),
        if (nrow(uncovered) > 0) {
          shiny::p(
            class = "bexte-note",
            paste0(
              "Not covered: ",
              paste(uncovered$id, " (", uncovered$reason, ")",
                    sep = "", collapse = "; "),
              "."
            )
          )
        }
      )
    })

    missing_ids <- shiny::reactive({
      cov <- coverage()
      if (is.null(cov)) input$items else cov$id[!cov$covered]
    })

    output$workload <- shiny::renderUI({
      ids <- missing_ids()
      if (length(ids) == 0) {
        return(bexte_status("Nothing to run - the results directory covers everything selected."))
      }
      requirements <- paper_replication_requirements(ids, case_studies_dir())
      shiny::div(
        class = "bexte-workload",
        shiny::strong(sprintf(
          "%d case studies, sample size factors %s, %d replicates, %d drift points",
          length(requirements$case_studies),
          paste(requirements$sample_size_factors, collapse = ", "),
          requirements$n_replicates,
          requirements$ndrift
        ))
      )
    })

    shiny::observeEvent(input$run, {
      ids <- missing_ids()
      if (length(ids) == 0) {
        shiny::showNotification("Nothing to run.", type = "message")
        return()
      }
      if (!is.null(state$proc) && state$proc$is_alive()) {
        shiny::showNotification("A run is already in progress.", type = "warning")
        return()
      }

      requirements <- paper_replication_requirements(ids, case_studies_dir())
      env <- paste0("paper_replication_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      methods_dict <- read_methods_template()[requirements$methods]

      save_environment(
        env = env,
        scenarios_config = requirements,
        mcmc_config = yaml::read_yaml(
          file.path(system.file("conf/combined", package = "BExTE"), "mcmc_config.yml")
        ),
        methods_dict_selected = methods_dict
      )

      state$env <- env
      state$proc <- launch_simulation_run(env)
      shiny::showNotification(paste0("Running ", env, "."), type = "message")
    })

    output$run_state <- shiny::renderUI({
      if (is.null(state$proc)) {
        return(bexte_state("idle"))
      }
      shiny::invalidateLater(2000, session)
      if (state$proc$is_alive()) bexte_state("running", "live") else bexte_state("finished", "done")
    })

    output$run_log <- shiny::renderText({
      shiny::req(state$env)
      shiny::invalidateLater(2000, session)
      log_file <- file.path("logs", state$env, "error_logs", "error_log.log")
      if (!file.exists(log_file)) {
        return("")
      }
      paste(utils::tail(readLines(log_file, warn = FALSE), 40), collapse = "\n")
    })

    shiny::observeEvent(input$export, {
      ids <- input$items
      shiny::req(length(ids) > 0)
      results_dir <- input$results_dir
      shiny::req(nzchar(results_dir))

      run_name <- basename(results_dir)
      figures_dir <- file.path("figures", "publication_figures", run_name, "")
      tables_dir <- file.path("tables", "publication_tables", run_name)

      shiny::withProgress(message = "Producing figures and tables", value = 0, {
        state$export <- export_paper_outputs(
          results_dir = results_dir,
          figures_dir = figures_dir,
          tables_dir = tables_dir,
          ids = ids,
          case_studies_config_dir = case_studies_dir(),
          progress = function(index, total, id) {
            shiny::setProgress(value = index / total, detail = id)
          }
        )
      })
    })

    output$export_status <- shiny::renderUI({
      status <- state$export
      if (is.null(status)) {
        return(bexte_empty("Nothing produced yet."))
      }
      ok <- sum(status$status == "ok")
      bexte_status(
        sprintf("%d of %d produced. Manifest written alongside the tables.",
                ok, nrow(status)),
        ok = ok == nrow(status)
      )
    })

    output$export_table <- shiny::renderTable({
      status <- state$export
      shiny::req(status)
      status[, c("id", "kind", "case_study", "target_sample_size_per_arm",
                 "metric", "status", "message")]
    })
  })
}
```

- [ ] **Step 3: Register the page in `app.R`**

Add after the `mod_analyze.R` source line:

```r
source(file.path(app_dir, "modules", "mod_replicate.R"))
```

Add after the `bslib::nav_panel("Analyze", ...)` line:

```r
    bslib::nav_panel("Replicate paper", mod_replicate_ui("replicate")),
```

Add at the end of the `server` function body:

```r
  mod_replicate_server("replicate", color_mode = color_mode)
```

- [ ] **Step 4: Verify the app sources and the module builds its UI**

Run:

```bash
Rscript -e '
devtools::load_all()
app_dir <- system.file("shiny_app", package = "BExTE")
source(file.path(app_dir, "helpers.R"))
source(file.path(app_dir, "theme.R"))
source(file.path(app_dir, "modules", "mod_replicate.R"))
ui <- mod_replicate_ui("replicate")
stopifnot(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
labels <- replicate_choice_labels()
stopifnot(length(labels) == 42)
cat("replicate page builds,", length(labels), "items\n")'
```

Expected: `replicate page builds, 42 items`.

- [ ] **Step 5: Launch the app and confirm the page renders**

Run: `Rscript -e 'devtools::load_all(); BExTE::run_bexte_app(port = 8111, launch.browser = FALSE)'`
Open `http://127.0.0.1:8111`, click "Replicate paper", and confirm: the checklist lists 42 items with the main four preselected, the results-directory picker is populated, and the three buttons render. Stop the app.

- [ ] **Step 6: Commit**

```bash
git add inst/shiny_app/modules/mod_replicate.R inst/shiny_app/app.R
git commit -m "Add the Replicate paper page to the Shiny app

Picks which published figures and tables to reproduce, runs only the
simulations that selection needs, and exports them with a manifest mapping
each generated filename back to its paper number.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 8: Documentation

**Files:**
- Modify: `README.md` (the "Browser-based interface" section)
- Modify: `_pkgdown.yml` (the `reference:` block)

**Interfaces:**
- Consumes: everything above. Produces no new code.

- [ ] **Step 1: Update the README**

In the "Browser-based interface" section, change "This opens a local Shiny app with three tabs:" to "four tabs:" and append after the **Analyze** bullet:

```markdown
- **Replicate paper**: pick which of the paper's figures and tables to reproduce, run only the simulations that selection needs, and export them to `figures/publication_figures/` and `tables/publication_tables/` with a `manifest.csv` mapping each file to its figure or table number. Tables S3-S6 and table S8 are not produced - see `specs/2026-09-11-replicate-paper-page-design.md`.
```

- [ ] **Step 2: Add the new exported functions to the pkgdown reference**

In `_pkgdown.yml`, add `scale_by_separate` to the `- title: Plots` contents list, and add a new section after it:

```yaml
- title: Paper replication
- contents:
  - paper_manifest
  - paper_manifest_ids
  - paper_manifest_entry
  - paper_sample_size_per_arm
  - paper_replication_requirements
  - paper_replication_coverage
  - export_paper_outputs
  - table_target_sample_sizes
  - table_case_study_summary
```

- [ ] **Step 3: Verify pkgdown still has complete reference coverage**

Run: `Rscript -e 'devtools::load_all(); pkgdown::check_pkgdown()'`
Expected: no error about topics missing from the reference index.

- [ ] **Step 4: Run the full test suite one last time**

Run: `Rscript -e "devtools::test(stop_on_failure = FALSE)"`
Expected: only `test-simulation_analysis.R:61` fails, exactly as on a clean checkout. Report the actual pass/fail counts.

- [ ] **Step 5: Commit**

```bash
git add README.md _pkgdown.yml
git commit -m "Document the Replicate paper page

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```
