# "Replicate paper" page — design

Date: 2026-09-11
Status: approved, pending implementation plan

## Problem

The paper's 39 figures and 4 generated tables are each one specific slice of
the simulation grid: a case study, a target sample size, a metric, sometimes a
method and a parameter value. Nothing in the repository records that mapping.
`inst/scripts/plots.R` and `inst/scripts/tables.R` call loop functions
(`forest_plot_methods_comparison()`, `operating_characteristics_vs_tie_plots()`,
…) that emit *every* combination in the results frame under generated
filenames, and the person reproducing the paper is left to find the right file
by hand. There is also no way to know which simulations a given figure needs,
so the only honest reproduction today is "run the whole `combined` env on an
HPC and go fishing in the output".

This adds a fourth Shiny page that (a) records the figure/table → generator
mapping declaratively, (b) derives the minimal simulation config that covers a
chosen subset, and (c) exports that subset to a dedicated folder with a
manifest.

## Non-goals

- Tables S3–S6 (design priors for type I error and for power; definitions of
  the Bayesian operating characteristics). These describe the methodology, not
  simulation output; the design priors live as R6 classes in
  `R/analysis_design_prior.R` with no table-ready labels. The exporter writes a
  README line saying they are hand-authored in the manuscript.
- Renaming figure outputs to paper numbers. Generator filenames are preserved;
  the paper number is recorded in `manifest.csv`.
- Changing any existing plot or table generator's output for existing callers.
  The one new argument (`relative_to_separate`) defaults to `FALSE`.

## Sample-size resolution

Captions name `N_T/2`; configs name `sample_size_factors`. The pipeline
computes (`R/simulation_scenarios.R:238`):

```
total_target_sample_size  = (source$control + source$treatment) / factor
target_sample_size_per_arm = floor(total_target_sample_size / 2)
```

Note it uses `control + treatment`, not the `total:` field — they disagree for
aprepitant, where `total: 673` is stale and `293 + 280 = 573` is what runs.

| Case study | source N | f=2 | f=4 | f=6 | captions used |
|---|---|---|---|---|---|
| botox | 468 | 117 | 58 | 39 | 117 (f2), 58 (f4) |
| belimumab | 1125 | 281 | 140 | 93 | 140 (f4), 93 (f6) |
| dapagliflozin | 267 | 66 | 33 | 22 | 66 (f2), 33 (f4) |
| mepolizumab | 551 | 137 | 68 | 45 | 68 (f4), 45 (f6) |
| teriflunomide | 1483 | 370 | 185 | 123 | 185 (f4), 123 (f6) |
| aprepitant | 573 | 143 | 71 | 47 | 143 (f2), 71 (f4) |

Every caption resolves. The union of factors the paper needs is `{2, 4, 6}`;
factor 1 is never plotted.

## Components

### `R/paper_figures_manifest.R`

A list of entries, one per paper item:

```r
list(
  id         = "S9",
  kind       = "figure",              # or "table"
  caption    = "Type I error and power relative to a separate analysis ...",
  case_study = "botox",
  sample_size_factor = 4,             # resolved to per-arm at run time
  metric     = "success_proba",
  needs      = "frequentist",         # which results file
  generator  = function(ctx) forest_plot(ctx$df, "success_proba",
                                         relative_to_separate = TRUE)
)
```

The leaf generators already take the scenario coordinates explicitly, so each
entry is a direct call and no plotting logic is reimplemented:

| Caption shape | Generator | n |
|---|---|---|
| "…for the three principal treatment-effect scenarios" | `forest_plot(df, metric)` | 19 |
| "…relative to a separate analysis" | `forest_plot(df, metric, relative_to_separate = TRUE)` | 6 |
| "…versus type I error rate" | `operating_characteristic_vs_tie(..., treatment_effect =)` | 10 |
| "…versus treatment-effect drift … comparisons with t-tests" | `plot_success_proba_vs_drift(baseline_success_proba = c("at_equivalent_TIE", "at_nominal_TIE"))` | 2 |
| "MSE versus … drift … across different w" | `plot_metric_vs_drift(method = "RMP", category = "parameters")` | 1 |
| "MSE versus mean moment-based ESS" | `plot_metric_vs_ess(ess_method = ess_moment)` | 1 |

Full entry list:

**Main** — 1: `forest_plot(botox, f4, success_proba)`. 2:
`operating_characteristic_vs_tie(botox, f4, partially_consistent,
success_proba)`. 3: `forest_plot(botox, f2, mse)`. 4:
`operating_characteristic_vs_tie(botox, f2, partially_consistent, mse)`.

**Supplementary** — S3: `plot_success_proba_vs_drift(belimumab, f6,
conditional_power_prior, gamma = 0.25, both baselines)`. S4: same for
`(botox, f4, p_value_based_PP, k = 20, lambda = 0.5)`. S5:
`plot_metric_vs_ess(botox, f2, ess_moment, mse)`. S6:
`forest_plot(botox, f2, ess_moment)`. S7: `forest_plot(botox, f2, bias)`.
S8: `operating_characteristic_vs_tie(botox, f2, no_effect, coverage, ratio = 1)`.
S9: `forest_plot(botox, f4, success_proba, relative)`. S10:
`operating_characteristic_vs_tie(dapagliflozin, f4, partially_consistent, mse)`.
S11: `forest_plot(dapagliflozin, f2, coverage)`. S12: `…(dapagliflozin, f2, mse)`.
S13: `…(dapagliflozin, f2, success_proba)`. S14: `…(dapagliflozin, f2,
success_proba, relative)`. S15: `operating_characteristic_vs_tie(belimumab, f4,
partially_consistent, mse)`. S16: `forest_plot(belimumab, f4, success_proba)`.
S17: `…(belimumab, f4, success_proba, relative)`. S18: `…(belimumab, f4, mse)`.
S19: `…(belimumab, f4, precision)`. S20: `plot_metric_vs_drift(mse, belimumab,
f6, RMP, category = "parameters")`. S21: `forest_plot(belimumab, f4, coverage)`.
S22: `operating_characteristic_vs_tie(mepolizumab, f4, partially_consistent,
mse)`. S23: `…(mepolizumab, f4, no_effect, coverage)`. S24:
`forest_plot(mepolizumab, f4, mse)`. S25: `…(mepolizumab, f4, coverage)`. S26:
`…(mepolizumab, f6, success_proba)`. S27: `…(mepolizumab, f4, success_proba,
relative)`. S28: `operating_characteristic_vs_tie(teriflunomide, f6,
partially_consistent, mse)`. S29: `…(teriflunomide, f6, no_effect, coverage)`.
S30: `forest_plot(teriflunomide, f4, mse)`. S31: `…(teriflunomide, f6,
success_proba)`. S32: `…(teriflunomide, f6, success_proba, relative)`. S33:
`operating_characteristic_vs_tie(aprepitant, f4, partially_consistent, mse)`.
S34: `forest_plot(aprepitant, f2, mse)`. S35: `…(aprepitant, f2, coverage)`.
S36: `…(aprepitant, f2, success_proba)`. S37: `…(aprepitant, f2, success_proba,
relative)`.

**Tables** — S1 (total target sample sizes per case study): new generator from
the case-study YAMLs × `sample_size_factors`. S2 (drift ranges): already emitted
as `drift_ranges.tex` by `R/simulation_scenarios.R:288`; the exporter copies it.
S7 (case-study summary): new generator from the YAMLs. S8 (precision and
coverage, belimumab f4): two calls to `generate_comparison_table(df, metric)`.

### `R/paper_replication.R`

- `paper_replication_requirements(ids)` — folds the selected entries into a
  `scenarios_config`: union of case studies, union of sample-size factors, all
  11 methods, `denominator_change_factor: [1]`, `target_to_source_std_ratio_range:
  [1]`, `ndrift: 30`, `n_replicates: 10000`.
- `paper_replication_coverage(results_dir, ids)` — per entry, whether the
  results frame contains rows at that case study / sample size / metric.
- `export_paper_outputs(results_dir, figures_dir, tables_dir, ids, progress)` —
  runs each generator, returns a data frame of id / caption / status / paths.

### `forest_plot(..., relative_to_separate = FALSE)`

New argument. When `TRUE`, each row's metric is divided by the `separate`
method's value for the same `case_study`, `target_sample_size_per_arm`, `drift`,
`target_to_source_std_ratio` and `source_denominator_change_factor`; confidence
bounds are scaled by the same denominator, and a dotted reference line is drawn
at 1. Rows whose matched `separate` value is 0 or missing yield `NA` and are
dropped with a warning rather than producing `Inf`.

### `launch_simulation_run(env)`

Extracted verbatim from `mod_run.R:begin_run()` into `helpers.R` so the Run page
and the Replicate page share one `callr::r_bg` launcher instead of duplicating
it. `mod_run_server()` is refactored to call it; its behaviour is unchanged.

### `inst/shiny_app/modules/mod_replicate.R`

Three steps, following the existing `rbext_step()` layout:

1. **Select** — checklist of the 43 paper items (39 figures, 4 tables) grouped
   Main / Supplementary and sub-grouped by case study, with select-all. Ticking
   one item selects all of its manifest entries: S5 expands to three (S5a/b/c,
   one per treatment-effect scenario) and S8 to two (S8a/S8b, precision and
   coverage), so 43 ticked items produce 46 manifest entries. Each row shows
   its caption and a coverage badge against the chosen `results/<env>/`.
2. **Run** — shows the derived scenario count (`estimate_total_scenarios()`),
   writes the derived env under `user_configs/`, launches it via
   `launch_simulation_run()`, and reuses the Run page's progress bar, log tail
   and cancel button. Entries already covered by the selected results directory
   are excluded from the derived config, so an HPC `results/combined/` needs no
   local compute.
3. **Produce figures and tables** — one button; calls `export_paper_outputs()`
   and renders the returned status table.

Registered in `app.R` as `bslib::nav_panel("Replicate paper",
mod_replicate_ui("replicate"))` after `"Analyze"`.

Plot globals (`figures_dir`, `remake_figures`, `methods_dict`,
`case_studies_config_dir`) are set through the existing
`prepare_plot_globals_for_env()` from `mod_analyze.R`, moved to `helpers.R` so
both pages use it.

## Minimal derived config

Only case studies and sample-size factors vary with the selection. The rest is
fixed by fidelity requirements:

- `denominator_change_factor: [1]` and `target_to_source_std_ratio_range: [1]` —
  no paper figure varies either, and `forest_plot_methods_comparison()` already
  filters to a denominator factor of 1.
- `ndrift: 30` regardless of selection. `forest_plot()` selects the three
  principal scenarios by nearest grid point (`which.min(abs(...))`), so a
  coarser grid silently moves which drift values are plotted.
- `n_replicates: 10000`.

Ticking the four main figures reduces the grid ~70× against `combined`; ticking
everything reduces it ~8×. It remains a long run and the page says so before
launching.

## Output

Generator filenames preserved:

```
figures/publication_figures/<run>/<case_study>/<generated name>.{pdf,png}
tables/publication_tables/<run>/<generated name>.tex
tables/publication_tables/<run>/manifest.csv
tables/publication_tables/<run>/README.md
```

`manifest.csv` columns: `id`, `kind`, `caption`, `case_study`,
`target_sample_size_per_arm`, `metric`, `generator_call`, `output_paths`,
`results_dir`, `status`. `.gitignore` already whitelists
`figures/publication_figures/`.

## Testing

`tests/testthat/test-paper_figures_manifest.R`
- every entry's `id` is unique and its `generator` is callable
- every `case_study` names an existing YAML, every `sample_size_factor` resolves
  to the per-arm size the caption states (table above, asserted explicitly)
- `paper_replication_requirements()` folds a known selection to the expected
  case studies and factors

`tests/testthat/test-forest_plot_relative.R`
- the ratio is computed against the matched `separate` row, not a global mean
- a zero or missing `separate` value drops the row with a warning, never `Inf`
  (consistent with the project's rule against finite surrogates for undefined
  quantities)
- `relative_to_separate = FALSE` output is byte-identical to today's

`tests/testthat/test-paper_replication.R`
- coverage check correctly reports a missing case study / sample size
- `export_paper_outputs()` smoke test against the existing light results
  fixture, asserting `manifest.csv` rows and that files land at the recorded
  paths

Note: `tests/testthat/test-simulation_analysis.R:61` already fails on a clean
checkout and is unrelated to this work.

## Open items carried into implementation

1. **Figure S4 says λ=20.** `equivalence_margin` for `p_value_based_PP` is
   `{0.1, 0.5}` in every config in the repo; 20 appears nowhere. `k = 20` is
   valid (`shape_parameter` includes 20). Treated as a caption typo; the
   manifest uses λ = 0.5 (its `important_values`) and records the substitution
   in a comment and in `manifest.csv`.
2. **Figure S5 names no treatment-effect scenario** but `plot_metric_vs_ess()`
   requires one. The manifest emits all three (`no_effect`,
   `partially_consistent`, `consistent`) as S5a/b/c.
3. **Table S8 pairs precision and coverage**; `generate_comparison_table()`
   handles one metric per table, so it is emitted as two sub-tables S8a/S8b.
