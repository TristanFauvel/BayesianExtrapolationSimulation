# Generate a power vs tie table

This function creates a table comparing power (or power difference) and
type I error (TIE) for different methods under specific conditions.

## Usage

``` r
table_power_vs_tie(
  results_metrics_df,
  case_study,
  target_sample_size_per_arm,
  treatment_effect,
  power_difference,
  metrics,
  source_denominator_change_factor,
  target_to_source_std_ratio
)
```

## Arguments

- results_metrics_df:

  A data frame containing the results and metrics.

- case_study:

  The specific case study to filter the results.

- target_sample_size_per_arm:

  The target sample size per arm to filter the results.

- treatment_effect:

  Character string specifying the treatment effect: "consistent",
  "partially_consistent", or "no_effect".

- power_difference:

  Logical; if TRUE, calculate power difference instead of power.

- metrics:

  A vector of metric names (not used in the current implementation).

## Value

This function doesn't return a value but generates and saves tables in
various formats (HTML, PDF, LaTeX).
