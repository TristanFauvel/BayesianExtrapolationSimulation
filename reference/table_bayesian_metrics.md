# Generate tables for Bayesian metrics

Generate tables for Bayesian metrics

## Usage

``` r
table_bayesian_metrics(
  results_metrics_df,
  case_study,
  target_sample_size_per_arm,
  source_denominator_change_factor = 1,
  target_to_source_std_ratio = 1
)
```

## Arguments

- results_metrics_df:

  A data frame containing the results and metrics.

- case_study:

  Character string specifying the case study.

## Value

This function doesn't return a value but saves the generated tables as
HTML, PDF, and LaTeX files.
