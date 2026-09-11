# Generate tables for methods operating characteristics

This function creates tables for various Bayesian operating
characteristics across different case studies and methods.

## Usage

``` r
table_bayesian_ocs(
  results_metrics_df,
  metrics,
  source_denominator_change_factor = 1,
  target_to_source_std_ratio = 1
)
```

## Arguments

- results_metrics_df:

  A data frame containing the results and metrics.

- metrics:

  A vector of metric names to include in the tables.

## Value

This function doesn't return a value but calls other functions to
generate and save tables.
