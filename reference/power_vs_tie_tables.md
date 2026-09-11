# Generate power vs tie tables for multiple conditions

This function generates multiple power vs tie tables for various
combinations of case studies, sample sizes, and treatment effects.

## Usage

``` r
power_vs_tie_tables(results_metrics_df, metrics)
```

## Arguments

- results_metrics_df:

  A data frame containing the results and metrics.

- metrics:

  A vector of metric names (not used in the current implementation).

## Value

This function doesn't return a value but generates multiple tables using
table_power_vs_tie().
