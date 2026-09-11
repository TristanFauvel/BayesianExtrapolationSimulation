# Generate a comparison table

This function generates a comparison table for different methods and
treatment effects.

## Usage

``` r
generate_comparison_table(
  results_df,
  x_metric,
  source_denominator_change_factor,
  target_to_source_std_ratio
)
```

## Arguments

- results_df:

  A dataframe containing the results to be compared.

- x_metric:

  The metric to be used for comparison.

- case_study:

  The case study to filter the results.

- target_sample_size_per_arm:

  The target sample size per arm to filter the results.

## Value

A kable object representing the comparison table.
