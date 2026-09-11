# Calculate Sweet Spots for Multiple Metrics

This function calculates the sweet spots for various metrics over
multiple case studies and methods. It determines where the metric
differences between two methods cross a reference value.

## Usage

``` r
sweet_spot(results_freq_df, metrics, based_on_CI = TRUE, nominal_tie = NULL)
```

## Arguments

- results_freq_df:

  A dataframe containing the results frequency data with columns
  "case_study", "method", "source_denominator_change_factor",
  "control_drift", and "target_sample_size_per_arm".

- metrics:

  A list of metrics to evaluate. Each metric should have a `name`
  attribute.

- nominal_tie:

  The nominal type-I error threshold. Required when `metrics` includes
  `success_proba`.

## Value

A dataframe containing the sweet spots for each metric, case study,
method, and combination of parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- sweet_spot(results_freq_df, metrics, nominal_tie = 0.025)
} # }
```
