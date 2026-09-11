# Generate a table of hyperparameters estimated using Empirical Bayes vs drift

Generate a table of hyperparameters estimated using Empirical Bayes vs
drift

## Usage

``` r
table_empirical_bayes_hyperparameters_vs_drift(
  results_metrics_df,
  method,
  case_study,
  control_drift,
  xvars,
  source_denominator_change_factor,
  target_to_source_std_ratio,
  parameters_combinations
)
```

## Arguments

- results_metrics_df:

  A data frame containing the results metrics.

- method:

  The method to filter the results by.

- case_study:

  The case study to filter the results by.

- control_drift:

  A logical value indicating whether to control for drift.

- xvars:

  A list containing the x-variable configurations.

- target_sample_size_per_arm:

  The target sample size per arm to filter the results by.

## Value

This function does not return a value. It creates and saves tables in
HTML, PDF, and LaTeX formats.
