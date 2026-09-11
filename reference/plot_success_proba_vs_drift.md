# Function to generate a metric vs drift plot

Function to generate a metric vs drift plot

## Usage

``` r
plot_success_proba_vs_drift(
  metric,
  results_metrics_df,
  theta_0,
  case_study,
  method,
  target_sample_size_per_arm = NULL,
  target_to_source_std_ratio,
  source_denominator_change_factor = NULL,
  parameters_combinations = NULL,
  xvars,
  join_points = FALSE,
  baseline_success_proba
)
```

## Arguments

- metric:

  The metric to plot

- results_metrics_df:

  The dataframe containing the results and metrics

- theta_0:

  The true treatment effect

- case_study:

  The case study name

- method:

  The method name

- target_sample_size_per_arm:

  The target sample size per arm

- target_to_source_std_ratio:

  Ratio between the target and source studies sampling standard
  deviation.

- parameters_combinations:

  The combinations of parameters to filter the results by

- xvars:

  A list of x-variables for control drift and treatment drift

- join_points:

  Whether to join the point with a line or not.

## Value

None
