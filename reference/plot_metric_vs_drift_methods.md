# Function to generate a metric vs drift plot

Function to generate a metric vs drift plot

## Usage

``` r
plot_metric_vs_drift_methods(
  metric,
  results_metrics_df,
  theta_0,
  case_study,
  target_sample_size_per_arm,
  xvars,
  target_to_source_std_ratio = 1,
  join_points = FALSE,
  source_denominator_change_factor = 1
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

- target_sample_size_per_arm:

  The target sample size per arm

- xvars:

  A list of x-variables for control drift and treatment drift

- target_to_source_std_ratio:

  Ratio between the target and source studies sampling standard
  deviation.

- join_points:

  Whether to join the point with a line or not.

- method:

  The method name

- category:

  The category to group the results by (either "parameters" or
  "target_sample_size_per_arm")

- parameters_combinations:

  The combinations of parameters to filter the results by

## Value

None
