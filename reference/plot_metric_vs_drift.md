# Function to generate a metric vs drift plot

Function to generate a metric vs drift plot

## Usage

``` r
plot_metric_vs_drift(
  metric,
  results_metrics_df,
  theta_0,
  case_study,
  method,
  category,
  control_drift,
  target_sample_size_per_arm,
  parameters_combinations,
  xvars,
  add_baselines = FALSE,
  target_to_source_std_ratio = 1,
  join_points = FALSE,
  source_denominator_change_factor = 1,
  analysis_config
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

- category:

  The category to group the results by (either "parameters" or
  "target_sample_size_per_arm")

- control_drift:

  Logical indicating whether to filter results by control drift (TRUE)
  or treatment drift (FALSE)

- target_sample_size_per_arm:

  The target sample size per arm

- parameters_combinations:

  The combinations of parameters to filter the results by

- xvars:

  A list of x-variables for control drift and treatment drift

- add_baselines:

  Logical indicating whether to add baselines to the plot

- target_to_source_std_ratio:

  Ratio between the target and source studies sampling standard
  deviation.

- join_points:

  Whether to join the point with a line or not.

## Value

None
