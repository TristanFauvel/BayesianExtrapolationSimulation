# Function to plot metric vs sample size

This function plots a metric against the sample size for a given method,
case study, and parameters combinations.

## Usage

``` r
plot_metric_vs_sample_size(
  metric,
  results_metrics_df,
  case_study,
  method,
  parameters_combinations,
  theta_0 = 0,
  add_baselines = FALSE,
  target_to_source_std_ratio = 1,
  source_denominator_change_factor = 1,
  analysis_config
)
```

## Arguments

- metric:

  The metric to plot.

- results_metrics_df:

  The data frame containing the results and metrics.

- case_study:

  The case study to plot the metric for.

- method:

  The method to plot the metric for.

- parameters_combinations:

  The parameter combinations to filter the data on.

- theta_0:

  Boundary of the null hypothesis space.

- add_baselines:

  A logical value indicating whether to add baselines to the plot.

## Value

None
