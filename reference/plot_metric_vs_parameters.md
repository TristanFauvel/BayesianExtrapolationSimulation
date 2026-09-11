# Function to plot metric vs parameters

This function plots a metric against the parameters for a given method,
case study, and sample size.

## Usage

``` r
plot_metric_vs_parameters(
  results_metrics_df,
  metric,
  case_study = "belimumab",
  method = "RMP",
  target_sample_size_per_arm = 93,
  theta_0 = NULL,
  add_baselines = FALSE,
  target_to_source_std_ratio = 1,
  source_denominator_change_factor = 1,
  analysis_config
)
```

## Arguments

- results_metrics_df:

  The data frame containing the results and metrics.

- metric:

  The metric to plot.

- case_study:

  The case study to plot the metric for.

- method:

  The method to plot the metric for.

- target_sample_size_per_arm:

  The target sample size per arm to plot the metric for.

- theta_0:

  Boundary of the null hypothesis space.

- add_baselines:

  A logical value indicating whether to add baselines to the plot.

## Value

None
