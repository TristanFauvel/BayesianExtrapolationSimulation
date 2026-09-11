# Function to plot metric vs sample size

This function plots a metric against the sample size for a given method,
case study, and parameters combinations.

## Usage

``` r
plot_metric_vs_sample_size_methods(
  metric,
  results_metrics_df,
  case_study,
  theta_0 = 0,
  target_to_source_std_ratio = 1,
  source_denominator_change_factor = 1,
  dodging = FALSE,
  join_points = FALSE,
  target_treatment_effect = target_treatment_effect
)
```

## Arguments

- metric:

  The metric to plot.

- results_metrics_df:

  The data frame containing the results and metrics.

- case_study:

  The case study to plot the metric for.

- theta_0:

  Boundary of the null hypothesis space.

## Value

None
