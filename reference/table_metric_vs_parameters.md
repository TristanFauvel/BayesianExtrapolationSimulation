# Plot metric vs parameters

Plot metric vs parameters

## Usage

``` r
table_metric_vs_parameters(
  results_metrics_df,
  metric,
  case_study = "belimumab",
  method = "RMP",
  target_sample_size_per_arm = 93,
  theta_0 = NULL,
  source_denominator_change_factor = 1,
  target_to_source_std_ratio = 1
)
```

## Arguments

- results_metrics_df:

  A dataframe containing the results metrics

- metric:

  The metric to be plotted

- case_study:

  The case study being analyzed

- method:

  The method being used

- target_sample_size_per_arm:

  The target sample size per arm

- theta_0:

  The null hypothesis value

## Value

Generates and saves tables in HTML, PDF, and LaTeX formats
