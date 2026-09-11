# Function to plot posterior parameters vs drift

This function plots the posterior parameters against the drift for a
given method, case study, sample size, and control drift option.

## Usage

``` r
plot_posterior_parameters_vs_drift(
  results_metrics_df,
  method,
  case_study,
  target_sample_size_per_arm,
  control_drift,
  xvars,
  source_denominator_change_factor,
  target_to_source_std_ratio
)
```

## Arguments

- results_metrics_df:

  The data frame containing the results and metrics.

- method:

  The method to plot the parameters for.

- case_study:

  The case study to plot the parameters for.

- target_sample_size_per_arm:

  The sample size to plot the parameters for.

- control_drift:

  A logical value indicating whether to filter the data based on control
  drift.

- xvars:

  Variable on the x axis

## Value

None
