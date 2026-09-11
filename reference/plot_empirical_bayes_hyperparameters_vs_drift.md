# Plot Empirical Bayes Hyperparameters vs Drift

This function plots the hyperparameters updated using empirical Bayes
against the drift for a given method, case study sample size, and
control drift option.

## Usage

``` r
plot_empirical_bayes_hyperparameters_vs_drift(
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

  The data frame containing the results and metrics.

- method:

  The method to plot the parameters for.

- case_study:

  The case study to plot the parameters for.

- control_drift:

  A logical value indicating whether to filter the data based on control
  drift.

- xvars:

  Variable on the x axis

## Value

None
