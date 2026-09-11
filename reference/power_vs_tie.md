# Function to generate a power vs tie plot

Function to generate a power vs tie plot

## Usage

``` r
power_vs_tie(
  results_metrics_df,
  case_study,
  target_sample_size_per_arm,
  treatment_effect,
  power_difference,
  metrics,
  source_denominator_change_factor,
  target_to_source_std_ratio
)
```

## Arguments

- results_metrics_df:

  The dataframe containing the results and metrics

- case_study:

  The case study name

- target_sample_size_per_arm:

  The target sample size per arm

- treatment_effect:

  The treatment effect type ("consistent", "no_effect",
  "partially_consistent")

- power_difference:

  Logical indicating whether to calculate power difference

- metrics:

  The metrics to include in the plot

## Value

None
