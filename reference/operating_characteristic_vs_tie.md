# Function to generate an operating characteristic vs tie plot

Function to generate an operating characteristic vs tie plot

## Usage

``` r
operating_characteristic_vs_tie(
  results_metrics_df,
  case_study,
  target_sample_size_per_arm,
  treatment_effect,
  operating_characteristic,
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

- operating_characteristic:

  The operating characteristic to plot (e.g., "power", "type_1_error")

- power_difference:

  Logical indicating whether to calculate difference for the operating
  characteristic

## Value

None
