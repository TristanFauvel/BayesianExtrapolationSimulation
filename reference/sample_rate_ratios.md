# Sample rate ratios based on the rates in the control and treatment arms

This function samples rate ratios based on the rates in the control and
treatment arms.

## Usage

``` r
sample_rate_ratios(
  control_rate,
  treatment_rate,
  n_replicates,
  n_control,
  n_treatment
)
```

## Arguments

- control_rate:

  The rate in the control arm

- treatment_rate:

  The rate in the treatment arm

- n_replicates:

  Number of replicates to sample

- n_control:

  Number of participants in the control arm

- n_treatment:

  Number of participants in the treatment arm

## Value

A vector of sampled rate ratios
