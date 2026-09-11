# Sample log odds ratios based on the number of participants and responders in each arm

This function samples log odds ratios based on the number of
participants and responders in the control and treatment arms.

## Usage

``` r
sample_log_odds_ratios(
  n_control,
  n_treatment,
  treatment_rate,
  control_rate,
  n_replicates
)
```

## Arguments

- n_control:

  Number of participants in the control arm

- n_treatment:

  Number of participants in the treatment arm

- treatment_rate:

  The rate of success (1s) in the treatment arm

- control_rate:

  The rate of success (1s) in the control arm

- n_replicates:

  Number of replicates to sample

## Value

A list containing the sampled log odds ratios and the standard error of
the log odds ratio
