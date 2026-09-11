# Compute odds ratios based on the number of participants and responders in each arm

This function computes odds ratios based on the number of participants
and responders in the control and treatment arms.

## Usage

``` r
compute_ORs(
  n_control,
  n_treatment,
  n_control_responders,
  n_treatment_responders
)
```

## Arguments

- n_control:

  Number of participants in the control arm

- n_treatment:

  Number of participants in the treatment arm

- n_control_responders:

  Number of responders in the control arm

- n_treatment_responders:

  Number of responders in the treatment arm

## Value

A list containing the log odds ratio, treatment rate, control rate, and
standard error of the log odds ratio
