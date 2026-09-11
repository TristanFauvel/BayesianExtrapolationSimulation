# Compute the log odds ratio from count data

Compute the log odds ratio from count data

## Usage

``` r
compute_log_odds_ratio_from_counts(
  n_control_responders,
  n_treatment_responders,
  n_control_nonresponders,
  n_treatment_nonresponders,
  continuity_correction
)
```

## Arguments

- n_control_responders:

  Number of responders in the control arm

- n_treatment_responders:

  Number of responders in the treatment arm

- n_control_nonresponders:

  Number of nonresponders in the control arm

- n_treatment_nonresponders:

  Number of nonresponders in the treatment arm

- continuity_correction:

  Whether to apply continuity correction or not

## Value

The log odds ratio
