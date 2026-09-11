# Compute the standard error of the log odds ratio

This function calculates the standard error of the log odds ratio based
on the number of noncases and cases in the unexposed and exposed groups.

## Usage

``` r
standard_error_log_odds_ratio(
  n_control_nonresponders,
  n_treatment_nonresponders,
  n_control_responders,
  n_treatment_responders,
  continuity_correction = TRUE
)
```

## Arguments

- n_control_nonresponders:

  Number of noncases in the unexposed group

- n_treatment_nonresponders:

  Number of noncases in the exposed group

- n_control_responders:

  Number of cases in the unexposed group

- n_treatment_responders:

  Number of cases in the exposed group

- continuity_correction:

  Logical; if TRUE, applies continuity correction (default is TRUE)

## Value

The standard error of the log odds ratio

The standard error of the log odds ratio

## Examples

``` r
standard_error_log_odds_ratio(100, 90, 50, 60)
#> [1] 0.2394387
```
