# Calculate the success rate in the target study arm from the drift on the log odds ratio scale

This function calculates the success rate in the target study arm based
on the drift (defined on the log scale) and the success rate in the arm
of interest in the source study.

## Usage

``` r
rate_from_drift_logOR(arm_drift, source_rate)
```

## Arguments

- arm_drift:

  The drift (defined on the log scale) between the target and source
  study arms

- source_rate:

  The success rate in the arm of interest in the source study

## Value

The success rate in the corresponding arm of the target study
