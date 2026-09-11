# Calculate the rate in the target study arm from the drift on the log relative risk scale

This function calculates the rate in the target study arm based on the
drift (defined on the log scale) and the rate in the arm of interest in
the source study.

## Usage

``` r
rate_from_drift_logRR(arm_drift, source_rate)
```

## Arguments

- arm_drift:

  The drift (defined on the log scale) between the target and source
  study arms

- source_rate:

  The rate in the arm of interest in the source study

## Value

The rate in the corresponding arm of the target study
