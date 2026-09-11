# Compute the frequentist power at equivalent tie

This function computes the frequentist power at equivalent tie for a
given set of results and analysis configuration.

## Usage

``` r
frequentist_power_at_equivalent_tie(
  results,
  analysis_config,
  simulation_config,
  parallelization = FALSE
)
```

## Arguments

- results:

  The results data frame.

- analysis_config:

  The analysis configuration.

## Value

The final results data frame with power and frequentist test columns
added.
