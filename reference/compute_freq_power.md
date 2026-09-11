# Compute the frequentist power

This function computes the power of a test for a given significance
level

This function computes the power of a test for a given significance
level

## Usage

``` r
compute_freq_power(
  alpha,
  target_data,
  frequentist_test,
  theta_0,
  null_space,
  simulation_config,
  case_study = NULL,
  n_replicates = 1000
)
```

## Arguments

- alpha:

  The significance level.

- target_data:

  Target data object

- frequentist_test:

  Type of frequentist test to apply, either z-test or t-test

- theta_0:

  Boundary of the null hypothesis space

- null_space:

  Side of the null space, either left or right.

- simulation_config:

  Simulation configuration.

- case_study:

  Optional case-study name.

- n_replicates:

  Number of Monte Carlo replicates for non-analytical power
  calculations.

## Value

A list containing the power and its confidence interval.
