# Calculate the probability of false positive

This function calculates the probability of false positive based on the
conditional probability of success, design prior samples, theta_0, and
null space.

## Usage

``` r
preposterior_proba_FP_MC(
  conditional_proba_success,
  design_prior_samples,
  theta_0,
  null_space
)
```

## Arguments

- conditional_proba_success:

  The conditional probability of success.

- design_prior_samples:

  The design prior samples.

- theta_0:

  The value of theta_0.

- null_space:

  The null space (either "left" or "right").

## Value

The probability of false positive.

## Examples

``` r
NA
#> [1] NA
```
