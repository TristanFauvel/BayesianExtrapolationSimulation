# Calculate the prior moment-based Effective Sample Size (ESS) for a Bayesian model.

This function calculates the prior moment-based ESS for a Bayesian model
using the RBesT package. It takes a RBesT model object and target data
as input and returns the prior moment-based ESS.

## Usage

``` r
prior_moment_ess(rbest_model, target_data)
```

## Arguments

- rbest_model:

  A RBesT model object.

- target_data:

  Target data containing sample information.

## Value

The prior moment-based ESS for the Bayesian model.
