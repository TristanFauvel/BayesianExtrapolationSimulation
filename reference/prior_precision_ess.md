# Calculate the prior precision-based Effective Sample Size (ESS) for a Bayesian model.

This function calculates the prior precision-based ESS for a Bayesian
model using the RBesT package. It takes a RBesT model object and target
data as input and returns the prior precision-based ESS.

## Usage

``` r
prior_precision_ess(rbest_model, target_data)
```

## Arguments

- rbest_model:

  A RBesT model object.

- target_data:

  Target data containing sample information.

## Value

The prior precision-based ESS for the Bayesian model.
