# SeparateGaussian class

This class represents a model with separate Gaussian components,
inheriting from `StaticBorrowingGaussian`. The power parameter is set to
0.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::ConjugateGaussian`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.md)
-\>
[`RBExT::StaticBorrowingGaussian`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/StaticBorrowingGaussian.md)
-\> `SeparateGaussian`

## Public fields

- `method`:

  Method name

## Methods

### Public methods

- [`SeparateGaussian$new()`](#method-SeparateGaussian-new)

- [`SeparateGaussian$clone()`](#method-SeparateGaussian-clone)

Inherited methods

- [`RBExT::Model$create()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-create)
- [`RBExT::Model$empirical_bayes_update()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-empirical_bayes_update)
- [`RBExT::Model$estimate_bayesian_operating_characteristics()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-estimate_bayesian_operating_characteristics)
- [`RBExT::Model$estimate_frequentist_operating_characteristics()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-estimate_frequentist_operating_characteristics)
- [`RBExT::Model$inference()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-inference)
- [`RBExT::Model$plot_pdfs()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-plot_pdfs)
- [`RBExT::Model$plot_posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-plot_posterior_pdf)
- [`RBExT::Model$plot_prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-plot_prior_pdf)
- [`RBExT::Model$posterior_quantile()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-posterior_quantile)
- [`RBExT::Model$prior_ESS()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_ESS)
- [`RBExT::Model$prior_treatment_benefit()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_treatment_benefit)
- [`RBExT::Model$simulation_for_given_treatment_effect()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-simulation_for_given_treatment_effect)
- [`RBExT::Model$test_decision()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-test_decision)
- [`RBExT::ConjugateGaussian$credible_interval()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-credible_interval)
- [`RBExT::ConjugateGaussian$posterior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-posterior_cdf)
- [`RBExT::ConjugateGaussian$posterior_mean()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-posterior_mean)
- [`RBExT::ConjugateGaussian$posterior_median()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-posterior_median)
- [`RBExT::ConjugateGaussian$posterior_moments()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-posterior_moments)
- [`RBExT::ConjugateGaussian$posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-posterior_pdf)
- [`RBExT::ConjugateGaussian$posterior_to_RBesT()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-posterior_to_RBesT)
- [`RBExT::ConjugateGaussian$posterior_variance()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-posterior_variance)
- [`RBExT::ConjugateGaussian$prior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-prior_cdf)
- [`RBExT::ConjugateGaussian$prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-prior_pdf)
- [`RBExT::ConjugateGaussian$prior_to_RBesT()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-prior_to_RBesT)
- [`RBExT::ConjugateGaussian$sample_posterior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-sample_posterior)
- [`RBExT::ConjugateGaussian$sample_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-sample_prior)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    SeparateGaussian$new(prior)

#### Arguments

- `prior`:

  Prior

#### Returns

A Model object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SeparateGaussian$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
NA
#> [1] NA
```
