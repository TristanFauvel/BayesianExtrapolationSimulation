# SeparateGaussian_RBesT

An R6 class representing a separate Gaussian model using RBesT.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::Model_RBesT`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.md)
-\> `SeparateGaussian_RBesT`

## Public fields

- `method`:

  Method name

## Methods

### Public methods

- [`SeparateGaussian_RBesT$new()`](#method-SeparateGaussian_RBesT-new)

- [`SeparateGaussian_RBesT$prior_to_RBesT()`](#method-SeparateGaussian_RBesT-prior_to_RBesT)

- [`SeparateGaussian_RBesT$posterior_to_RBesT()`](#method-SeparateGaussian_RBesT-posterior_to_RBesT)

- [`SeparateGaussian_RBesT$clone()`](#method-SeparateGaussian_RBesT-clone)

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
- [`RBExT::Model_RBesT$credible_interval()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-credible_interval)
- [`RBExT::Model_RBesT$posterior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_cdf)
- [`RBExT::Model_RBesT$posterior_mean()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_mean)
- [`RBExT::Model_RBesT$posterior_median()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_median)
- [`RBExT::Model_RBesT$posterior_moments()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_moments)
- [`RBExT::Model_RBesT$posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_pdf)
- [`RBExT::Model_RBesT$posterior_variance()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_variance)
- [`RBExT::Model_RBesT$prior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-prior_cdf)
- [`RBExT::Model_RBesT$prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-prior_pdf)
- [`RBExT::Model_RBesT$sample_posterior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-sample_posterior)
- [`RBExT::Model_RBesT$sample_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-sample_prior)

------------------------------------------------------------------------

### Method `new()`

Initializes the SeparateGaussian_RBesT object.

#### Usage

    SeparateGaussian_RBesT$new(prior)

#### Arguments

- `prior`:

  Prior information for the analysis.

#### Returns

A new SeparateGaussian_RBesT object.

------------------------------------------------------------------------

### Method `prior_to_RBesT()`

Converts the prior distribution to the RBesT format.

#### Usage

    SeparateGaussian_RBesT$prior_to_RBesT(...)

#### Arguments

- `...`:

  Additional arguments.

#### Returns

None

------------------------------------------------------------------------

### Method `posterior_to_RBesT()`

Converts the posterior distribution to the RBesT format.

#### Usage

    SeparateGaussian_RBesT$posterior_to_RBesT(target_data, ...)

#### Arguments

- `target_data`:

  Target study data.

- `...`:

  Additional arguments.

#### Returns

None

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SeparateGaussian_RBesT$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
