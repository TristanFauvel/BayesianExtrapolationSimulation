# Model_RBesT

An R6 class representing a Bayesian model using RBesT.

## Super class

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\> `Model_RBesT`

## Public fields

- `posterior_summary`:

  Summary of the posterior distribution

## Methods

### Public methods

- [`Model_RBesT$new()`](#method-Model_RBesT-new)

- [`Model_RBesT$prior_pdf()`](#method-Model_RBesT-prior_pdf)

- [`Model_RBesT$prior_cdf()`](#method-Model_RBesT-prior_cdf)

- [`Model_RBesT$posterior_moments()`](#method-Model_RBesT-posterior_moments)

- [`Model_RBesT$posterior_mean()`](#method-Model_RBesT-posterior_mean)

- [`Model_RBesT$posterior_variance()`](#method-Model_RBesT-posterior_variance)

- [`Model_RBesT$posterior_median()`](#method-Model_RBesT-posterior_median)

- [`Model_RBesT$posterior_pdf()`](#method-Model_RBesT-posterior_pdf)

- [`Model_RBesT$posterior_cdf()`](#method-Model_RBesT-posterior_cdf)

- [`Model_RBesT$sample_prior()`](#method-Model_RBesT-sample_prior)

- [`Model_RBesT$sample_posterior()`](#method-Model_RBesT-sample_posterior)

- [`Model_RBesT$prior_to_RBesT()`](#method-Model_RBesT-prior_to_RBesT)

- [`Model_RBesT$posterior_to_RBesT()`](#method-Model_RBesT-posterior_to_RBesT)

- [`Model_RBesT$credible_interval()`](#method-Model_RBesT-credible_interval)

- [`Model_RBesT$clone()`](#method-Model_RBesT-clone)

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

------------------------------------------------------------------------

### Method `new()`

Initializes the GaussianRMP object

#### Usage

    Model_RBesT$new(prior)

#### Arguments

- `prior`:

  The prior information for the analysis.

------------------------------------------------------------------------

### Method `prior_pdf()`

Calculates the prior probability density function (PDF) for a given
target treatment effect.

#### Usage

    Model_RBesT$prior_pdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The prior PDF.

------------------------------------------------------------------------

### Method `prior_cdf()`

Calculates the prior cumulative distribution function (CDF) for a given
target treatment effect.

#### Usage

    Model_RBesT$prior_cdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The prior CDF.

------------------------------------------------------------------------

### Method `posterior_moments()`

Calculates the posterior moments based on the target data.

#### Usage

    Model_RBesT$posterior_moments(target_data)

#### Arguments

- `target_data`:

  The target data for the analysis.

#### Returns

None

------------------------------------------------------------------------

### Method `posterior_mean()`

Calculates the posterior mean.

#### Usage

    Model_RBesT$posterior_mean()

#### Returns

The posterior mean.

------------------------------------------------------------------------

### Method `posterior_variance()`

Calculates the posterior variance.

#### Usage

    Model_RBesT$posterior_variance()

#### Returns

The posterior variance.

------------------------------------------------------------------------

### Method `posterior_median()`

Calculates the posterior median.

#### Usage

    Model_RBesT$posterior_median(...)

#### Arguments

- `...`:

  Additional arguments

#### Returns

The posterior median.

------------------------------------------------------------------------

### Method `posterior_pdf()`

Calculates the posterior probability density function (PDF) for a given
target treatment effect.

#### Usage

    Model_RBesT$posterior_pdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The posterior PDF.

------------------------------------------------------------------------

### Method `posterior_cdf()`

Calculates the posterior cumulative distribution function (CDF) for a
given target treatment effect.

#### Usage

    Model_RBesT$posterior_cdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The posterior CDF.

------------------------------------------------------------------------

### Method `sample_prior()`

Samples from the prior distribution.

#### Usage

    Model_RBesT$sample_prior(n_samples)

#### Arguments

- `n_samples`:

  The number of samples to generate.

#### Returns

The samples from the prior distribution.

------------------------------------------------------------------------

### Method `sample_posterior()`

Samples from the posterior distribution.

#### Usage

    Model_RBesT$sample_posterior(n_samples)

#### Arguments

- `n_samples`:

  The number of samples to generate.

#### Returns

The samples from the posterior distribution.

------------------------------------------------------------------------

### Method `prior_to_RBesT()`

Converts the prior distribution to the RBesT format.

#### Usage

    Model_RBesT$prior_to_RBesT(...)

#### Arguments

- `...`:

  Additional arguments

#### Returns

None

------------------------------------------------------------------------

### Method `posterior_to_RBesT()`

Converts the posterior distribution to the RBesT format.

#### Usage

    Model_RBesT$posterior_to_RBesT(target_data, ...)

#### Arguments

- `target_data`:

  Target study data

- `...`:

  Additional arguments

------------------------------------------------------------------------

### Method `credible_interval()`

Calculates the credible interval.

#### Usage

    Model_RBesT$credible_interval(level = 0.95)

#### Arguments

- `level`:

  Level of the credible interval.

#### Returns

A vector containing the lower and upper bounds of the credible interval.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Model_RBesT$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
