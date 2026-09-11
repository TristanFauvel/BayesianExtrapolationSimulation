# GaussianRMP class

A class for Gaussian Robust Mixture Prior models.

## Details

This class represents a Gaussian Robust Mixture Prior model. It inherits
from the Model class.

This class extends the Model class and provides methods for initializing
the model, updating priors, calculating posterior moments, and sampling
from prior and posterior distributions.

## Super class

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\> `GaussianRMP`

## Public fields

- `w`:

  The weight of the prior distribution.

- `vague_prior_mean`:

  The mean of the vague prior distribution.

- `vague_prior_variance`:

  The variance of the vague prior distribution.

- `info_prior_mean`:

  The mean of the informative prior distribution.

- `info_prior_variance`:

  The variance of the informative prior distribution.

- `wpost`:

  The weight of the posterior distribution.

- `vague_posterior_mean`:

  The mean of the vague posterior distribution.

- `info_posterior_mean`:

  The mean of the informative posterior distribution.

- `vague_posterior_variance`:

  The variance of the vague posterior distribution.

- `info_posterior_variance`:

  The variance of the informative posterior distribution.

- `method`:

  Name of the method

## Methods

### Public methods

- [`GaussianRMP$new()`](#method-GaussianRMP-new)

- [`GaussianRMP$empirical_bayes_update()`](#method-GaussianRMP-empirical_bayes_update)

- [`GaussianRMP$prior_weight()`](#method-GaussianRMP-prior_weight)

- [`GaussianRMP$prior_pdf()`](#method-GaussianRMP-prior_pdf)

- [`GaussianRMP$prior_cdf()`](#method-GaussianRMP-prior_cdf)

- [`GaussianRMP$posterior_moments()`](#method-GaussianRMP-posterior_moments)

- [`GaussianRMP$posterior_pdf()`](#method-GaussianRMP-posterior_pdf)

- [`GaussianRMP$posterior_cdf()`](#method-GaussianRMP-posterior_cdf)

- [`GaussianRMP$sample_prior()`](#method-GaussianRMP-sample_prior)

- [`GaussianRMP$sample_posterior()`](#method-GaussianRMP-sample_posterior)

- [`GaussianRMP$posterior_mean()`](#method-GaussianRMP-posterior_mean)

- [`GaussianRMP$posterior_variance()`](#method-GaussianRMP-posterior_variance)

- [`GaussianRMP$prior_to_RBesT()`](#method-GaussianRMP-prior_to_RBesT)

- [`GaussianRMP$posterior_to_RBesT()`](#method-GaussianRMP-posterior_to_RBesT)

- [`GaussianRMP$print_model_summary()`](#method-GaussianRMP-print_model_summary)

- [`GaussianRMP$clone()`](#method-GaussianRMP-clone)

Inherited methods

- [`RBExT::Model$create()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-create)
- [`RBExT::Model$credible_interval()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-credible_interval)
- [`RBExT::Model$estimate_bayesian_operating_characteristics()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-estimate_bayesian_operating_characteristics)
- [`RBExT::Model$estimate_frequentist_operating_characteristics()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-estimate_frequentist_operating_characteristics)
- [`RBExT::Model$inference()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-inference)
- [`RBExT::Model$plot_pdfs()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-plot_pdfs)
- [`RBExT::Model$plot_posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-plot_posterior_pdf)
- [`RBExT::Model$plot_prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-plot_prior_pdf)
- [`RBExT::Model$posterior_median()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-posterior_median)
- [`RBExT::Model$posterior_quantile()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-posterior_quantile)
- [`RBExT::Model$prior_ESS()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_ESS)
- [`RBExT::Model$prior_treatment_benefit()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_treatment_benefit)
- [`RBExT::Model$simulation_for_given_treatment_effect()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-simulation_for_given_treatment_effect)
- [`RBExT::Model$test_decision()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-test_decision)

------------------------------------------------------------------------

### Method `new()`

Initialize a new GaussianRMP object.

#### Usage

    GaussianRMP$new(prior)

#### Arguments

- `prior`:

  A list containing prior information for the analysis.

------------------------------------------------------------------------

### Method `empirical_bayes_update()`

Update the vague prior variance based on empirical Bayes approach.

#### Usage

    GaussianRMP$empirical_bayes_update(target_data)

#### Arguments

- `target_data`:

  The target data for the analysis.

------------------------------------------------------------------------

### Method `prior_weight()`

Calculate the posterior weight based on the target data.

#### Usage

    GaussianRMP$prior_weight(target_data)

#### Arguments

- `target_data`:

  The target data for the analysis.

#### Returns

The posterior weight.

------------------------------------------------------------------------

### Method `prior_pdf()`

Calculate the prior probability density function (PDF) for a given
target treatment effect.

#### Usage

    GaussianRMP$prior_pdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The prior PDF.

------------------------------------------------------------------------

### Method `prior_cdf()`

Calculate the prior cumulative distribution function (CDF) for a given
target treatment effect.

#### Usage

    GaussianRMP$prior_cdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The prior CDF.

------------------------------------------------------------------------

### Method `posterior_moments()`

Calculate the posterior moments based on the target data.

#### Usage

    GaussianRMP$posterior_moments(target_data)

#### Arguments

- `target_data`:

  A list containing the target data for the analysis.

------------------------------------------------------------------------

### Method `posterior_pdf()`

Calculate the posterior probability density function (PDF) for a given
target treatment effect.

#### Usage

    GaussianRMP$posterior_pdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The posterior PDF.

------------------------------------------------------------------------

### Method `posterior_cdf()`

Calculate the posterior cumulative distribution function (CDF) for a
given target treatment effect.

#### Usage

    GaussianRMP$posterior_cdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The posterior CDF.

------------------------------------------------------------------------

### Method `sample_prior()`

Sample from the prior distribution.

#### Usage

    GaussianRMP$sample_prior(n_samples)

#### Arguments

- `n_samples`:

  The number of samples to generate.

#### Returns

The samples from the prior distribution.

------------------------------------------------------------------------

### Method `sample_posterior()`

Sample from the posterior distribution.

#### Usage

    GaussianRMP$sample_posterior(n_samples)

#### Arguments

- `n_samples`:

  The number of samples to generate.

#### Returns

The samples from the posterior distribution.

------------------------------------------------------------------------

### Method `posterior_mean()`

Calculate the posterior mean.

#### Usage

    GaussianRMP$posterior_mean()

#### Returns

The posterior mean.

------------------------------------------------------------------------

### Method `posterior_variance()`

Calculate the posterior variance.

#### Usage

    GaussianRMP$posterior_variance()

#### Returns

The posterior variance.

------------------------------------------------------------------------

### Method `prior_to_RBesT()`

Convert the prior distribution to the RBesT format.

#### Usage

    GaussianRMP$prior_to_RBesT(...)

#### Arguments

- `...`:

  Additional arguments.

------------------------------------------------------------------------

### Method `posterior_to_RBesT()`

Convert the posterior distribution to the RBesT format.

#### Usage

    GaussianRMP$posterior_to_RBesT(target_data, ...)

#### Arguments

- `target_data`:

  Target data for the analysis.

- `...`:

  Additional arguments.

------------------------------------------------------------------------

### Method `print_model_summary()`

Print a summary of the model attributes

This method creates and prints a formatted table of key model
attributes.

#### Usage

    GaussianRMP$print_model_summary()

#### Returns

A printed data frame displaying the following model attributes:

- Posterior Weight

- Vague Posterior Mean

- Vague Posterior Variance

- Informative Posterior Mean

- Informative Posterior Variance

All numeric values are formatted to 6 decimal places.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GaussianRMP$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
