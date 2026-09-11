# Gaussian_NPP class

This class represents a Gaussian model using the NPP (Noninformative
Power Prior) approach. It inherits from the Model class.

## Super class

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\> `Gaussian_NPP`

## Public fields

- `power_parameter_mean`:

  The mean of the prior on the power parameter.

- `power_parameter_std`:

  The standard deviation of the prior on the power parameter.

- `target_treatment_effect_estimate`:

  Estimate of the treatment effect in the target study

- `target_treatment_effect_standard_error`:

  Standard error on the estimate of the treatment effect in the target
  study

- `p`:

  Parameter of the initial Beta prior on the power parameter

- `q`:

  Parameter of the initial Beta prior on the power parameter

- `prior_normconst`:

  Normalization constant of the prior

- `posterior_normconst`:

  Normalization constant of the posterior

- `max_posterior_pdf`:

  Maximum value of the posterior p.d.f., used for rejection sampling of
  the posterior p.d.f.

- `method`:

  Name of the method

- `summary_measure_likelihood`:

  Summary measure likelihood

## Methods

### Public methods

- [`Gaussian_NPP$new()`](#method-Gaussian_NPP-new)

- [`Gaussian_NPP$unnormalized_posterior_power_parameter_pdf()`](#method-Gaussian_NPP-unnormalized_posterior_power_parameter_pdf)

- [`Gaussian_NPP$power_parameter_posterior_pdf()`](#method-Gaussian_NPP-power_parameter_posterior_pdf)

- [`Gaussian_NPP$normalizing_constant_power_parameter()`](#method-Gaussian_NPP-normalizing_constant_power_parameter)

- [`Gaussian_NPP$inference()`](#method-Gaussian_NPP-inference)

- [`Gaussian_NPP$credible_interval()`](#method-Gaussian_NPP-credible_interval)

- [`Gaussian_NPP$posterior_median()`](#method-Gaussian_NPP-posterior_median)

- [`Gaussian_NPP$sample_posterior()`](#method-Gaussian_NPP-sample_posterior)

- [`Gaussian_NPP$sample_prior()`](#method-Gaussian_NPP-sample_prior)

- [`Gaussian_NPP$posterior_cdf()`](#method-Gaussian_NPP-posterior_cdf)

- [`Gaussian_NPP$posterior_pdf()`](#method-Gaussian_NPP-posterior_pdf)

- [`Gaussian_NPP$prior_pdf()`](#method-Gaussian_NPP-prior_pdf)

- [`Gaussian_NPP$plot_power_parameter_posterior_pdf()`](#method-Gaussian_NPP-plot_power_parameter_posterior_pdf)

- [`Gaussian_NPP$plot_power_parameter_vs_drift()`](#method-Gaussian_NPP-plot_power_parameter_vs_drift)

- [`Gaussian_NPP$clone()`](#method-Gaussian_NPP-clone)

Inherited methods

- [`RBExT::Model$create()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-create)
- [`RBExT::Model$empirical_bayes_update()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-empirical_bayes_update)
- [`RBExT::Model$estimate_bayesian_operating_characteristics()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-estimate_bayesian_operating_characteristics)
- [`RBExT::Model$estimate_frequentist_operating_characteristics()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-estimate_frequentist_operating_characteristics)
- [`RBExT::Model$plot_pdfs()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-plot_pdfs)
- [`RBExT::Model$plot_posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-plot_posterior_pdf)
- [`RBExT::Model$plot_prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-plot_prior_pdf)
- [`RBExT::Model$posterior_mean()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-posterior_mean)
- [`RBExT::Model$posterior_moments()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-posterior_moments)
- [`RBExT::Model$posterior_quantile()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-posterior_quantile)
- [`RBExT::Model$posterior_to_RBesT()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-posterior_to_RBesT)
- [`RBExT::Model$prior_ESS()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_ESS)
- [`RBExT::Model$prior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_cdf)
- [`RBExT::Model$prior_to_RBesT()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_to_RBesT)
- [`RBExT::Model$prior_treatment_benefit()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_treatment_benefit)
- [`RBExT::Model$simulation_for_given_treatment_effect()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-simulation_for_given_treatment_effect)
- [`RBExT::Model$test_decision()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-test_decision)

------------------------------------------------------------------------

### Method `new()`

Initialize a new Gaussian_NPP object.

#### Usage

    Gaussian_NPP$new(prior)

#### Arguments

- `prior`:

  Prior object containing method parameters.

#### Returns

A new Gaussian_NPP object.

------------------------------------------------------------------------

### Method `unnormalized_posterior_power_parameter_pdf()`

Calculate the unnormalized posterior power parameter PDF.

#### Usage

    Gaussian_NPP$unnormalized_posterior_power_parameter_pdf(
      power_parameter,
      target_data
    )

#### Arguments

- `power_parameter`:

  Power parameter value.

- `target_data`:

  Target study data.

#### Returns

Unnormalized posterior power parameter PDF value.

------------------------------------------------------------------------

### Method `power_parameter_posterior_pdf()`

Return the posterior distribution of the power parameter

#### Usage

    Gaussian_NPP$power_parameter_posterior_pdf(power_parameter, target_data)

#### Arguments

- `power_parameter`:

  Power parameter value.

- `target_data`:

  Target study data.

#### Returns

Posterior power parameter PDF value.

------------------------------------------------------------------------

### Method `normalizing_constant_power_parameter()`

Calculate the normalizing constant for the power parameter.

#### Usage

    Gaussian_NPP$normalizing_constant_power_parameter(target_data)

#### Arguments

- `target_data`:

  Target study data.

#### Returns

Normalizing constant value.

------------------------------------------------------------------------

### Method `inference()`

Perform inference on the target data.

#### Usage

    Gaussian_NPP$inference(target_data)

#### Arguments

- `target_data`:

  Target study data.

#### Returns

A string indicating the success status of the inference.

------------------------------------------------------------------------

### Method `credible_interval()`

Calculate the credible interval.

#### Usage

    Gaussian_NPP$credible_interval(level = 0.95)

#### Arguments

- `level`:

  Credible interval level (default: 0.95).

#### Returns

A vector containing the lower and upper bounds of the credible interval.

------------------------------------------------------------------------

### Method `posterior_median()`

Return the median of the posterior distribution.

#### Usage

    Gaussian_NPP$posterior_median(...)

#### Arguments

- `...`:

  Optional arguments

#### Returns

Median of the posterior distribution.

------------------------------------------------------------------------

### Method `sample_posterior()`

Sample from the posterior distribution.

#### Usage

    Gaussian_NPP$sample_posterior(n_samples)

#### Arguments

- `n_samples`:

  Number of samples to draw from the posterior distribution.

#### Returns

A vector of samples from the posterior distribution.

------------------------------------------------------------------------

### Method `sample_prior()`

Sample from the prior distribution.

#### Usage

    Gaussian_NPP$sample_prior(n_samples)

#### Arguments

- `n_samples`:

  Number of samples to draw from the prior distribution.

#### Returns

A vector of samples from the prior distribution.

------------------------------------------------------------------------

### Method `posterior_cdf()`

Calculate the posterior cumulative distribution function (CDF).

#### Usage

    Gaussian_NPP$posterior_cdf(x)

#### Arguments

- `x`:

  Vector of points to evaluate the CDF at.

#### Returns

Vector of CDF values corresponding to the input points.

------------------------------------------------------------------------

### Method `posterior_pdf()`

Calculate the posterior probability density function (PDF).

#### Usage

    Gaussian_NPP$posterior_pdf(x)

#### Arguments

- `x`:

  Vector of points to evaluate the PDF at.

#### Returns

Vector of PDF values corresponding to the input points.

------------------------------------------------------------------------

### Method `prior_pdf()`

Calculate the prior probability density function (PDF).

#### Usage

    Gaussian_NPP$prior_pdf(x)

#### Arguments

- `x`:

  Vector of points to evaluate the PDF at.

#### Returns

Vector of PDF values corresponding to the input points.

------------------------------------------------------------------------

### Method `plot_power_parameter_posterior_pdf()`

Plot posterior probability density function (PDF) of the power parameter

#### Usage

    Gaussian_NPP$plot_power_parameter_posterior_pdf(target_data)

#### Arguments

- `target_data`:

  Target study data

#### Returns

A plot

------------------------------------------------------------------------

### Method `plot_power_parameter_vs_drift()`

Plot the power parameter as a function of drift in treatment effect

#### Usage

    Gaussian_NPP$plot_power_parameter_vs_drift(
      source_treatment_effect_estimate,
      target_data,
      min_drift,
      max_drift,
      resolution
    )

#### Arguments

- `source_treatment_effect_estimate`:

  Treatment effect estimate in the source study

- `target_data`:

  Target study data

- `min_drift`:

  Minimum drift value

- `max_drift`:

  Maximum drift value

- `resolution`:

  Number of points on the drift grid.

#### Returns

The prior PDF.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Gaussian_NPP$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
