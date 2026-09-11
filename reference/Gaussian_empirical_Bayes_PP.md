# Gaussian_empirical_Bayes_PP class

This is a parent class for variants of empirical Bayes PP methods for
normally distributed summary measure of the treatment effect.

## Format

R6Class object.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::ConjugateGaussian`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.md)
-\>
[`RBExT::StaticBorrowingGaussian`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/StaticBorrowingGaussian.md)
-\> `Gaussian_empirical_Bayes_PP`

## Public fields

- `power_parameter`:

  The power parameter.

- `summary_measure_likelihood`:

  The summary measure distribution.

- `null_space`:

  Null hypothesis space.

- `empirical_bayes`:

  Boolean indicating if empirical Bayes is used.

## Methods

### Public methods

- [`Gaussian_empirical_Bayes_PP$new()`](#method-Gaussian_empirical_Bayes_PP-new)

- [`Gaussian_empirical_Bayes_PP$hypothesis_space_transformation()`](#method-Gaussian_empirical_Bayes_PP-hypothesis_space_transformation)

- [`Gaussian_empirical_Bayes_PP$empirical_bayes_update()`](#method-Gaussian_empirical_Bayes_PP-empirical_bayes_update)

- [`Gaussian_empirical_Bayes_PP$inference()`](#method-Gaussian_empirical_Bayes_PP-inference)

- [`Gaussian_empirical_Bayes_PP$power_parameter_estimation()`](#method-Gaussian_empirical_Bayes_PP-power_parameter_estimation)

- [`Gaussian_empirical_Bayes_PP$prior_pdf()`](#method-Gaussian_empirical_Bayes_PP-prior_pdf)

- [`Gaussian_empirical_Bayes_PP$plot_power_parameter_vs_drift()`](#method-Gaussian_empirical_Bayes_PP-plot_power_parameter_vs_drift)

- [`Gaussian_empirical_Bayes_PP$clone()`](#method-Gaussian_empirical_Bayes_PP-clone)

Inherited methods

- [`RBExT::Model$create()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-create)
- [`RBExT::Model$estimate_bayesian_operating_characteristics()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-estimate_bayesian_operating_characteristics)
- [`RBExT::Model$estimate_frequentist_operating_characteristics()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-estimate_frequentist_operating_characteristics)
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
- [`RBExT::ConjugateGaussian$prior_to_RBesT()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-prior_to_RBesT)
- [`RBExT::ConjugateGaussian$sample_posterior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-sample_posterior)
- [`RBExT::ConjugateGaussian$sample_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.html#method-sample_prior)

------------------------------------------------------------------------

### Method `new()`

Initialize the Gaussian_empirical_Bayes_PP object.

#### Usage

    Gaussian_empirical_Bayes_PP$new(prior, null_space, theta_0)

#### Arguments

- `prior`:

  The prior object.

- `null_space`:

  Null space

- `theta_0`:

  The theta_0 value.

#### Returns

NULL

------------------------------------------------------------------------

### Method `hypothesis_space_transformation()`

Transform the hypothesis space.

#### Usage

    Gaussian_empirical_Bayes_PP$hypothesis_space_transformation(target_data)

#### Arguments

- `target_data`:

  The target data.

#### Returns

A list containing transformed treatment effect estimates.

------------------------------------------------------------------------

### Method `empirical_bayes_update()`

Empirical Bayes update

#### Usage

    Gaussian_empirical_Bayes_PP$empirical_bayes_update(target_data)

#### Arguments

- `target_data`:

  Target study data

#### Returns

NULL Perform inference using the Gaussian_empirical_Bayes_PP method.

------------------------------------------------------------------------

### Method `inference()`

#### Usage

    Gaussian_empirical_Bayes_PP$inference(target_data)

#### Arguments

- `target_data`:

  The target data.

#### Returns

The inference result. Estimate the power parameter.

------------------------------------------------------------------------

### Method `power_parameter_estimation()`

#### Usage

    Gaussian_empirical_Bayes_PP$power_parameter_estimation()

#### Returns

The estimated power parameter.

------------------------------------------------------------------------

### Method `prior_pdf()`

Calculate the prior probability density function (PDF) for a given
target treatment effect.

#### Usage

    Gaussian_empirical_Bayes_PP$prior_pdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The prior PDF.

------------------------------------------------------------------------

### Method `plot_power_parameter_vs_drift()`

Plot the power parameter as a function of drift in treatment effect

#### Usage

    Gaussian_empirical_Bayes_PP$plot_power_parameter_vs_drift(
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

    Gaussian_empirical_Bayes_PP$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
