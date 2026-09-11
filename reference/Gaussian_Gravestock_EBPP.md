# Gaussian_Gravestock_EBPP class

This class inherits from Gaussian_empirical_Bayes_PP and implements the
Gravestock's EBPP method.

## Format

R6Class object.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::ConjugateGaussian`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.md)
-\>
[`RBExT::StaticBorrowingGaussian`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/StaticBorrowingGaussian.md)
-\>
[`RBExT::Gaussian_empirical_Bayes_PP`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Gaussian_empirical_Bayes_PP.md)
-\> ` Gaussian_gravestock_EBPP`

## Public fields

- `method`:

  Method name

## Methods

### Public methods

- [`Gaussian_Gravestock_EBPP$new()`](#method-%20Gaussian_gravestock_EBPP-new)

- [`Gaussian_Gravestock_EBPP$power_parameter_estimation()`](#method-%20Gaussian_gravestock_EBPP-power_parameter_estimation)

- [`Gaussian_Gravestock_EBPP$clone()`](#method-%20Gaussian_gravestock_EBPP-clone)

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
- [`RBExT::Gaussian_empirical_Bayes_PP$empirical_bayes_update()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Gaussian_empirical_Bayes_PP.html#method-empirical_bayes_update)
- [`RBExT::Gaussian_empirical_Bayes_PP$hypothesis_space_transformation()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Gaussian_empirical_Bayes_PP.html#method-hypothesis_space_transformation)
- [`RBExT::Gaussian_empirical_Bayes_PP$inference()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Gaussian_empirical_Bayes_PP.html#method-inference)
- [`RBExT::Gaussian_empirical_Bayes_PP$plot_power_parameter_vs_drift()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Gaussian_empirical_Bayes_PP.html#method-plot_power_parameter_vs_drift)
- [`RBExT::Gaussian_empirical_Bayes_PP$prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Gaussian_empirical_Bayes_PP.html#method-prior_pdf)

------------------------------------------------------------------------

### Method `new()`

Initialize the Gaussian_Gravestock_EBPP object.

#### Usage

    Gaussian_Gravestock_EBPP$new(prior, null_space, theta_0)

#### Arguments

- `prior`:

  The prior object.

- `null_space`:

  Side of the null hypothesis space

- `theta_0`:

  Boundary of the null hypothesis space

#### Returns

NULL

------------------------------------------------------------------------

### Method `power_parameter_estimation()`

Estimate the power parameter using the Gravestock's EBPP method.

#### Usage

    Gaussian_Gravestock_EBPP$power_parameter_estimation(target_data)

#### Arguments

- `target_data`:

  The target data.

- `source_treatment_effect_estimate`:

  Treatment effect estimate in the source study

- `target_treatment_effect_estimate`:

  Treatment effect estimate in the target study

#### Returns

The estimated power parameter.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Gaussian_Gravestock_EBPP$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
