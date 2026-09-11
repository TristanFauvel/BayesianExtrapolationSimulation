# Gaussian_empirical_Bayes_PP class

This is a parent class for variants of empirical Bayes PP methods for
normally distributed summary measure of the treatment effect.

## Format

R6Class object.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::MCMCModel`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.md)
-\>
[`RBExT::BinomialCPP`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/BinomialCPP.md)
-\> `p_value_based_PP_Binomial`

## Public fields

- `power_parameter`:

  The power parameter.

- `summary_measure_likelihood`:

  The summary measure distribution.

- `null_space`:

  Null hypothesis space.

- `empirical_bayes`:

  Boolean indicating if empirical Bayes is used.

- `shape_parameter`:

  Shape parameter

- `equivalence_margin`:

  Equivalence margin for the test

- `method`:

  Method name

- `prior_var`:

  Prior variance

- `mcmc_config`:

  MCMC configuration

## Methods

### Public methods

- [`p_value_based_PP_Binomial$new()`](#method-p_value_based_PP_Binomial-new)

- [`p_value_based_PP_Binomial$hypothesis_space_transformation()`](#method-p_value_based_PP_Binomial-hypothesis_space_transformation)

- [`p_value_based_PP_Binomial$empirical_bayes_update()`](#method-p_value_based_PP_Binomial-empirical_bayes_update)

- [`p_value_based_PP_Binomial$inference()`](#method-p_value_based_PP_Binomial-inference)

- [`p_value_based_PP_Binomial$test()`](#method-p_value_based_PP_Binomial-test)

- [`p_value_based_PP_Binomial$power_parameter_estimation()`](#method-p_value_based_PP_Binomial-power_parameter_estimation)

- [`p_value_based_PP_Binomial$prior_pdf()`](#method-p_value_based_PP_Binomial-prior_pdf)

- [`p_value_based_PP_Binomial$plot_power_parameter_vs_drift()`](#method-p_value_based_PP_Binomial-plot_power_parameter_vs_drift)

- [`p_value_based_PP_Binomial$clone()`](#method-p_value_based_PP_Binomial-clone)

Inherited methods

- [`RBExT::Model$create()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-create)
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
- [`RBExT::Model$prior_to_RBesT()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_to_RBesT)
- [`RBExT::Model$prior_treatment_benefit()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_treatment_benefit)
- [`RBExT::Model$simulation_for_given_treatment_effect()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-simulation_for_given_treatment_effect)
- [`RBExT::Model$test_decision()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-test_decision)
- [`RBExT::MCMCModel$check_data()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-check_data)
- [`RBExT::MCMCModel$check_mcmc_config()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-check_mcmc_config)
- [`RBExT::MCMCModel$compute_posterior_parameters()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-compute_posterior_parameters)
- [`RBExT::MCMCModel$credible_interval()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-credible_interval)
- [`RBExT::MCMCModel$posterior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_cdf)
- [`RBExT::MCMCModel$posterior_median()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_median)
- [`RBExT::MCMCModel$posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_pdf)
- [`RBExT::MCMCModel$prior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-prior_cdf)
- [`RBExT::MCMCModel$sample_posterior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-sample_posterior)
- [`RBExT::MCMCModel$sample_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-sample_prior)
- [`RBExT::BinomialCPP$draw_mcmc_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/BinomialCPP.html#method-draw_mcmc_prior)
- [`RBExT::BinomialCPP$prepare_data()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/BinomialCPP.html#method-prepare_data)

------------------------------------------------------------------------

### Method `new()`

Initialize the p_value_based_PP object.

#### Usage

    p_value_based_PP_Binomial$new(prior, theta_0, null_space, mcmc_config)

#### Arguments

- `prior`:

  The prior object.

- `theta_0`:

  Boundary of the null hypothesis space

- `null_space`:

  Null space.

- `mcmc_config`:

  MCMC configuration.

#### Returns

None

------------------------------------------------------------------------

### Method `hypothesis_space_transformation()`

Transform the hypothesis space.

#### Usage

    p_value_based_PP_Binomial$hypothesis_space_transformation(target_data)

#### Arguments

- `target_data`:

  The target data.

#### Returns

A list containing transformed treatment effect estimates.

------------------------------------------------------------------------

### Method `empirical_bayes_update()`

Empirical Bayes update

#### Usage

    p_value_based_PP_Binomial$empirical_bayes_update(target_data)

#### Arguments

- `target_data`:

  Target study data

#### Returns

NULL Perform inference using the Gaussian_empirical_Bayes_PP method.

------------------------------------------------------------------------

### Method `inference()`

#### Usage

    p_value_based_PP_Binomial$inference(target_data)

#### Arguments

- `target_data`:

  The target data.

#### Returns

The inference result. Test method

------------------------------------------------------------------------

### Method `test()`

This method performs the test for the given target data.

#### Usage

    p_value_based_PP_Binomial$test(
      target_data,
      source_treatment_effect_estimate,
      target_treatment_effect_estimate
    )

#### Arguments

- `target_data`:

  The target data object.

- `source_treatment_effect_estimate`:

  The source treatment effect estimate.

- `target_treatment_effect_estimate`:

  The target treatment effect estimate.

#### Returns

The p-value. Power parameter estimation method

------------------------------------------------------------------------

### Method `power_parameter_estimation()`

This method estimates the power parameter for the given target data.

#### Usage

    p_value_based_PP_Binomial$power_parameter_estimation(target_data)

#### Arguments

- `target_data`:

  The target data object.

- `source_treatment_effect_estimate`:

  The source treatment effect estimate.

- `target_treatment_effect_estimate`:

  The target treatment effect estimate.

#### Returns

The power parameter.

------------------------------------------------------------------------

### Method `prior_pdf()`

Calculate the prior probability density function (PDF) for a given
target treatment effect.

#### Usage

    p_value_based_PP_Binomial$prior_pdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The prior PDF.

------------------------------------------------------------------------

### Method `plot_power_parameter_vs_drift()`

Plot the power parameter as a function of drift in treatment effect

#### Usage

    p_value_based_PP_Binomial$plot_power_parameter_vs_drift(
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

    p_value_based_PP_Binomial$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
