# TruncatedGaussianRMP class

This class represents a Bayesian borrowing model with a truncated
Gaussian prior. It inherits from the MCMCModel class.

## Value

An R6 class object representing a TruncatedGaussianRMP model

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::MCMCModel`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.md)
-\> `TruncatedGaussianRMP`

## Public fields

- `w`:

  The weight parameter for the mixture prior

- `vague_prior_mean`:

  The mean of the vague prior distribution

- `vague_posterior_mean`:

  The mean of the vague posterior distribution

- `vague_prior_variance`:

  The variance of the vague prior distribution

- `vague_posterior_variance`:

  The variance of the vague posterior distribution

- `info_prior_mean`:

  The mean of the informative prior distribution

- `info_posterior_mean`:

  The mean of the informative posterior distribution

- `info_prior_variance`:

  The variance of the informative prior distribution

- `info_posterior_variance`:

  The variance of the informative posterior distribution

- `wpost`:

  The posterior weight parameter for the mixture prior

- `method`:

  Method name

## Methods

### Public methods

- [`TruncatedGaussianRMP$new()`](#method-TruncatedGaussianRMP-new)

- [`TruncatedGaussianRMP$empirical_bayes_update()`](#method-TruncatedGaussianRMP-empirical_bayes_update)

- [`TruncatedGaussianRMP$prepare_data()`](#method-TruncatedGaussianRMP-prepare_data)

- [`TruncatedGaussianRMP$sample_prior()`](#method-TruncatedGaussianRMP-sample_prior)

- [`TruncatedGaussianRMP$inference()`](#method-TruncatedGaussianRMP-inference)

- [`TruncatedGaussianRMP$print_model_summary()`](#method-TruncatedGaussianRMP-print_model_summary)

- [`TruncatedGaussianRMP$clone()`](#method-TruncatedGaussianRMP-clone)

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
- [`RBExT::MCMCModel$draw_mcmc_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-draw_mcmc_prior)
- [`RBExT::MCMCModel$posterior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_cdf)
- [`RBExT::MCMCModel$posterior_median()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_median)
- [`RBExT::MCMCModel$posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_pdf)
- [`RBExT::MCMCModel$prior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-prior_cdf)
- [`RBExT::MCMCModel$prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-prior_pdf)
- [`RBExT::MCMCModel$sample_posterior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-sample_posterior)

------------------------------------------------------------------------

### Method `new()`

Initialize the TruncatedGaussianRMP object

#### Usage

    TruncatedGaussianRMP$new(prior, mcmc_config)

#### Arguments

- `prior`:

  The prior object

- `mcmc_config`:

  The MCMC configuration parameters

------------------------------------------------------------------------

### Method `empirical_bayes_update()`

Update the empirical Bayes parameters

#### Usage

    TruncatedGaussianRMP$empirical_bayes_update(target_data)

#### Arguments

- `target_data`:

  The target data for inference

------------------------------------------------------------------------

### Method `prepare_data()`

Prepare the data for use with Stan

#### Usage

    TruncatedGaussianRMP$prepare_data(target_data)

#### Arguments

- `target_data`:

  The target data for inference

#### Returns

A list of data prepared for Stan

------------------------------------------------------------------------

### Method `sample_prior()`

Sample from the prior distribution

#### Usage

    TruncatedGaussianRMP$sample_prior(n_samples)

#### Arguments

- `n_samples`:

  Number of samples to generate

#### Returns

A vector of samples from the prior distribution

------------------------------------------------------------------------

### Method `inference()`

Inference

#### Usage

    TruncatedGaussianRMP$inference(target_data)

#### Arguments

- `target_data`:

  Target data object

#### Returns

Indicator whether inference succeeded or not

------------------------------------------------------------------------

### Method `print_model_summary()`

Print a summary of the model attributes

This method creates and prints a formatted table of key model
attributes.

#### Usage

    TruncatedGaussianRMP$print_model_summary()

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

    TruncatedGaussianRMP$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
