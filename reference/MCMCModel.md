# MCMCModel class

This class represents a Bayesian borrowing model using MCMC sampling. It
inherits from the Model class.

## Super class

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\> `MCMCModel`

## Public fields

- `stan_model_code`:

  Code of the Stan model

- `stan_model`:

  The compiled Stan model

- `fit`:

  The MCMC fit object

- `fit_summary`:

  Summary of the Stan fit

- `treatment_effect_summary`:

  Summary statistics of the treatment effect posterior distribution

- `credible_interval_97.5`:

  The upper bound of the credible interval

- `credible_interval_2.5`:

  The lower bound of the credible interval

- `mcmc_config`:

  The MCMC configuration parameters

- `mcmc_ess`:

  MCMC ESS

- `n_divergences`:

  Number of divergences in MCMC inference

- `rhat`:

  r-hat statistics

- `draws_dir`:

  Directory where to store MCMC draws (used by Stan)

- `prior_draws`:

  Draws from the prior

- `prior_pdf_approx`:

  Approximation to the prior probability density function

- `prior_cdf_approx`:

  Approximation to the prior cumulative density function

- `posterior_pdf_approx`:

  Approximation to the posterior probability density function

- `posterior_cdf_approx`:

  Approximation to the posterior cumulative density function

## Methods

### Public methods

- [`MCMCModel$new()`](#method-MCMCModel-new)

- [`MCMCModel$check_mcmc_config()`](#method-MCMCModel-check_mcmc_config)

- [`MCMCModel$prepare_data()`](#method-MCMCModel-prepare_data)

- [`MCMCModel$inference()`](#method-MCMCModel-inference)

- [`MCMCModel$credible_interval()`](#method-MCMCModel-credible_interval)

- [`MCMCModel$posterior_median()`](#method-MCMCModel-posterior_median)

- [`MCMCModel$sample_posterior()`](#method-MCMCModel-sample_posterior)

- [`MCMCModel$check_data()`](#method-MCMCModel-check_data)

- [`MCMCModel$compute_posterior_parameters()`](#method-MCMCModel-compute_posterior_parameters)

- [`MCMCModel$draw_mcmc_prior()`](#method-MCMCModel-draw_mcmc_prior)

- [`MCMCModel$posterior_pdf()`](#method-MCMCModel-posterior_pdf)

- [`MCMCModel$posterior_cdf()`](#method-MCMCModel-posterior_cdf)

- [`MCMCModel$prior_pdf()`](#method-MCMCModel-prior_pdf)

- [`MCMCModel$prior_cdf()`](#method-MCMCModel-prior_cdf)

- [`MCMCModel$sample_prior()`](#method-MCMCModel-sample_prior)

- [`MCMCModel$clone()`](#method-MCMCModel-clone)

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
- [`RBExT::Model$prior_to_RBesT()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_to_RBesT)
- [`RBExT::Model$prior_treatment_benefit()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_treatment_benefit)
- [`RBExT::Model$simulation_for_given_treatment_effect()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-simulation_for_given_treatment_effect)
- [`RBExT::Model$test_decision()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-test_decision)

------------------------------------------------------------------------

### Method `new()`

Initialize the MCMCModel object

#### Usage

    MCMCModel$new(prior, mcmc_config)

#### Arguments

- `prior`:

  The prior object

- `mcmc_config`:

  The MCMC configuration parameters

------------------------------------------------------------------------

### Method `check_mcmc_config()`

Check validity of the MCMC configuration

#### Usage

    MCMCModel$check_mcmc_config(mcmc_config)

#### Arguments

- `mcmc_config`:

  MCMC configuration

------------------------------------------------------------------------

### Method `prepare_data()`

Prepare the data for inference. Subclasses must implement the
'prepare_data' method.

#### Usage

    MCMCModel$prepare_data(target_data)

#### Arguments

- `target_data`:

  The target data for inference

------------------------------------------------------------------------

### Method `inference()`

Perform inference using MCMC sampling

#### Usage

    MCMCModel$inference(target_data)

#### Arguments

- `target_data`:

  The target data for inference

------------------------------------------------------------------------

### Method `credible_interval()`

Calculate the credible interval

#### Usage

    MCMCModel$credible_interval(level = 0.95)

#### Arguments

- `level`:

  The confidence level for the credible interval (default is 0.95)

#### Returns

The credible interval as a numeric vector

------------------------------------------------------------------------

### Method `posterior_median()`

Get the posterior median

#### Usage

    MCMCModel$posterior_median(...)

#### Arguments

- `...`:

  Optional argument

#### Returns

The posterior median as a numeric value

------------------------------------------------------------------------

### Method `sample_posterior()`

Sample from the posterior distribution

#### Usage

    MCMCModel$sample_posterior(n_samples)

#### Arguments

- `n_samples`:

  The number of samples to draw from the posterior distribution

#### Returns

The sampled treatment effect values as a numeric vector

------------------------------------------------------------------------

### Method `check_data()`

Check the validity of the data

#### Usage

    MCMCModel$check_data(data_list)

#### Arguments

- `data_list`:

  The list of data elements

------------------------------------------------------------------------

### Method `compute_posterior_parameters()`

Compute the posterior parameters. If there are posterior borrowing
parameters, the following method must be overriden in the subclass.

#### Usage

    MCMCModel$compute_posterior_parameters()

------------------------------------------------------------------------

### Method `draw_mcmc_prior()`

Draw samples from the prior using MCMC

#### Usage

    MCMCModel$draw_mcmc_prior()

------------------------------------------------------------------------

### Method `posterior_pdf()`

Posterior PDF

#### Usage

    MCMCModel$posterior_pdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  Point at which to evaluate the posterior PDF

------------------------------------------------------------------------

### Method `posterior_cdf()`

Calculates the posterior cumulative distribution function (CDF) for a
given target treatment effect.

#### Usage

    MCMCModel$posterior_cdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  The target treatment effect.

#### Returns

The posterior CDF.

------------------------------------------------------------------------

### Method `prior_pdf()`

Prior PDF

#### Usage

    MCMCModel$prior_pdf(
      target_treatment_effect,
      n_samples_quantile_estimation = 10000
    )

#### Arguments

- `target_treatment_effect`:

  Point at which to evaluate the prior PDF

- `n_samples_quantile_estimation`:

  Number of samples used to estimate the quantiles of the distribution

------------------------------------------------------------------------

### Method `prior_cdf()`

Prior CDF

#### Usage

    MCMCModel$prior_cdf(
      target_treatment_effect,
      n_samples_quantile_estimation = 10000
    )

#### Arguments

- `target_treatment_effect`:

  Point at which to evaluate the prior CDF

- `n_samples_quantile_estimation`:

  Number of samples used to estimate the quantiles of the distribution

------------------------------------------------------------------------

### Method `sample_prior()`

Sample from the prior distribution

#### Usage

    MCMCModel$sample_prior(n_samples)

#### Arguments

- `n_samples`:

  Number of samples to draw

#### Returns

A vector of samples

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MCMCModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
