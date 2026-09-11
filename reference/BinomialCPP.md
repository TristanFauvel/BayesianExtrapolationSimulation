# BinomialCPP Class

A class representing a Binomial Conditional Power Prior model.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::MCMCModel`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.md)
-\> `BinomialCPP`

## Public fields

- `power_parameter`:

  The power parameter for the model

- `method`:

  Method name

- `stan_prior`:

  Stan prior model

- `stan_prior_code`:

  Stan code for the prior

## Methods

### Public methods

- [`BinomialCPP$new()`](#method-BinomialCPP-new)

- [`BinomialCPP$prepare_data()`](#method-BinomialCPP-prepare_data)

- [`BinomialCPP$draw_mcmc_prior()`](#method-BinomialCPP-draw_mcmc_prior)

- [`BinomialCPP$clone()`](#method-BinomialCPP-clone)

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
- [`RBExT::MCMCModel$check_data()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-check_data)
- [`RBExT::MCMCModel$check_mcmc_config()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-check_mcmc_config)
- [`RBExT::MCMCModel$compute_posterior_parameters()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-compute_posterior_parameters)
- [`RBExT::MCMCModel$credible_interval()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-credible_interval)
- [`RBExT::MCMCModel$inference()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-inference)
- [`RBExT::MCMCModel$posterior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_cdf)
- [`RBExT::MCMCModel$posterior_median()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_median)
- [`RBExT::MCMCModel$posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_pdf)
- [`RBExT::MCMCModel$prior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-prior_cdf)
- [`RBExT::MCMCModel$prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-prior_pdf)
- [`RBExT::MCMCModel$sample_posterior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-sample_posterior)
- [`RBExT::MCMCModel$sample_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-sample_prior)

------------------------------------------------------------------------

### Method `new()`

Initialize an instance of the BinomialCPP class.

#### Usage

    BinomialCPP$new(prior, mcmc_config)

#### Arguments

- `prior`:

  The prior object

- `mcmc_config`:

  The MCMC configuration parameters

------------------------------------------------------------------------

### Method `prepare_data()`

Prepare data for the model

#### Usage

    BinomialCPP$prepare_data(target_data)

#### Arguments

- `target_data`:

  The target study data

#### Returns

A list of prepared data

------------------------------------------------------------------------

### Method `draw_mcmc_prior()`

Sample from the prior using Stan

#### Usage

    BinomialCPP$draw_mcmc_prior()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BinomialCPP$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
