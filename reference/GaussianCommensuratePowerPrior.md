# GaussianCommensuratePowerPrior class

This class represents a Gaussian Commensurate Power Prior model.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::MCMCModel`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.md)
-\> `GaussianCommensuratePowerPrior`

## Public fields

- `method`:

  Method name

- `heterogeneity_prior_family`:

  Heterogeneity prior family (half_normal, inverse_gamma)

## Methods

### Public methods

- [`GaussianCommensuratePowerPrior$new()`](#method-GaussianCommensuratePowerPrior-new)

- [`GaussianCommensuratePowerPrior$prepare_data()`](#method-GaussianCommensuratePowerPrior-prepare_data)

- [`GaussianCommensuratePowerPrior$compute_posterior_parameters()`](#method-GaussianCommensuratePowerPrior-compute_posterior_parameters)

- [`GaussianCommensuratePowerPrior$sample_prior()`](#method-GaussianCommensuratePowerPrior-sample_prior)

- [`GaussianCommensuratePowerPrior$joint_prior_pdf()`](#method-GaussianCommensuratePowerPrior-joint_prior_pdf)

- [`GaussianCommensuratePowerPrior$unnormalized_prior_pdf()`](#method-GaussianCommensuratePowerPrior-unnormalized_prior_pdf)

- [`GaussianCommensuratePowerPrior$prior_pdf()`](#method-GaussianCommensuratePowerPrior-prior_pdf)

- [`GaussianCommensuratePowerPrior$clone()`](#method-GaussianCommensuratePowerPrior-clone)

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
- [`RBExT::MCMCModel$credible_interval()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-credible_interval)
- [`RBExT::MCMCModel$draw_mcmc_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-draw_mcmc_prior)
- [`RBExT::MCMCModel$inference()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-inference)
- [`RBExT::MCMCModel$posterior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_cdf)
- [`RBExT::MCMCModel$posterior_median()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_median)
- [`RBExT::MCMCModel$posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-posterior_pdf)
- [`RBExT::MCMCModel$prior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-prior_cdf)
- [`RBExT::MCMCModel$sample_posterior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/MCMCModel.html#method-sample_posterior)

------------------------------------------------------------------------

### Method `new()`

Initialize the GaussianCommensuratePowerPrior object

#### Usage

    GaussianCommensuratePowerPrior$new(prior, mcmc_config)

#### Arguments

- `prior`:

  The prior object

- `mcmc_config`:

  The MCMC configuration parameters

------------------------------------------------------------------------

### Method `prepare_data()`

Prepare the data to be used by CmdStanR

#### Usage

    GaussianCommensuratePowerPrior$prepare_data(target_data)

#### Arguments

- `target_data`:

  Target study data

------------------------------------------------------------------------

### Method `compute_posterior_parameters()`

Compute posterior parameters

#### Usage

    GaussianCommensuratePowerPrior$compute_posterior_parameters()

------------------------------------------------------------------------

### Method `sample_prior()`

Draw samples from the prior distribution. Based on equation 8 in Hobbs
et al (2011).

#### Usage

    GaussianCommensuratePowerPrior$sample_prior(n_samples)

#### Arguments

- `n_samples`:

  Number of samples

------------------------------------------------------------------------

### Method `joint_prior_pdf()`

Joint prior p.d.f. Based on equation (8) in Hobbs et al (2011).

#### Usage

    GaussianCommensuratePowerPrior$joint_prior_pdf(treatment_effect, gamma, tau)

#### Arguments

- `treatment_effect`:

  Treatment effect

- `gamma`:

  Power parameter

- `tau`:

  Heterogeneity parameter

------------------------------------------------------------------------

### Method `unnormalized_prior_pdf()`

Integrate out gamma and tau to get the marginal PDF for treatment_effect

#### Usage

    GaussianCommensuratePowerPrior$unnormalized_prior_pdf(treatment_effect)

#### Arguments

- `treatment_effect`:

  Treatment effect

------------------------------------------------------------------------

### Method `prior_pdf()`

Prior p.d.f. of the treatment effect

#### Usage

    GaussianCommensuratePowerPrior$prior_pdf(treatment_effect)

#### Arguments

- `treatment_effect`:

  Treatment effect

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GaussianCommensuratePowerPrior$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
