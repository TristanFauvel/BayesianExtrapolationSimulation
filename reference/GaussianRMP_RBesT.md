# GaussianRMP_RBesT class

A class for Gaussian Robust Mixture Prior models using RBesT for
inference.

## Details

This class represents a Gaussian Robust Mixture Prior model. It inherits
from the Model class. Inference is performed using RBesT.

This class extends the Model_RBesT class and provides methods for
initializing the model, updating priors, calculating posterior moments,
and converting distributions to RBesT format.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::Model_RBesT`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.md)
-\> `GaussianRMP_RBesT`

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

  Method name

## Methods

### Public methods

- [`GaussianRMP_RBesT$new()`](#method-GaussianRMP_RBesT-new)

- [`GaussianRMP_RBesT$empirical_bayes_update()`](#method-GaussianRMP_RBesT-empirical_bayes_update)

- [`GaussianRMP_RBesT$posterior_moments()`](#method-GaussianRMP_RBesT-posterior_moments)

- [`GaussianRMP_RBesT$prior_to_RBesT()`](#method-GaussianRMP_RBesT-prior_to_RBesT)

- [`GaussianRMP_RBesT$posterior_to_RBesT()`](#method-GaussianRMP_RBesT-posterior_to_RBesT)

- [`GaussianRMP_RBesT$print_model_summary()`](#method-GaussianRMP_RBesT-print_model_summary)

- [`GaussianRMP_RBesT$clone()`](#method-GaussianRMP_RBesT-clone)

Inherited methods

- [`RBExT::Model$create()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-create)
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
- [`RBExT::Model_RBesT$credible_interval()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-credible_interval)
- [`RBExT::Model_RBesT$posterior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_cdf)
- [`RBExT::Model_RBesT$posterior_mean()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_mean)
- [`RBExT::Model_RBesT$posterior_median()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_median)
- [`RBExT::Model_RBesT$posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_pdf)
- [`RBExT::Model_RBesT$posterior_variance()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-posterior_variance)
- [`RBExT::Model_RBesT$prior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-prior_cdf)
- [`RBExT::Model_RBesT$prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-prior_pdf)
- [`RBExT::Model_RBesT$sample_posterior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-sample_posterior)
- [`RBExT::Model_RBesT$sample_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model_RBesT.html#method-sample_prior)

------------------------------------------------------------------------

### Method `new()`

Initialize a new GaussianRMP_RBesT object.

#### Usage

    GaussianRMP_RBesT$new(prior)

#### Arguments

- `prior`:

  A list containing prior information for the analysis.

------------------------------------------------------------------------

### Method `empirical_bayes_update()`

Update the vague prior variance based on empirical Bayes approach.

#### Usage

    GaussianRMP_RBesT$empirical_bayes_update(target_data)

#### Arguments

- `target_data`:

  A list containing the target data for the analysis.

------------------------------------------------------------------------

### Method `posterior_moments()`

Calculate the posterior moments based on the target data.

#### Usage

    GaussianRMP_RBesT$posterior_moments(target_data)

#### Arguments

- `target_data`:

  A list containing the target data for the analysis.

------------------------------------------------------------------------

### Method `prior_to_RBesT()`

Convert the prior distribution to the RBesT format.

#### Usage

    GaussianRMP_RBesT$prior_to_RBesT(...)

#### Arguments

- `...`:

  Additional arguments.

------------------------------------------------------------------------

### Method `posterior_to_RBesT()`

Convert the posterior distribution to the RBesT format.

#### Usage

    GaussianRMP_RBesT$posterior_to_RBesT(target_data, ...)

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

    GaussianRMP_RBesT$print_model_summary()

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

    GaussianRMP_RBesT$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
