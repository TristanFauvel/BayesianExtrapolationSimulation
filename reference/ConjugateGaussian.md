# ConjugateGaussian class

This class represents a conjugate Gaussian model (Gaussian prior and
Gaussian likelihood)

## Super class

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\> `ConjugateGaussian`

## Public fields

- `prior_mean`:

  Prior mean

- `prior_var`:

  Prior variance

- `empirical_bayes`:

  Whether the method relies on empirical Bayes or not

- `post_mean`:

  Posterior mean

- `post_var`:

  Posterior variance

- `posterior_parameters`:

  Posterior parameters

## Methods

### Public methods

- [`ConjugateGaussian$new()`](#method-ConjugateGaussian-new)

- [`ConjugateGaussian$sample_prior()`](#method-ConjugateGaussian-sample_prior)

- [`ConjugateGaussian$sample_posterior()`](#method-ConjugateGaussian-sample_posterior)

- [`ConjugateGaussian$prior_pdf()`](#method-ConjugateGaussian-prior_pdf)

- [`ConjugateGaussian$prior_cdf()`](#method-ConjugateGaussian-prior_cdf)

- [`ConjugateGaussian$posterior_cdf()`](#method-ConjugateGaussian-posterior_cdf)

- [`ConjugateGaussian$posterior_pdf()`](#method-ConjugateGaussian-posterior_pdf)

- [`ConjugateGaussian$posterior_mean()`](#method-ConjugateGaussian-posterior_mean)

- [`ConjugateGaussian$posterior_variance()`](#method-ConjugateGaussian-posterior_variance)

- [`ConjugateGaussian$posterior_moments()`](#method-ConjugateGaussian-posterior_moments)

- [`ConjugateGaussian$posterior_median()`](#method-ConjugateGaussian-posterior_median)

- [`ConjugateGaussian$credible_interval()`](#method-ConjugateGaussian-credible_interval)

- [`ConjugateGaussian$prior_to_RBesT()`](#method-ConjugateGaussian-prior_to_RBesT)

- [`ConjugateGaussian$posterior_to_RBesT()`](#method-ConjugateGaussian-posterior_to_RBesT)

- [`ConjugateGaussian$clone()`](#method-ConjugateGaussian-clone)

Inherited methods

- [`RBExT::Model$create()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-create)
- [`RBExT::Model$empirical_bayes_update()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-empirical_bayes_update)
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

------------------------------------------------------------------------

### Method `new()`

Initialize object from the ConjugateGaussian class

#### Usage

    ConjugateGaussian$new(prior)

#### Arguments

- `prior`:

  Prior

------------------------------------------------------------------------

### Method `sample_prior()`

Sample from the prior

#### Usage

    ConjugateGaussian$sample_prior(n_samples)

#### Arguments

- `n_samples`:

  Number of samples from the prior

------------------------------------------------------------------------

### Method `sample_posterior()`

Sample from the posterior

#### Usage

    ConjugateGaussian$sample_posterior(n_samples)

#### Arguments

- `n_samples`:

  Number of samples from the posterior

------------------------------------------------------------------------

### Method `prior_pdf()`

Prior PDF

#### Usage

    ConjugateGaussian$prior_pdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  Point at which to evaluate the prior PDF

------------------------------------------------------------------------

### Method `prior_cdf()`

Prior CDF

#### Usage

    ConjugateGaussian$prior_cdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  Point at which to evaluate the prior CDF

------------------------------------------------------------------------

### Method `posterior_cdf()`

Posterior CDF

#### Usage

    ConjugateGaussian$posterior_cdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  Point at which to evaluate the posterior CDF

------------------------------------------------------------------------

### Method `posterior_pdf()`

Posterior PDF

#### Usage

    ConjugateGaussian$posterior_pdf(target_treatment_effect)

#### Arguments

- `target_treatment_effect`:

  Point at which to evaluate the posterior PDF

------------------------------------------------------------------------

### Method `posterior_mean()`

Posterior mean

#### Usage

    ConjugateGaussian$posterior_mean(target_data)

#### Arguments

- `target_data`:

  Target study data

------------------------------------------------------------------------

### Method `posterior_variance()`

Posterior variance

#### Usage

    ConjugateGaussian$posterior_variance(target_data)

#### Arguments

- `target_data`:

  Target study data

------------------------------------------------------------------------

### Method `posterior_moments()`

Posterior moments

#### Usage

    ConjugateGaussian$posterior_moments(target_data)

#### Arguments

- `target_data`:

  Target study data

------------------------------------------------------------------------

### Method `posterior_median()`

Posterior median

#### Usage

    ConjugateGaussian$posterior_median(...)

#### Arguments

- `...`:

  Additional argument

#### Returns

The posterior median

------------------------------------------------------------------------

### Method `credible_interval()`

Credible interval

#### Usage

    ConjugateGaussian$credible_interval(level = 0.95)

#### Arguments

- `level`:

  Level of the credible interval

------------------------------------------------------------------------

### Method `prior_to_RBesT()`

Convert the prior to RBesT format

#### Usage

    ConjugateGaussian$prior_to_RBesT(...)

#### Arguments

- `...`:

  Additional arguments

------------------------------------------------------------------------

### Method `posterior_to_RBesT()`

Convert the posterior distribution to RBesT format

#### Usage

    ConjugateGaussian$posterior_to_RBesT(target_data, ...)

#### Arguments

- `target_data`:

  Target study data

- `...`:

  Additional arguments

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConjugateGaussian$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
