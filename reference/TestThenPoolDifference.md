# TestThenPoolDifference class

This class extends the TestThenPool class to implement the
Test-Then-Pool framework for difference trials.

## Details

The TestThenPoolDifference class provides methods for testing and
inference in difference trials using the Test-Then-Pool framework.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::TestThenPool`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.md)
-\> `TestThenPoolDifference`

## Public fields

- `significance_level`:

  Significance level for the test

- `method`:

  Method name

## Methods

### Public methods

- [`TestThenPoolDifference$new()`](#method-TestThenPoolDifference-new)

- [`TestThenPoolDifference$test_pvalue()`](#method-TestThenPoolDifference-test_pvalue)

- [`TestThenPoolDifference$test()`](#method-TestThenPoolDifference-test)

- [`TestThenPoolDifference$clone()`](#method-TestThenPoolDifference-clone)

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
- [`RBExT::Model$prior_ESS()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_ESS)
- [`RBExT::Model$prior_treatment_benefit()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-prior_treatment_benefit)
- [`RBExT::Model$simulation_for_given_treatment_effect()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.html#method-simulation_for_given_treatment_effect)
- [`RBExT::TestThenPool$credible_interval()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-credible_interval)
- [`RBExT::TestThenPool$empirical_bayes_update()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-empirical_bayes_update)
- [`RBExT::TestThenPool$inference()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-inference)
- [`RBExT::TestThenPool$plot_test_pvalue_vs_drift()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-plot_test_pvalue_vs_drift)
- [`RBExT::TestThenPool$plot_test_vs_drift()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-plot_test_vs_drift)
- [`RBExT::TestThenPool$posterior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-posterior_cdf)
- [`RBExT::TestThenPool$posterior_median()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-posterior_median)
- [`RBExT::TestThenPool$posterior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-posterior_pdf)
- [`RBExT::TestThenPool$posterior_to_RBesT()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-posterior_to_RBesT)
- [`RBExT::TestThenPool$prior_cdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-prior_cdf)
- [`RBExT::TestThenPool$prior_pdf()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-prior_pdf)
- [`RBExT::TestThenPool$prior_to_RBesT()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-prior_to_RBesT)
- [`RBExT::TestThenPool$sample_posterior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-sample_posterior)
- [`RBExT::TestThenPool$sample_prior()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-sample_prior)
- [`RBExT::TestThenPool$test_decision()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TestThenPool.html#method-test_decision)

------------------------------------------------------------------------

### Method `new()`

Instantiate model

#### Usage

    TestThenPoolDifference$new(prior, mcmc_config = NULL)

#### Arguments

- `prior`:

  Prior

- `mcmc_config`:

  MCMC configuration

------------------------------------------------------------------------

### Method `test_pvalue()`

#### Usage

    TestThenPoolDifference$test_pvalue(target_data, test_type = "t-test")

#### Arguments

- `target_data`:

  Target study data

- `test_type`:

  Frequentist test

------------------------------------------------------------------------

### Method `test()`

#### Usage

    TestThenPoolDifference$test(target_data, test_type = "t-test")

#### Arguments

- `target_data`:

  Target study data

- `test_type`:

  Frequentist test

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TestThenPoolDifference$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
