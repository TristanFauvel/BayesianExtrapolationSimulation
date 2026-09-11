# p_value_based_PP_Gaussian class

This class represents a p-value based power prior method. It inherits
from the Gaussian_empirical_Bayes_PP class.

## Super classes

[`RBExT::Model`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Model.md)
-\>
[`RBExT::ConjugateGaussian`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ConjugateGaussian.md)
-\>
[`RBExT::StaticBorrowingGaussian`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/StaticBorrowingGaussian.md)
-\>
[`RBExT::Gaussian_empirical_Bayes_PP`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/Gaussian_empirical_Bayes_PP.md)
-\> `p_value_based_PP`

## Public fields

- `shape_parameter`:

  The shape parameter for the method.

- `equivalence_margin`:

  The equivalence margin for the method.

- `method`:

  Method name

## Methods

### Public methods

- [`p_value_based_PP_Gaussian$new()`](#method-p_value_based_PP-new)

- [`p_value_based_PP_Gaussian$test()`](#method-p_value_based_PP-test)

- [`p_value_based_PP_Gaussian$power_parameter_estimation()`](#method-p_value_based_PP-power_parameter_estimation)

- [`p_value_based_PP_Gaussian$clone()`](#method-p_value_based_PP-clone)

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

Initialize the p_value_based_PP object.

#### Usage

    p_value_based_PP_Gaussian$new(prior, theta_0, null_space)

#### Arguments

- `prior`:

  The prior object.

- `theta_0`:

  Boundary of the null hypothesis space

- `null_space`:

  Null space.

#### Returns

None Test method

------------------------------------------------------------------------

### Method `test()`

This method performs the test for the given target data.

#### Usage

    p_value_based_PP_Gaussian$test(
      target_data,
      source_treatment_effect_estimate,
      target_treatment_effect_estimate,
      test_type = "t-test"
    )

#### Arguments

- `target_data`:

  The target data object.

- `source_treatment_effect_estimate`:

  The source treatment effect estimate.

- `target_treatment_effect_estimate`:

  The target treatment effect estimate.

- `test_type`:

  Type of frequentist test used

#### Returns

The p-value. Power parameter estimation method

------------------------------------------------------------------------

### Method `power_parameter_estimation()`

This method estimates the power parameter for the given target data.

#### Usage

    p_value_based_PP_Gaussian$power_parameter_estimation(target_data)

#### Arguments

- `target_data`:

  The target data object.

#### Returns

The power parameter.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    p_value_based_PP_Gaussian$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
