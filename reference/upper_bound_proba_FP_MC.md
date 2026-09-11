# Calculate the upper bound probability of false positive

This function calculates the upper bound probability of false positive
based on the model, prior probability of no benefit, source data,
theta_0, target sample size per arm, case study configuration, number of
replicates, confidence level, null space, and critical value.

## Usage

``` r
upper_bound_proba_FP_MC(
  model,
  prior_proba_no_benefit,
  source_data,
  theta_0,
  target_sample_size_per_arm,
  case_study_config,
  target_to_source_std_ratio,
  n_replicates,
  confidence_level,
  null_space,
  critical_value,
  case_study,
  method,
  n_samples_quantiles_estimation
)
```

## Arguments

- model:

  The model.

- prior_proba_no_benefit:

  The prior probability of no benefit.

- source_data:

  The source data.

- theta_0:

  The value of theta_0.

- target_sample_size_per_arm:

  The target sample size per arm.

- case_study_config:

  The case study configuration.

- target_to_source_std_ratio:

  Ratio between target and source sampling standard deviations.

- n_replicates:

  The number of replicates.

- confidence_level:

  The confidence level.

- null_space:

  The null space (either "left" or "right").

- critical_value:

  The critical value.

- case_study:

  Case study name.

- method:

  Method name.

- n_samples_quantiles_estimation:

  Number of samples used to estimate distribution quantiles.

## Value

The upper bound probability of false positive, defined as \\Pr(Study
success\|\theta_T = \theta_0) \times Pr(\theta_T \leq \theta_0)\\

## Examples

``` r
NA
#> [1] NA
```
