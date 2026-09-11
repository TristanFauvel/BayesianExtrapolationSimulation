# Simulation Bayesian OCs

This function estimates the Bayesian operating characteristics for the
model in the scenario considered.

## Usage

``` r
estimate_bayesian_ocs(
  scenario,
  case_study_config,
  source_data,
  target_sample_size_per_arm,
  model,
  method_parameters,
  theta_0,
  n_replicates,
  critical_value,
  computation_state,
  design_prior_type,
  json_parameters,
  simulation_config,
  target_to_source_std_ratio,
  mcmc_config = NULL
)
```

## Arguments

- scenario:

  Simulation scenario

- case_study_config:

  The case study configuration.

- source_data:

  The source data.

- target_sample_size_per_arm:

  The target sample size per arm.

- model:

  The model.

- method_parameters:

  The method parameters.

- theta_0:

  The true parameter value.

- n_replicates:

  The number of replicates.

- critical_value:

  The critical value.

- computation_state:

  The computation state.

- design_prior_type:

  The design prior type.

- json_parameters:

  Method parameters in json format

- simulation_config:

  Simulation configuration

- target_to_source_std_ratio:

  Ratio between the source and target study sampling standard deviation.

- mcmc_config:

  MCMC configuration

## Value

The results of the Bayesian operating characteristics estimation.
