# Simulate a Scenario

This function simulates a scenario based on the provided inputs and
returns the results of the simulation.

## Usage

``` r
frequentist_ocs_scenario_simulation(
  scenario,
  simulation_config,
  scenarios_config,
  freq_filename,
  config_dir,
  case_studies_config_dir,
  frequentist_metrics,
  inference_metrics
)
```

## Arguments

- scenario:

  The scenario to simulate.

- simulation_config:

  Simulation configuration

- freq_filename:

  File name for the frequentist OCs

- config_dir:

  Configuration files directory

- case_studies_config_dir:

  Case studies configurations directory

- frequentist_metrics:

  List of frequentist metrics, as defined by sourcing
  `metrics_config.R`.

- inference_metrics:

  List of inference metrics, as defined by sourcing `metrics_config.R`.

## Value

The results of the simulation.
