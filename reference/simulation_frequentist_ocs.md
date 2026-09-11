# Run simulations based on the given environment

Run simulations based on the given environment

## Usage

``` r
simulation_frequentist_ocs(
  env,
  simulation_config,
  scenarios_config,
  analysis_config,
  config_dir,
  case_studies_config_dir,
  logging_file_path,
  frequentist_metrics,
  inference_metrics
)
```

## Arguments

- env:

  The environment to run the simulations in

- simulation_config:

  Simulation configuration

- analysis_config:

  Analysis configuration

- config_dir:

  Configuration directory

- case_studies_config_dir:

  Case studies configuration directory

- frequentist_metrics:

  List of frequentist metrics, as defined by sourcing
  `metrics_config.R`.

- inference_metrics:

  List of inference metrics, as defined by sourcing `metrics_config.R`.

## Value

None
