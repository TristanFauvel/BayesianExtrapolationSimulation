# Perform Simulation Analysis

This function performs a simulation analysis for a given environment
using the specified analysis configuration and configuration directory.
It reads the output configuration, loads results, applies frequentist
power analysis, and computes the sweet spot.

## Usage

``` r
simulation_analysis(
  env,
  analysis_config,
  config_dir,
  frequentist_metrics,
  case_studies = "all",
  to_compute = c("frequentist_power_at_equivalent_tie",
    "frequentist_power_at_nominal_tie", "sweet_spot", "bayesian_ocs"),
  methods = "all",
  case_studies_config_dir = NULL,
  parallelization = NULL
)
```

## Arguments

- env:

  A character string specifying the environment for the analysis (e.g.,
  "development", "production").

- analysis_config:

  A list containing the analysis configuration parameters.

- config_dir:

  A character string specifying the directory containing the
  configuration files.

- frequentist_metrics:

  List of frequentist metrics.

- parallelization:

  Whether the analysis runs in parallel: a single logical, or a list of
  method names, in the same form as the `parallelization` entry of
  `scenarios_config.yml`. Defaults to that entry, read from
  `config_dir`.

## Value

This function does not return a value. It writes the results and sweet
spot analysis to CSV files in the specified results directory.
