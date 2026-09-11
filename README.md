# R Bayesian Extrapolation Tool

[![R Package Build](https://github.com/TristanFauvel/BayesianExtrapolationSimulation/actions/workflows/build.yml/badge.svg)](https://github.com/TristanFauvel/BayesianExtrapolationSimulation/actions/workflows/build.yml)
[![pkgdown](https://github.com/TristanFauvel/BayesianExtrapolationSimulation/actions/workflows/pkgdown.yml/badge.svg)](https://tristanfauvel.github.io/BayesianExtrapolationSimulation/)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.0.2-blue.svg)](DESCRIPTION)
[![R >= 3.5.0](https://img.shields.io/badge/R-%3E%3D3.5.0-276DC3.svg)](https://www.r-project.org/)

A collection of R tools to :

- Study frequentist and Bayesian operating characteristics of clinical trial designs leveraging Bayesian partial extrapolation (also known as borrowing).
- Analyze clinical trial data using Bayesian partial extrapolation methods.

<div class="row row-cols-1 row-cols-sm-2 row-cols-lg-4 g-3 my-4">
<div class="col"><a href="articles/index.html" class="card h-100 text-decoration-none"><div class="card-body"><h5 class="card-title">📚 Vignettes</h5><p class="card-text small text-muted">Step-by-step guides to simulation studies, data generation, borrowing methods, and case-study replications.</p></div></a></div>
<div class="col"><a href="reference/index.html" class="card h-100 text-decoration-none"><div class="card-body"><h5 class="card-title">🔧 API reference</h5><p class="card-text small text-muted">Every exported function and R6 class, grouped by topic: data, methods, plots, tables, operating characteristics.</p></div></a></div>
<div class="col"><a href="#browser-based-interface" class="card h-100 text-decoration-none"><div class="card-body"><h5 class="card-title">🖥️ Browser app</h5><p class="card-text small text-muted">Configure, run, and analyze a simulation study from a Shiny app, without hand-editing config files.</p></div></a></div>
<div class="col"><a href="news/index.html" class="card h-100 text-decoration-none"><div class="card-body"><h5 class="card-title">📝 Changelog</h5><p class="card-text small text-muted">What changed in each release.</p></div></a></div>
</div>

Copyright 2024 Quinten Health, under exclusive licence to the European Medicines Agency. Sharing and distribution are prohibited.

- Core contributor: Tristan Fauvel
- Contributors: Pascal Godbillot
- Project management: Marie Génin
- Leads: Julien Tanniou, Billy Amzal
- Sponsors: European Medicines Agency, Quinten Health

## Quick start

### Installation

RBExT needs R >= 4.0 and, for the MCMC-based methods, a C++ toolchain and a
CmdStan installation - the Stan models are compiled to native binaries the
first time they are used. `install.R` handles all three.

Download `RBExT_0.0.2.tar.gz` and `install.R` from the latest release into the
same directory, then run:

```
Rscript install.R
```

That installs the dependencies, RBExT itself, and CmdStan if it is missing.
Load the package with:

```r
library(RBExT)
```

<details>
<summary>Installing by hand</summary>

`cmdstanr` is not on CRAN, so its repository has to be declared before the
dependencies will resolve:

```r
options(repos = c(
  CRAN = "https://cloud.r-project.org",
  stan = "https://stan-dev.r-universe.dev"
))
install.packages("/path/to/RBExT_0.0.2.tar.gz", repos = NULL, type = "source")
cmdstanr::check_cmdstan_toolchain(fix = TRUE)
cmdstanr::install_cmdstan()
```

</details>

<details>
<summary>Working from a source checkout</summary>

`renv.lock` pins every dependency. From the checkout:

```r
renv::restore()
devtools::load_all()
```

</details>

### Running and analyzing simulations

- To launch a simulation study, run main.R

- To plot the results of the simulation study, run plots.R

- To create the results tables, run tables.R

### Browser-based interface

Instead of hand-editing config files and running the scripts above, you can
configure, run, and analyze a simulation study from a browser:

```r
library(RBExT)
run_rbext_app()
```

The app works out of a *workspace* directory, where `results/`, `logs/` and
`user_configs/` live. Launched from a source checkout it uses the checkout, so
results land next to the ones `main.R` produces; installed from a release
tarball it uses a per-user directory under `tools::R_user_dir("RBExT", "data")`.
Either way the path is reported when the app starts, and `run_rbext_app()`
takes an explicit one:

```r
run_rbext_app(workspace = "~/rbext-studies")
```

On Linux, `install.R` also adds an **RBExT** entry to the application menu, so
the app can be started without an R session. `create_rbext_shortcut()` rewrites
it, optionally pinned to a workspace:

```r
create_rbext_shortcut(workspace = "~/rbext-studies")
```

The entry opens a terminal, which is where the workspace path and the progress
of a run appear, and where Ctrl+C stops the app.

When developing against a checkout, `devtools::load_all()` replaces
`library(RBExT)`.

This opens a local Shiny app with three tabs:

- **Configure**: pick an existing case study or build a new one, choose methods and their parameter grids, and set scenario/MCMC settings. Saving writes a new environment under `user_configs/` (gitignored) without touching the package's own `inst/conf/`.
- **Run**: launch a saved environment as a background process, with a live progress bar, log tail, and a cancel button.
- **Analyze**: browse any `results/<env>/` directory - including ones produced by `main.R`/HPC runs, not just ones launched from the app - filter it, render interactive plots, and browse/export the data as a table.

## Design logic

There are three main objects in the simulation framework: source data, target data, and models.
Source data are defined based on the case study configuration. Target data depend on the source data, and the specific scenario (drift, sample size). Models correspond to specific methods, and depend on the source data. They implement methods to estimate their operating characteristics. The target data objects implement a sample method, allowing to sample many replicates of the target study.

## Access documentation

The full documentation website is available at [tristanfauvel.github.io/BayesianExtrapolationSimulation](https://tristanfauvel.github.io/BayesianExtrapolationSimulation/).

To access it locally instead :
browseURL("docs/index.html")

## Methods and configurations requiring MCMC inference

MCMC inference with Stan is used in the following scenarios :

- RMP x aprepitant
- separate x aprepitant
- pooling x aprepitant
- conditional_power_prior x aprepitant

When using MCMC inference, the method inherits from the MCMCModel class. The initialize() method contains a string with the Stan code. This code is saved in a temporary Stan file which is used to compile the model (using `self$stan_model <- cmdstanr::cmdstan_model(stan_file_path)`). At inference

### Comparison with existing packages

- psborrow2 implements Bayesian dynamic borrowing in scenarios where a two-arm RCT is supplemented with external data on the control arm.

- RBEsT focuses on meta-analytic and mixture models

## Contributing

New contributors are always welcome. Please have a look at the [contribution guidelines](CONTRIBUTING.md) on how to get started and to make sure your code complies with our guidelines.

## Rationale of the design

The core of the code implements the inference logic: a source data class represents source data, which are observed. Target data are represented in a different class. They different as we can sample target data replicates. When initializing a model, source data are provided to it, as well as methods parameters and MCMC configuration (if applicable), so as to define a prior. Inference is performed by using the inference method on target data, which updates properties of the model object with moments of the treatment effect posterior distribution and, if applicable, computes the posterior distribution of borrowing parameters. Note that inference proceeds in two steps: first, if the method uses empirical Bayes, prior parameters are updated based on the target data sample, second Bayes" rule is applied.

When launching a simulation study, all scenarios are generated based on the provided configuration files. These scenarios are then sequentially treated: source data and target data are initiated, and a model is defined. Target data replicates are then generated depending on the target data characteristics. The model is then fitted on each data replicate, and the corresponding inference metrics and frequentist metrics are computed.
The data from the simulation are then analyzed: the sweet spot is computed for each metric of interest, and the Bayesian operating characteristics are estimated.
