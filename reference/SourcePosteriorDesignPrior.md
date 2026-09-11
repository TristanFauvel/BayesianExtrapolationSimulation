# SourcePosteriorDesignPrior class

A class representing the source posterior design prior for Bayesian
borrowing.

## Super class

[`RBExT::DesignPrior`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/DesignPrior.md)
-\> `SourcePosteriorDesignPrior`

## Public fields

- `parameters`:

  Parameters

- `summary_measure_likelihood`:

  Treatment effect distribution

- `n_successes_control`:

  Number of successes in the control arm

- `n_successes_treatment`:

  Number of successes in the treatment arm

- `n_control`:

  Number of participants in the control arm

- `n_treatment`:

  Number of participants in the treatment arm

## Methods

### Public methods

- [`SourcePosteriorDesignPrior$new()`](#method-SourcePosteriorDesignPrior-new)

- [`SourcePosteriorDesignPrior$sample()`](#method-SourcePosteriorDesignPrior-sample)

- [`SourcePosteriorDesignPrior$cdf()`](#method-SourcePosteriorDesignPrior-cdf)

- [`SourcePosteriorDesignPrior$pdf()`](#method-SourcePosteriorDesignPrior-pdf)

- [`SourcePosteriorDesignPrior$clone()`](#method-SourcePosteriorDesignPrior-clone)

Inherited methods

- [`RBExT::DesignPrior$create()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/DesignPrior.html#method-create)

------------------------------------------------------------------------

### Method `new()`

Initializes a new instance of SourcePosteriorDesignPrior.

#### Usage

    SourcePosteriorDesignPrior$new(
      source_data,
      case_study_config,
      simulation_config,
      mcmc_config = NULL
    )

#### Arguments

- `source_data`:

  The source data object.

- `case_study_config`:

  Case study configuration

- `simulation_config`:

  Simulation configuration

- `mcmc_config`:

  MCMC configuration

------------------------------------------------------------------------

### Method [`sample()`](https://rdrr.io/r/base/sample.html)

Samples from the source posterior design prior.

#### Usage

    SourcePosteriorDesignPrior$sample(n_samples)

#### Arguments

- `n_samples`:

  The number of samples to generate.

------------------------------------------------------------------------

### Method `cdf()`

Computes the cumulative distribution function (CDF) of the source
posterior design prior.

#### Usage

    SourcePosteriorDesignPrior$cdf(x)

#### Arguments

- `x`:

  The value at which to evaluate the CDF.

------------------------------------------------------------------------

### Method [`pdf()`](https://rdrr.io/r/grDevices/pdf.html)

Computes the cumulative distribution function (PDF) of the source
posterior design prior.

#### Usage

    SourcePosteriorDesignPrior$pdf(x)

#### Arguments

- `x`:

  The value at which to evaluate the PDF.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SourcePosteriorDesignPrior$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
