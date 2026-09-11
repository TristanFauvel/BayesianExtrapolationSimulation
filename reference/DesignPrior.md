# DesignPrior Class

A class representing the design prior for Bayesian borrowing.

## Public fields

- `design_prior_type`:

  Type of design prior.

- `RBesT_model`:

  RBesT version of the model.

## Methods

### Public methods

- [`DesignPrior$create()`](#method-DesignPrior-create)

- [`DesignPrior$sample()`](#method-DesignPrior-sample)

- [`DesignPrior$cdf()`](#method-DesignPrior-cdf)

- [`DesignPrior$pdf()`](#method-DesignPrior-pdf)

- [`DesignPrior$clone()`](#method-DesignPrior-clone)

------------------------------------------------------------------------

### Method `create()`

Creates a new instance of DesignPrior.

#### Usage

    DesignPrior$create(
      design_prior_type,
      model,
      source_data,
      case_study_config,
      simulation_config,
      mcmc_config,
      case_study
    )

#### Arguments

- `design_prior_type`:

  The type of design prior.

- `model`:

  The model object.

- `source_data`:

  The source data object.

- `case_study_config`:

  Case study configuration

- `simulation_config`:

  Simulation configuration

- `mcmc_config`:

  MCMC configuration

- `case_study`:

  Case study name

#### Returns

A new instance of DesignPrior.

------------------------------------------------------------------------

### Method [`sample()`](https://rdrr.io/r/base/sample.html)

Samples from the design prior.

#### Usage

    DesignPrior$sample(n_samples)

#### Arguments

- `n_samples`:

  The number of samples to generate.

------------------------------------------------------------------------

### Method `cdf()`

Computes the cumulative distribution function (CDF) of the design prior.

#### Usage

    DesignPrior$cdf(x)

#### Arguments

- `x`:

  The value at which to evaluate the CDF.

------------------------------------------------------------------------

### Method [`pdf()`](https://rdrr.io/r/grDevices/pdf.html)

Computes the PDF of the design prior.

#### Usage

    DesignPrior$pdf(x)

#### Arguments

- `x`:

  The value at which to evaluate the PDF.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DesignPrior$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
