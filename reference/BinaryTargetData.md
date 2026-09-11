# BinaryTargetData class

A class for binary target data objects.

## Super class

[`RBExT::TargetData`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TargetData.md)
-\> `BinaryTargetData`

## Public fields

- `control_rate`:

  Rate in the control arm of the target study

- `treatment_rate`:

  Rate in the treatment arm of the target study

## Methods

### Public methods

- [`BinaryTargetData$new()`](#method-BinaryTargetData-new)

- [`BinaryTargetData$generate()`](#method-BinaryTargetData-generate)

- [`BinaryTargetData$to_dict()`](#method-BinaryTargetData-to_dict)

- [`BinaryTargetData$clone()`](#method-BinaryTargetData-clone)

Inherited methods

- [`RBExT::TargetData$plot_sample()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TargetData.html#method-plot_sample)

------------------------------------------------------------------------

### Method `new()`

Initializes the binary target data object.

#### Usage

    BinaryTargetData$new(
      source_data,
      sampling_approximation,
      target_sample_size_per_arm,
      control_drift,
      treatment_drift,
      summary_measure_likelihood
    )

#### Arguments

- `source_data`:

  The source data object.

- `sampling_approximation`:

  The sampling approximation flag.

- `target_sample_size_per_arm`:

  The target sample size per arm.

- `control_drift`:

  The control drift.

- `treatment_drift`:

  The treatment drift.

- `summary_measure_likelihood`:

  The summary measure distribution.

------------------------------------------------------------------------

### Method `generate()`

Generates samples for the binary target data object.

#### Usage

    BinaryTargetData$generate(n_replicates)

#### Arguments

- `n_replicates`:

  The number of replicates to generate.

#### Returns

A data frame containing the generated samples.

------------------------------------------------------------------------

### Method `to_dict()`

Converts the target data object to a dictionary.

#### Usage

    BinaryTargetData$to_dict()

#### Returns

A list representing the target data object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BinaryTargetData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
