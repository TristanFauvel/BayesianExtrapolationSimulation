# ContinuousTargetData class

A class for continuous target data objects.

## Super class

[`RBExT::TargetData`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TargetData.md)
-\> `ContinuousTargetData`

## Methods

### Public methods

- [`ContinuousTargetData$new()`](#method-ContinuousTargetData-new)

- [`ContinuousTargetData$generate()`](#method-ContinuousTargetData-generate)

- [`ContinuousTargetData$to_dict()`](#method-ContinuousTargetData-to_dict)

- [`ContinuousTargetData$clone()`](#method-ContinuousTargetData-clone)

Inherited methods

- [`RBExT::TargetData$plot_sample()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TargetData.html#method-plot_sample)

------------------------------------------------------------------------

### Method `new()`

Initializes the continuous target data object.

#### Usage

    ContinuousTargetData$new(
      source_data,
      sampling_approximation,
      target_sample_size_per_arm,
      control_drift,
      treatment_drift,
      summary_measure_likelihood,
      target_to_source_std_ratio
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

  The summary measure

- `target_to_source_std_ratio`:

  Ratio between the target and source studies standard deviation

------------------------------------------------------------------------

### Method `generate()`

Generates samples for the continuous target data object.

#### Usage

    ContinuousTargetData$generate(n_replicates)

#### Arguments

- `n_replicates`:

  The number of replicates to generate.

#### Returns

A data frame containing the generated samples.

------------------------------------------------------------------------

### Method `to_dict()`

Converts the target data object to a dictionary.

#### Usage

    ContinuousTargetData$to_dict()

#### Returns

A list representing the target data object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ContinuousTargetData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
