# TargetData class

A base class for target data objects.

## Public fields

- `endpoint`:

  The endpoint of the target data.

- `treatment_drift`:

  The treatment drift value.

- `control_drift`:

  The control drift value.

- `drift`:

  The drift value calculated as the difference between treatment and
  control drifts.

- `summary_measure_likelihood`:

  The distribution of the summary measure.

- `sample_size_per_arm`:

  The target sample size per arm.

- `sample_size_control`:

  The target sample size for the control group.

- `sample_size_treatment`:

  The target sample size for the treatment group.

- `treatment_effect`:

  The target treatment effect value.

- `sampling_approximation`:

  The flag indicating if sampling approximation is used.

- `standard_deviation`:

  The target standard deviation value.

- `sample`:

  The sample data.

## Methods

### Public methods

- [`TargetData$new()`](#method-TargetData-new)

- [`TargetData$to_dict()`](#method-TargetData-to_dict)

- [`TargetData$plot_sample()`](#method-TargetData-plot_sample)

- [`TargetData$clone()`](#method-TargetData-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the target data object.

#### Usage

    TargetData$new(
      source_data,
      sampling_approximation,
      target_sample_size_per_arm,
      control_drift = 0,
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

### Method `to_dict()`

Converts the target data object to a dictionary.

#### Usage

    TargetData$to_dict()

#### Returns

A list representing the target data object.

------------------------------------------------------------------------

### Method `plot_sample()`

Plot the target data samples

#### Usage

    TargetData$plot_sample(data)

#### Arguments

- `data`:

  Data samples

#### Returns

A plot.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TargetData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
