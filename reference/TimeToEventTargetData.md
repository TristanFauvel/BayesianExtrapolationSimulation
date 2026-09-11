# Time To Event Target Data

This class represents target data for time-to-event analysis. It
inherits from the TargetData class.

## Super class

[`RBExT::TargetData`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TargetData.md)

## Public fields

- `control_rate`:

  Rate in the control arm of the target study

- `treatment_rate`:

  Rate in the treatment arm of the target study

- `max_follow_up_time`:

  Maximum follow-up time

## Methods

### Public methods

- [`TimeToEventTargetData$new()`](#method-NA-new)

- [`TimeToEventTargetData$generate()`](#method-NA-generate)

- [`TimeToEventTargetData$to_dict()`](#method-NA-to_dict)

- [`TimeToEventTargetData$clone()`](#method-unknown-clone)

Inherited methods

- [`RBExT::TargetData$plot_sample()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/TargetData.html#method-plot_sample)

------------------------------------------------------------------------

### Method `new()`

Initialize the TimeToEventTargetData object

#### Usage

    TimeToEventTargetData$new(
      source_data,
      sampling_approximation,
      target_sample_size_per_arm,
      control_drift = 0,
      treatment_drift,
      summary_measure_likelihood,
      max_follow_up_time
    )

#### Arguments

- `source_data`:

  The source data used for generating the target data.

- `sampling_approximation`:

  A flag indicating whether to use sampling approximation.

- `target_sample_size_per_arm`:

  The target sample size per arm.

- `control_drift`:

  The control drift.

- `treatment_drift`:

  The treatment drift.

- `summary_measure_likelihood`:

  The summary measure distribution.

- `max_follow_up_time`:

  The maximum follow-up time. Generate target data

------------------------------------------------------------------------

### Method `generate()`

This function generates target data based on the specified parameters.

#### Usage

    TimeToEventTargetData$generate(n_replicates)

#### Arguments

- `n_replicates`:

  The number of replicates to generate.

#### Returns

A data frame containing the generated target data.

------------------------------------------------------------------------

### Method `to_dict()`

Converts the target data object to a dictionary.

#### Usage

    TimeToEventTargetData$to_dict()

#### Returns

A list representing the target data object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TimeToEventTargetData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
