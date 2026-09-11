# ObservedSourceData class

This class represents the observed source data used for Bayesian
borrowing in clinical studies.

## Public fields

- `endpoint`:

  character. The endpoint of the study. Possible values are
  "continuous", "binary", "time_to_event", "recurrent_event".

- `summary_measure_likelihood`:

  character. The distribution of the summary measure. Possible values
  are "normal", "Binomial".

- `sample_size_control`:

  numeric. The sample size in the control arm of the source data.

- `sample_size_treatment`:

  numeric. The sample size in the treatment arm of the source data.

- `treatment_effect_estimate`:

  numeric. The estimated treatment effect in the source data.

- `standard_error`:

  numeric. The standard error of the treatment effect estimate in the
  source data.

- `control_rate`:

  numeric. The rate of success (1s) in the control arm of the source
  data (for binary endpoint).

- `treatment_rate`:

  numeric. The rate of success (1s) in the treatment arm of the source
  data (for binary endpoint).

- `equivalent_source_sample_size_per_arm`:

  numeric. Equivalent sample size per arm.

## Methods

### Public methods

- [`ObservedSourceData$new()`](#method-ObservedSourceData-new)

- [`ObservedSourceData$to_dict()`](#method-ObservedSourceData-to_dict)

- [`ObservedSourceData$clone()`](#method-ObservedSourceData-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize an instance of ObservedSourceData

#### Usage

    ObservedSourceData$new(case_study_config)

#### Arguments

- `case_study_config`:

  list. A list containing the configuration for the case study.

#### Returns

An instance of ObservedSourceData. Convert the object's data to a
dictionary format

------------------------------------------------------------------------

### Method `to_dict()`

#### Usage

    ObservedSourceData$to_dict()

#### Returns

list. A list containing the source data attributes.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ObservedSourceData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
