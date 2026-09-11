# ObservedTargetData class

A wrapper used for analyzing observed data (not used for simulation)

## Public fields

- `summary_measure_likelihood`:

  Summary measure distribution

- `sample_size_per_arm`:

  Sample size per arm in the target study

- `sample`:

  Observed data sample

## Methods

### Public methods

- [`ObservedTargetData$new()`](#method-ObservedTargetData-new)

- [`ObservedTargetData$clone()`](#method-ObservedTargetData-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes an ObservedTargetData object with the given parameters

#### Usage

    ObservedTargetData$new(
      treatment_effect_estimate,
      treatment_effect_standard_error,
      target_sample_size_per_arm,
      summary_measure_likelihood
    )

#### Arguments

- `treatment_effect_estimate`:

  The estimated treatment effect

- `treatment_effect_standard_error`:

  The standard error of the treatment effect estimate

- `target_sample_size_per_arm`:

  The target sample size per treatment arm

- `summary_measure_likelihood`:

  The distribution of the summary measure (e.g., "normal", "binomial")

#### Returns

An initialized ObservedTargetData object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ObservedTargetData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
