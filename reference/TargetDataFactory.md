# TargetDataFactory class

A factory class for creating different types of target data objects.

## Methods

### Public methods

- [`TargetDataFactory$create()`](#method-TargetDataFactory-create)

- [`TargetDataFactory$clone()`](#method-TargetDataFactory-clone)

------------------------------------------------------------------------

### Method `create()`

Creates a target data object based on the source data and configuration.

#### Usage

    TargetDataFactory$create(
      source_data,
      case_study_config,
      target_sample_size_per_arm,
      control_drift = 0,
      treatment_drift,
      summary_measure_likelihood,
      target_to_source_std_ratio = NULL
    )

#### Arguments

- `source_data`:

  The source data object.

- `case_study_config`:

  The case study configuration object.

- `target_sample_size_per_arm`:

  The target sample size per arm.

- `control_drift`:

  The control drift.

- `treatment_drift`:

  The treatment drift.

- `summary_measure_likelihood`:

  The summary measure distribution.

- `target_to_source_std_ratio`:

  Ratio between the target and source study sampling standard deviation

#### Returns

The created target data object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TargetDataFactory$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
