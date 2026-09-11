# SourceData class

This class represents the source data used for Bayesian borrowing. It
processes the source data to compute necessary statistics for various
endpoints.

## Super class

[`RBExT::ObservedSourceData`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ObservedSourceData.md)
-\> `SourceData`

## Methods

### Public methods

- [`SourceData$new()`](#method-SourceData-new)

- [`SourceData$clone()`](#method-SourceData-clone)

Inherited methods

- [`RBExT::ObservedSourceData$to_dict()`](https://quinten-health-os.github.io/BayesianExtrapolationSimulation/reference/ObservedSourceData.html#method-to_dict)

------------------------------------------------------------------------

### Method `new()`

Initialize the SourceData object

#### Usage

    SourceData$new(case_study_config, source_denominator = NA)

#### Arguments

- `case_study_config`:

  list: A configuration list containing details of the case study.

- `source_denominator`:

  numeric: An optional denominator for computing control rates.

#### Returns

A new SourceData object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SourceData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
