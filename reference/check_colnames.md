# Check columns in a dataframe

This function checks if a dataframe contains the expected columns and
only the expected columns. When `types` is supplied it additionally
checks that those columns hold the declared type, reporting every
mismatch at once.

## Usage

``` r
check_colnames(df, expected_colnames, types = default_coltypes(expected_colnames))
```

## Arguments

- df:

  A dataframe to check.

- expected_colnames:

  A character vector of column names the dataframe should contain.

- types:

  An optional named character vector mapping column names to the type
  they are expected to hold, using the vocabulary of
  column_type_predicates. Names must be among `expected_colnames`;
  columns the spec does not mention are not type checked.

## Value

No return value, called for side effects.
