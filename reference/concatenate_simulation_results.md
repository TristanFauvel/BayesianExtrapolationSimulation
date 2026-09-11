# Concatenate all simulation results to a global dataframe

This function reads all CSV files with names starting with "results"
from the specified directory, concatenates them, and saves the result to
a new CSV file.

## Usage

``` r
concatenate_simulation_results(results_dir, ocs_filename)
```

## Arguments

- results_dir:

  A character string specifying the path of the results folder.

- ocs_filename:

  A character string specifying the filename of the results dataframe.

## Value

No return value, called for side effects.
