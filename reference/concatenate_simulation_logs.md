# Concatenate all simulation logs to a global dataframe

This function reads all CSV files with names starting with "logs" from
both the "bayesian" and "frequentist" subdirectories, concatenates them,
adds an "OCs" column, and saves the result to a new CSV file.

## Usage

``` r
concatenate_simulation_logs(results_dir)
```

## Arguments

- results_dir:

  A character string specifying the path of the results folder.

## Value

No return value, called for side effects.
