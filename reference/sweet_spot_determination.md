# Determine Sweet Spot Bounds and Width

This function calculates the bounds and width of the "sweet spot" where
a given `y_values` vector crosses a specified `reference_value`. It uses
linear interpolation for precise crossing points.

## Usage

``` r
sweet_spot_determination(
  x_values,
  y_values,
  reference_value = 0,
  larger_is_better,
  return_NA_at_boundaries = FALSE
)
```

## Arguments

- x_values:

  A numeric vector of x values.

- y_values:

  A numeric vector of y values.

- reference_value:

  A numeric reference value to determine the crossing points.

## Value

A list containing:

- sweet_spot_bounds A numeric vector of length 2 with the lower and
  upper bounds of the sweet spot.

- sweet_spot_widthA numeric value indicating the width of the sweet
  spot.
