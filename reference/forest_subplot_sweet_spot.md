# Create a forest plot

This function creates a forest plot using the provided data.

## Usage

``` r
forest_subplot_sweet_spot(
  data,
  title,
  sweet_spot_lower,
  sweet_spot_upper,
  x_range,
  x_metric_label,
  x_metric_name,
  methods_labels,
  legend = FALSE,
  case_study,
  sort_by = "values"
)
```

## Arguments

- data:

  The data for creating the forest plot.

- title:

  The title of the forest plot.

- sweet_spot_lower:

  Lower limit of the metric on the x-axis

- sweet_spot_upper:

  Upper limit of the metric on the x-axis

- x_metric_label:

  Label of the metric on the x-axis

- x_metric_name:

  Name of the metric on the x-axis

- ylabel:

  A logical value indicating whether to display the y-axis label.

## Value

A ggplot2 object representing the forest plot.
