# Create a forest plot

This function creates a forest plot using the provided data.

## Usage

``` r
forest_subplot(
  data,
  title,
  ylabel,
  x_metric_name,
  x_metric_uncertainty_lower,
  x_metric_uncertainty_upper,
  x_metric_label,
  methods_labels,
  legend = FALSE,
  sort_by = FALSE
)
```

## Arguments

- data:

  The data for creating the forest plot.

- title:

  The title of the forest plot.

- ylabel:

  A logical value indicating whether to display the y-axis label.

- x_metric_name:

  Name of the metric on the x-axis

- x_metric_uncertainty_lower:

  Lower limit of the metric on the x-axis

- x_metric_uncertainty_upper:

  Upper limit of the metric on the x-axis

- x_metric_label:

  Label of the metric on the x-axis

## Value

A ggplot2::ggplot( object representing the forest plot.
