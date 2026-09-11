# Sample aggregate normal data based on the mean and variance

This function samples aggregate normal data based on the mean and
variance.

## Usage

``` r
sample_aggregate_normal_data(mean, variance, n_replicates, n_samples_per_arm)
```

## Arguments

- mean:

  The mean of the normal distribution

- variance:

  The variance of the normal distribution

- n_replicates:

  Number of replicates to sample

- n_samples_per_arm:

  Number of samples per replicate

## Value

A list containing the sample mean and sample standard error for each
replicate
