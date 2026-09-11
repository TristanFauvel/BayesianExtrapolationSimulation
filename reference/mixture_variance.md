# Calculate the variance of a mixture distribution

This function computes the variance of a mixture of two distributions.

## Usage

``` r
mixture_variance(w, sigma2A, muA, sigma2B, muB)
```

## Arguments

- w:

  Numeric. The weight of the first distribution in the mixture.

- sigma2A:

  Numeric. The variance of the first distribution.

- muA:

  Numeric. The mean of the first distribution.

- sigma2B:

  Numeric. The variance of the second distribution.

- muB:

  Numeric. The mean of the second distribution.

## Value

Numeric. The total variance of the mixture distribution.
