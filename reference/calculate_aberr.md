# Aberration calculation functions

Aberration calculation functions

## Usage

``` r
calculate_aberr_power(data, aberr_prefix = "C", power = 1)

calculate_aberr_mean(X, N)

calculate_aberr_var(X, X2, N)

calculate_aberr_disp_index(mean, var)

calculate_aberr_u_value(X, N, mean, var, assessment_u = 1)

init_aberr_table(
  data,
  type = c("count", "case"),
  aberr_module = c("dicentrics", "translocations", "micronuclei")
)
```

## Arguments

- data:

  Count or case data.

- aberr_prefix:

  Prefix of the aberrations in the data.

- power:

  Power of aberration.

- X:

  Sum of detected aberrations.

- N:

  Number of cells analysed.

- X2:

  Quadratic sum of detected aberrations.

- mean:

  Mean.

- var:

  Variance.

- assessment_u:

  Expected \\u\\-value of the assessment. For a Poisson distribution
  this should be unity.

- type:

  Type of input data. Either "count" and "case".

- aberr_module:

  Aberration module.
