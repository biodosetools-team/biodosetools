# Get standard errors using delta method

Delta method for approximating the standard error of a transformation
\\g(X)\\ of a random variable \\X = (x1, x2, ...)\\, given estimates of
the mean and covariance matrix of \\X\\.

## Usage

``` r
get_deltamethod_std_err(
  fit_is_lq,
  variable = c("dose", "fraction_partial", "fraction_hetero"),
  mean_estimate,
  cov_estimate,
  protracted_g_value = NA,
  d0 = NA
)
```

## Arguments

- fit_is_lq:

  Whether the fit is linear quadratic (`TRUE`) or linear (`FALSE`).

- variable:

  Variable resulting of the transformation \\g(X)\\.

- mean_estimate:

  The estimated mean of \\X\\.

- cov_estimate:

  The estimated covariance matrix of \\X\\.

- protracted_g_value:

  Protracted \\G(x)\\ value.

- d0:

  Survival coefficient of irradiated cells.

## Value

Numeric value containing the standard error of the dose estimate.
