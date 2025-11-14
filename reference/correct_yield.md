# Correct yields if they are below the curve

Correct yields if they are below the curve

## Usage

``` r
correct_yield(
  yield,
  type = "estimate",
  general_fit_coeffs,
  general_fit_var_cov_mat,
  conf_int
)
```

## Arguments

- yield:

  Numeric value of yield.

- type:

  Type of yield calculation. Can be "estimate", "lower", or "upper".

- general_fit_coeffs:

  Generalised fit coefficients matrix.

- general_fit_var_cov_mat:

  Generalised variance-covariance matrix.

- conf_int:

  Curve confidence interval.

## Value

Numeric value of corrected yield.
