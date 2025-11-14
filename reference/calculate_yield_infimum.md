# Calculate theoretical yield infimum

Calculate theoretical yield infimum

## Usage

``` r
calculate_yield_infimum(
  type = c("estimate", "lower", "upper"),
  general_fit_coeffs,
  general_fit_var_cov_mat = NULL,
  conf_int = 0.95
)
```

## Arguments

- type:

  Type of yield calculation. Can be "estimate", "lower", or "upper".

- general_fit_coeffs:

  Generalised fit coefficients matrix.

- general_fit_var_cov_mat:

  Generalised variance-covariance matrix.

- conf_int:

  Curve confidence interval, 95% by default.

## Value

Numeric value of yield infimum.
