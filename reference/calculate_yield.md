# Calculate yield from dose

Calculate yield from dose

## Usage

``` r
calculate_yield(
  dose,
  type = c("estimate", "lower", "upper"),
  general_fit_coeffs,
  general_fit_var_cov_mat = NULL,
  protracted_g_value = 1,
  conf_int = 0.95
)
```

## Arguments

- dose:

  Numeric value of dose.

- type:

  Type of yield calculation. Can be "estimate", "lower", or "upper".

- general_fit_coeffs:

  Generalised fit coefficients matrix.

- general_fit_var_cov_mat:

  Generalised variance-covariance matrix.

- protracted_g_value:

  Protracted \\G(x)\\ value.

- conf_int:

  Curve confidence interval, 95% by default.

## Value

Numeric value of yield.
