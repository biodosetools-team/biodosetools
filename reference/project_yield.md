# Project yield into dose-effect fitting curve

Project yield into dose-effect fitting curve

## Usage

``` r
project_yield(
  yield,
  type = "estimate",
  general_fit_coeffs,
  general_fit_var_cov_mat = NULL,
  protracted_g_value = 1,
  conf_int = 0.95
)
```

## Arguments

- yield:

  Yield to be projected.

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

Numeric value of projected dose.

## Examples

``` r
fit_coeffs <- data.frame(
  estimate   = c(0.001280319, 0.021038724, 0.063032534),
  std.error  = c(0.0004714055, 0.0051576170, 0.0040073856),
  statistic  = c(2.715961, 4.079156, 15.729091),
  p.value    = c(6.608367e-03, 4.519949e-05, 9.557291e-56),
  row.names =  c("coeff_C", "coeff_alpha", "coeff_beta")
)



project_yield(yield = 0.67,
              type = "estimate",
              general_fit_coeffs = fit_coeffs[, "estimate"],
              general_fit_var_cov_mat = NULL,
              protracted_g_value = 1,
              conf_int = 0.95)
#> [1] 3.094549
```
