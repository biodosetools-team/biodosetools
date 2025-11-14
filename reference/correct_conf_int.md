# Correct yield confidence interval

Correct yield confidence interval if simple method is required.

## Usage

``` r
correct_conf_int(
  conf_int,
  general_fit_var_cov_mat,
  protracted_g_value = 1,
  type,
  dose = seq(0, 10, 0.2)
)
```

## Arguments

- conf_int:

  Confidence interval.

- general_fit_var_cov_mat:

  Generalised variance-covariance matrix.

- protracted_g_value:

  Protracted \\G(x)\\ value.

- type:

  Type of yield calculation. Can be "estimate", "lower", or "upper".

- dose:

  Numeric value of dose.

## Value

Numeric value of corrected confidence interval.
