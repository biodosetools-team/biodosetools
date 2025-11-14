# Calculate AIC (Akaike's 'An Information Criterion')

Calculate AIC (Akaike's 'An Information Criterion')

## Usage

``` r
AIC_from_data(
  general_fit_coeffs,
  data,
  dose_var = "dose",
  yield_var = "yield",
  fit_link = "identity"
)
```

## Arguments

- general_fit_coeffs:

  Generalised fit coefficients matrix.

- data:

  Data (dose, yield) to calculate AIC from.

- dose_var:

  Name of the dose variable (enquoted).

- yield_var:

  Name of the yield variable (enquoted).

- fit_link:

  A specification for the model link function.

## Value

Numeric value of AIC.
