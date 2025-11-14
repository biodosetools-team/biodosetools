# Calculate IRR (this is the same as the Odds-Ratio calculation in IAEA2011)

Calculate IRR (this is the same as the Odds-Ratio calculation in
IAEA2011)

## Usage

``` r
calculate_aberr_IRR(case_data, fit_coeffs, badge_dose)
```

## Arguments

- case_data:

  Case data in data frame form.

- fit_coeffs:

  Fitting coefficients matrix.

- badge_dose:

  Suspected (e.g. badge) dose

## Value

character variable representing IRR in the for (P(X=k\|D=0
Gy):P(X=k\|D=badge_dose)).
