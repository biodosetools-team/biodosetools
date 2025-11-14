# Perform max-likelihood optimization fitting

Method based on the paper by Oliveira, M. et al. (2016). Zero-inflated
regression models for radiation-induced chromosome aberration data: A
comparative study. Biometrical Journal, 58(2), 259-279.
\<doi:10.1002/bimj.201400233\>.

## Usage

``` r
fit_maxlik_method(
  data,
  model_formula,
  model_family = c("automatic", "poisson", "quasipoisson", "nb2"),
  fit_link,
  aberr_module = c("dicentrics", "translocations", "micronuclei")
)
```

## Arguments

- data:

  Count data.

- model_formula:

  Model formula.

- model_family:

  Model family.

- fit_link:

  Family link.

- aberr_module:

  Aberration module.

## Value

List object containing maxLik fit results.
