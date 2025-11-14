# Perform GLM (Generalised Linear Model) fitting

Method based on the paper by Edwards, A. A. et al. (1979). Radiation
induced chromosome aberrations and the Poisson distribution. Radiation
and Environmental Biophysics, 16(2), 89-100. \<doi:10.1007/BF01323216\>.

## Usage

``` r
fit_glm_method(
  count_data,
  model_formula,
  model_family = c("automatic", "poisson", "quasipoisson", "nb2"),
  fit_link = "identity",
  aberr_module = c("dicentrics", "translocations", "micronuclei")
)
```

## Arguments

- count_data:

  Count data in data frame form.

- model_formula:

  Model formula.

- model_family:

  Model family.

- fit_link:

  Family link.

- aberr_module:

  Aberration module.

## Value

List object containing GLM fit results.
