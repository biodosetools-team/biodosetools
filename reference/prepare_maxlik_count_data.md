# Prepare count data for max-likelihood optimization fitting

Prepare count data for max-likelihood optimization fitting

## Usage

``` r
prepare_maxlik_count_data(
  count_data,
  model_formula,
  aberr_module = c("dicentrics", "translocations", "micronuclei")
)
```

## Arguments

- count_data:

  Count data in data frame form.

- model_formula:

  Model formula.

- aberr_module:

  Aberration module.

## Value

Data frame of parsed count data.
