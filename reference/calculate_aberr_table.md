# Calculate aberrations table

Calculate aberrations table

## Usage

``` r
calculate_aberr_table(
  data,
  type = c("count", "case"),
  aberr_module = c("dicentrics", "translocations", "micronuclei"),
  assessment_u = 1
)
```

## Arguments

- data:

  Count or case data.

- type:

  Type of input data. Either "count" and "case".

- aberr_module:

  Aberration module, required for `type = "case"`.

- assessment_u:

  Expected \\u\\-value of the assessment. For a Poisson distribution
  this should be unity.

## Value

Data frame containing cell count (\\N\\), aberrations (\\X\\), and other
coefficients (dispersion index, \\u\\-value, ...), as well as raw count
or case `data`.

## Examples

``` r
data <- data.frame(
    ID = c("example1", "example2"),
    C0 = c(302, 160),
    C1 = c(28, 55),
    C2 = c(22, 19),
    C3 = c(8, 17),
    C4 = c(1, 9),
    C5 = c(0, 4)
)


calculate_aberr_table(data,
                      type = "case",
                      aberr_module = "dicentrics",
                      assessment_u = 1)
#> # A tibble: 2 × 13
#>   ID        N     X    C0    C1    C2    C3    C4    C5     y  y_err    DI     u
#>   <chr> <int> <int> <int> <int> <int> <int> <int> <int> <dbl>  <dbl> <dbl> <dbl>
#> 1 exam…   361   100   302    28    22     8     1     0 0.277 0.0368  1.77  10.4
#> 2 exam…   264   200   160    55    19    17     9     4 0.758 0.0736  1.89  10.2
```
