# Calculate manual translocation rate

Calculate manual translocation rate

## Usage

``` r
calculate_trans_rate_manual(cells, genome_factor, expected_aberr_value)
```

## Arguments

- cells:

  Number of cells `N`.

- genome_factor:

  Genomic conversion factor.

- expected_aberr_value:

  Expected aberrations.

## Value

Numeric value of translocation rate.

## Examples

``` r
calculate_trans_rate_manual(cells = 1000,
                            genome_factor = 0.58,
                            expected_aberr_value = 0.3)
#> [1] 174
```
