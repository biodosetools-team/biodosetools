# Calculate protracted function \\G(x)\\

Calculation based on the paper by Lea, D. E. & Catcheside, D. G. (1942).
The mechanism of the induction by radiation of chromosome aberrations
in*Tradescantia*. Journal of Genetics, 44(2-3), 216-245.
\<doi:10.1007/BF02982830\>.

## Usage

``` r
protracted_g_function(time, time_0 = 2)
```

## Arguments

- time:

  Time over which the irradiation occurred.

- time_0:

  The mean lifetime of the breaks, which has been shown to be on the
  order of ~ 2 hours (default value).

## Value

Numeric value of \\G(x)\\.

## Examples

``` r
protracted_g_function(time = 3,
                      time_0 = 2)
#> [1] 0.6427824
```
