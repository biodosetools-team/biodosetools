# Calculate algB

Calculate algB

## Usage

``` r
M_estimate(x, iter_loc = 50, iter_scale = 1000)
```

## Arguments

- x:

  vector of n observations.

- iter_loc:

  number of iteration steps for location estimate (default=50).

- iter_scale:

  number of iteration steps for scale estimate (default=1000).

## Value

Numeric value of zscore using algB.

## Examples

``` r
X = c(3.65, 2.5, 4.85)

M_estimate(x = as.numeric(X),
           iter_loc=50,
           iter_scale=1000)
#>   location    scale
#> 1 3.666172 1.192475
```
