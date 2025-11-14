# Calculate ZScore

Calculate ZScore

## Usage

``` r
calc.zValue.new(X, type, alg, c)
```

## Arguments

- X:

  vector of estimated doses.

- type:

  dose or freq.

- alg:

  algorithm. Choose between algA, algB or QHampel

- c:

  value for reference dose.

## Value

Numeric value of zscore.

## Examples

``` r
calc.zValue.new(X = c(3.65, 2.5, 4.85),
                type = "dose",
                alg = "algA" ,
                c = c(3.5, 3, 4.25))
#> [1]  0.1126264 -0.3754214  0.4505056
```
