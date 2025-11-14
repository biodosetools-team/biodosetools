# Calculate QHampel

Calculate QHampel

## Usage

``` r
QHampel(y, lab, tol.G1 = 1e-06)
```

## Arguments

- y:

  vector of data values.

- lab:

  corresponding lab numbers.

- tol.G1:

  decimal place accuracy of the numerator of s.star.

## Value

Numeric value of zscore using QHampel algorithm.

## Examples

``` r
X = c(3.65, 2.5, 4.85)

QHampel(y = as.numeric(X),
        lab = 1:length(X),
        tol.G1=0.000001)
#>     x.star   s.star
#> 1 3.666667 2.579755
```
