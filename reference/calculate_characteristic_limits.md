# Characteristic limits function———————————————–

Characteristic limits function———————————————–

## Usage

``` r
calculate_characteristic_limits(
  mu0 = NULL,
  n1 = NULL,
  y0 = NULL,
  n0 = NULL,
  alpha = 0.05,
  beta = 0.1,
  ymax = 100,
  type = "const"
)
```

## Arguments

- mu0:

  Background rate

- n1:

  Number of cells that will be analysed.

- y0:

  Background number of dicentrics.

- n0:

  Background number of cells analysed.

- alpha:

  Type I error rate, 0.05 by default.

- beta:

  Type II error rate, 0.1 by default.

- ymax:

  Max dicentrics to evaluate.

- type:

  Type.

## Value

List of characteristic limits (`decision_threshold`, `detection_limit`).

## Examples

``` r
control_data <- c(aberr = 4,
                  cells = 1000)

calculate_characteristic_limits(
      y0 = control_data["aberr"],
      n0 = control_data["cells"],
      n1 = 20,
      alpha = 0.05,
      beta = 0.1,
      ymax = 100,
      type = "var"
)
#> $decision_threshold
#> [1] 1
#> 
#> $detection_limit
#> [1] 3.88972
#> 
```
