# Summary and curve tables

Summary and curve tables

## Usage

``` r
summary_curve_tables(num_labs, list_lab_names, all_rds)
```

## Arguments

- num_labs:

  number of laboratories participating.

- list_lab_names:

  list with the laboratory names.

- all_rds:

  list with the rds files.

## Value

list(sorted_table_ilc, sorted_table_curve)

## Examples

``` r
#fit_results_X is the output from the Estimation module

fit_results_A1 <- system.file("extdata", "A1_Estimation_results.rds", package = "biodosetools") %>%
  readRDS()

fit_results_A2 <- system.file("extdata", "A2_Estimation_results.rds", package = "biodosetools") %>%
  readRDS()



summary_curve_tables(num_labs = 2,
                     list_lab_names = c("A", "B"),
                     all_rds = list(fit_results_A1, fit_results_A2)
                     )
#> [[1]]
#>   Lab     Module   Type Sample   N   X estimate    lower    upper     y  y.err
#> 1   A dicentrics manual      1 200 140 2.589230 2.083403 3.208857 0.700 0.0610
#> 4   B dicentrics manual      1 200 111 2.610970 2.231150 3.045616 0.555 0.0495
#> 3   A dicentrics manual      2 200 204 3.146055 2.614931 3.785879 1.020 0.0733
#> 6   B dicentrics manual      2 200 147 3.018682 2.622748 3.471269 0.735 0.0556
#> 2   A dicentrics manual      3 200 368 4.259400 3.882349 4.688910 1.840 0.0923
#> 5   B dicentrics manual      3 200 253 3.989222 3.552963 4.487336 1.265 0.0785
#>       DI       u
#> 1 1.0625  0.6252
#> 4 0.8818 -1.1840
#> 3 1.0539  0.5389
#> 6 0.8406 -1.5951
#> 2 0.9255 -0.7442
#> 5 0.9731 -0.2692
#> 
#> [[2]]
#>   Lab     Module   Type radiation quality calibration irradiation temperature
#> 1   A dicentrics manual            Cs-137   air kerma         air          20
#> 4   B dicentrics manual             Co-60   air kerma         air          37
#>   dose rate curve origin           C      alpha      beta  C std.error
#> 1     0.446          own 0.001189589 0.01903783 0.0968831 0.0001040828
#> 4      0.27          own 0.000500000 0.01420000 0.0759000 0.0005000000
#>   alpha std.error beta std.error max curve dose
#> 1     0.004119324    0.003505209           6.00
#> 4     0.004400000    0.002700000           5.05
#> 
```
