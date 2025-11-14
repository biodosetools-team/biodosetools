# Calculate absorbed dose for mixed fields exposure

Calculate absorbed dose for mixed fields exposure

## Usage

``` r
fun.estimate.criticality(
  num_cases,
  dics,
  cells,
  coef_gamma,
  cov_gamma,
  coef_neutron,
  cov_neutron,
  ratio,
  p
)
```

## Arguments

- num_cases:

  number of cases to estimate.

- dics:

  dicentrics.

- cells:

  total cells.

- coef_gamma:

  Coefficients for gamma curve.

- cov_gamma:

  Covariance matrix for gamma curve.

- coef_neutron:

  Coefficients for neutron curve.

- cov_neutron:

  Covariance matrix for neutron curve.

- ratio:

  gamma-neutron ratio.

- p:

  photon contribution in mixed curve.

## Value

dose estimation

## Examples

``` r
#Results from the fitting module
fit_results_gamma <- system.file("extdata",
                                 "gamma_dicentrics-fitting-results.rds",
                                 package = "biodosetools") %>%
  readRDS()

fit_results_neutrons <- system.file("extdata",
                                    "neutrons-mixed-dicentrics-fitting-results.rds",
                                    package = "biodosetools") %>%
  readRDS()

#FUNCTION TO ESTIMATE DOSE IN CRITICALITY ACCIDENTS
fun.estimate.criticality(num_cases = 2,
                         dics = c(380,456),
                         cells = c(218,567),
                         coef_gamma = fit_results_gamma[["fit_coeffs"]][,1],
                         cov_gamma = fit_results_gamma[["fit_var_cov_mat"]],
                         coef_neutron = fit_results_neutrons[["fit_coeffs"]][,1],
                         cov_neutron = fit_results_neutrons[["fit_var_cov_mat"]],
                         ratio = 1.2,
                         p = 0)
#> [[1]]
#> [[1]]$gamma
#>      est      lwr      upr 
#> 2.173586 1.784314 2.562857 
#> 
#> [[1]]$neutron
#>      est      lwr      upr 
#> 1.811322 1.486929 2.135715 
#> 
#> [[1]]$total
#>      est      lwr      upr 
#> 3.984907 3.271243 4.698572 
#> 
#> 
#> [[2]]
#> [[2]]$gamma
#>       est       lwr       upr 
#> 1.0947571 0.8739362 1.3155780 
#> 
#> [[2]]$neutron
#>       est       lwr       upr 
#> 0.9122976 0.7282802 1.0963150 
#> 
#> [[2]]$total
#>      est      lwr      upr 
#> 2.007055 1.602216 2.411893 
#> 
#> 
```
