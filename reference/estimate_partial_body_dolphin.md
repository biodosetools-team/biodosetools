# Partial-body dose estimation (Dolphin's method)

Method based on the paper by Dolphin, G. W. (1969). Biological Dosimetry
with Particular Reference to Chromosome Aberration Analysis: A Review of
Methods. International Atomic Energy Agency (IAEA) Retrieved from
<https://inis.iaea.org/search/search.aspx?orig_q=RN:45029080>.

## Usage

``` r
estimate_partial_body_dolphin(
  num_cases,
  case_data,
  fit_coeffs,
  fit_var_cov_mat,
  conf_int = 0.95,
  protracted_g_value = 1,
  genome_factor = 1,
  gamma,
  aberr_module = c("dicentrics", "translocations", "micronuclei")
)
```

## Arguments

- num_cases:

  number of cases.

- case_data:

  Case data in data frame form.

- fit_coeffs:

  Fitting coefficients matrix.

- fit_var_cov_mat:

  Fitting variance-covariance matrix.

- conf_int:

  Confidence interval, 95% by default.

- protracted_g_value:

  Protracted \\G(x)\\ value.

- genome_factor:

  Genomic conversion factor used in translocations, else 1.

- gamma:

  Survival coefficient of irradiated cells.

- aberr_module:

  Aberration module.

## Value

List containing estimated doses data frame, observed fraction of cells
scored which were irradiated, estimated fraction of irradiated blood
data frame, AIC, and `conf_int_*` used.

## Examples

``` r
#The fitting RDS result from the fitting module is needed. Alternatively, manual data
#frames that match the structure of the RDS can be used:
fit_coeffs <- data.frame(
    estimate   = c(0.001280319, 0.021038724, 0.063032534),
    std.error  = c(0.0004714055, 0.0051576170, 0.0040073856),
    statistic  = c(2.715961, 4.079156, 15.729091),
    p.value    = c(6.608367e-03, 4.519949e-05, 9.557291e-56),
    row.names =  c("coeff_C", "coeff_alpha", "coeff_beta")
  )


fit_var_cov_mat <- data.frame(
  coeff_C      = c(2.222231e-07, -9.949044e-07,  4.379944e-07),
  coeff_alpha  = c(-9.949044e-07, 2.660101e-05, -1.510494e-05),
  coeff_beta   = c(4.379944e-07, -1.510494e-05, 1.605914e-05),
  row.names =  c("coeff_C", "coeff_alpha", "coeff_beta")
)


case_data <- data.frame(
  ID= "example1",
  N = 361,
  X = 100,
  C0 = 302,
  C1 = 28,
  C2 = 22,
  C3 = 8,
  C4 = 1,
  C5 = 0,
  y = 0.277,
  y_err = 0.0368,
  DI = 1.77,
  u = 10.4
)

#FUNCTION ESTIMATE_PARTIAL_BODY_DOLPHIN
estimate_partial_body_dolphin(
  num_cases = 1,
  case_data = case_data,
  fit_coeffs = as.matrix(fit_coeffs),
  fit_var_cov_mat = as.matrix(fit_var_cov_mat),
  conf_int = 0.95,
  gamma = 1 / 2.7,
  aberr_module = "dicentrics"
)
#> [[1]]
#> [[1]]$est_doses
#>      lower estimate    upper
#> 1 3.493143 4.137905 4.782667
#> 
#> [[1]]$est_yield
#>       lower estimate    upper
#> 1 0.8347555  1.16761 1.500465
#> 
#> [[1]]$est_frac
#>       lower  estimate     upper
#> 1 0.5137142 0.5901794 0.6666446
#> 
#> [[1]]$est_metaphases_frac
#>   pi_estimate pi_std_err
#> 1   0.2372438 0.03360268
#> 
#> [[1]]$AIC
#> [1] 8.132597
#> 
#> [[1]]$conf_int
#> [1] 0.95
#> 
#> 
```
