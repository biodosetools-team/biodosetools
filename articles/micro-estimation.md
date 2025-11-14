# Micronuclei dose estimation

## Load pre-calculated curve

The first step is to either load the pre-calculated curve in `.rds`
format obtained in the dose-effect fitting module or input the curve
coefficients manually in case the user wants to use a pre-existing curve
calculated outside of Biodose Tools. Clicking on “Preview data” will
load the curve into the app and display it on the “Results” tabbed box.
If coefficients are entered manually, a irradiation conditions tab will
appear to fill with corresponding information.

!['Curve fitting data options' box and 'Results' tabbed box in the dose
estimation module when loading curve from an \`.rds\`
file.](figures/screenshot-micronuclei-estimate-01.png)

‘Curve fitting data options’ box and ‘Results’ tabbed box in the dose
estimation module when loading curve from an `.rds` file.

!['Curve fitting data options' box and 'Results' tabbed box in the dose
estimation module when inputting curve coefficients manually. Note that
if no variance-covariance matrix is provided, only the variances
calculated from the coefficients' standard errors will be used in
calculations.](figures/screenshot-micronuclei-estimate-02.png)

‘Curve fitting data options’ box and ‘Results’ tabbed box in the dose
estimation module when inputting curve coefficients manually. Note that
if no variance-covariance matrix is provided, only the variances
calculated from the coefficients’ standard errors will be used in
calculations.

This step is accomplished in R by either using the results from
[`fit()`](../reference/fit.md) or by loading an existing `.rds` object
via [`readRDS()`](https://rdrr.io/r/base/readRDS.html):

``` r
fit_results <- system.file("extdata", "micronuclei-fitting-results.rds", package = "biodosetools") %>%
  readRDS()
```

``` r
fit_results$fit_coeffs
#>                estimate    std.error statistic      p.value
#> coeff_C     0.022304968 0.0016412463 13.590263 2.747258e-06
#> coeff_alpha 0.027791344 0.0044014447  6.314141 3.987058e-04
#> coeff_beta  0.003332782 0.0009379284  3.553343 9.301982e-03
```

## Input case data

Next we can choose to either load the case data from a file (supported
formats are `.csv`, `.dat`, and `.txt`) or to input the data manually.
Once the table is generated and filled, the “Calculate parameters”
button will calculate the total number of cells ($N$), total number of
aberrations ($X$), as well as mean ($\bar{y}$), standard error
($\sigma$), dispersion index ($\sigma^{2}/\bar{y}$), and $u$-value. An
ID column will also appear to identify each case. Multiple cases are
supported.

!['Data input options' and 'Data input' boxes in the dose estimation
module.](figures/screenshot-micronuclei-estimate-03.png)

‘Data input options’ and ‘Data input’ boxes in the dose estimation
module.

This step is accomplished in R by calling the
[`calculate_aberr_table()`](../reference/calculate_aberr_table.md)
function:

``` r
case_data <- data.frame(
  ID = 'Case1', C0 = 302, C1 = 28, C2 = 22, C3 = 8, C4 = 1
) %>%
  calculate_aberr_table(
    type = "case",
    assessment_u = 1,
    aberr_module = "micronuclei"
  )
```

``` r
case_data
#> # A tibble: 1 × 12
#>   ID        N     X    C0    C1    C2    C3    C4     y  y_err    DI     u
#>   <chr> <int> <int> <int> <int> <int> <int> <int> <dbl>  <dbl> <dbl> <dbl>
#> 1 Case1   361   100   302    28    22     8     1 0.277 0.0368  1.77  10.4
```

## Perform dose estimation

The final step is to select the dose estimation options. In the “Dose
estimation options” box we can select type of exposure (acute,
protracted, and highly protracted), type of assessment (whole-body,
partial-body, or heterogeneous), and error methods for each type of
assessment.

!['Dose estimation options' box in the dose estimation
module.](figures/screenshot-micronuclei-estimate-04.png)

‘Dose estimation options’ box in the dose estimation module.

!['Results' tabbed box, 'Curve plot' and 'Save results' boxes in the
dose estimation module.](figures/screenshot-micronuclei-estimate-05.png)

‘Results’ tabbed box, ‘Curve plot’ and ‘Save results’ boxes in the dose
estimation module.

To perform the dose estimation in R we can call the adequate
`estimate_*()` functions. In this example, we will use
[`estimate_whole_body_merkle()`](../reference/estimate_whole_body_merkle.md)
and
[`estimate_partial_body_dolphin()`](../reference/estimate_partial_body_dolphin.md).
First of all, however, we will need to load the fit coefficients and
variance-covariance matrix:

``` r
fit_coeffs <- fit_results[["fit_coeffs"]]
fit_var_cov_mat <- fit_results[["fit_var_cov_mat"]]
```

Since we have a protracted exposure, we need to calculate the value of
$G(x)$:

``` r
protracted_g_value <- protracted_g_function(
  time = 0.5,
  time_0 = 2
)
```

``` r
protracted_g_value
#> [1] 0.9216251
```

After that is done, we can simply call
[`estimate_whole_body_merkle()`](../reference/estimate_whole_body_merkle.md):

``` r
results_whole_merkle <- estimate_whole_body_merkle(
  num_cases = 1,
  case_data,
  fit_coeffs,
  fit_var_cov_mat,
  conf_int_yield = 0.95,
  conf_int_curve = 0.95,
  protracted_g_value,
  aberr_module = "micronuclei"
)
```

To visualize the estimated doses, we call the
[`plot_estimated_dose_curve()`](../reference/plot_estimated_dose_curve.md)
function:

``` r
plot_estimated_dose_curve(
  est_doses = list(whole = results_whole_merkle),
  fit_coeffs,
  fit_var_cov_mat,
  protracted_g_value = 1,
  conf_int_curve = 0.95,
  aberr_name = "Micronuclei",
  place = "UI"
)
```

![Plot of estimated doses generated by \\biodosetools\\. The grey
shading indicates the uncertainties associated with the calibration
curve.](micro-estimation_files/figure-html/micro-estimated-dose-curve-1.png)

Plot of estimated doses generated by {biodosetools}. The grey shading
indicates the uncertainties associated with the calibration curve.
