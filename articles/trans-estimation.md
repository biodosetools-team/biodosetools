# Translocations dose estimation

## Load pre-calculated curve

The first step is to either load the pre-calculated curve in `.rds`
format obtained in the dose-effect fitting module or input the curve
coefficients manually. Clicking on “Preview data” will load the curve
into the app and display it on the “Results” tabbed box. If coefficients
are entered manually, a irradiation conditions tab will appear to fill
with corresponding information.

!['Curve fitting data options' box and 'Results' tabbed box in the dose
estimation module when loading curve from an \`.rds\`
file.](figures/screenshot-translocations-estimate-01.png)

‘Curve fitting data options’ box and ‘Results’ tabbed box in the dose
estimation module when loading curve from an `.rds` file.

!['Curve fitting data options' box and 'Results' tabbed box in the dose
estimation module when inputting curve coefficients manually. Note that
if no variance-covariance matrix is provided, only the variances
calculated from the coefficients' standard errors will be used in
calculations.](figures/screenshot-translocations-estimate-01b.png)

‘Curve fitting data options’ box and ‘Results’ tabbed box in the dose
estimation module when inputting curve coefficients manually. Note that
if no variance-covariance matrix is provided, only the variances
calculated from the coefficients’ standard errors will be used in
calculations.

This step is accomplished in R by either using the results from
[`fit()`](../reference/fit.md) or by loading an existing `.rds` object
via [`readRDS()`](https://rdrr.io/r/base/readRDS.html):

``` r
fit_results <- system.file("extdata", "translocations-fitting-results.rds", package = "biodosetools") %>%
  readRDS()
```

``` r
fit_results$fit_coeffs
#>                estimate   std.error statistic      p.value
#> coeff_C     0.006560406 0.002052834  3.195780 1.269259e-02
#> coeff_alpha 0.027197296 0.009922177  2.741061 2.540751e-02
#> coeff_beta  0.057982322 0.004630926 12.520675 1.550057e-06
```

## Calculate genomic conversion factor

To be able to fit the equivalent full genome case data, we need to
calculate the genomic conversion factor.

To do this, in the “Stain color options” box we select the sex of the
individual, and the list of chromosomes and stains used for the
translocation assay. Clicking on “Generate table” will show a table in
the “Chromosome data” box in which we select the chromosome-stain pairs.
Clicking on the “Calculate fraction” will calculate the genomic
conversion factor.

!['Stains color options', 'Chromosome data' and 'Genomic conversion
factor' boxes in the dose estimation
module.](figures/screenshot-translocations-estimate-02.png)

‘Stains color options’, ‘Chromosome data’ and ‘Genomic conversion
factor’ boxes in the dose estimation module.

To calculate the genomic conversion factor in R we call the
[`calculate_genome_factor()`](../reference/calculate_genome_factor.md)
function:

``` r
genome_factor <- calculate_genome_factor(
  dna_table = dna_content_fractions_morton,
  chromosome = c(1, 2, 3, 4, 5, 6),
  color = c("Red", "Red", "Green", "Red", "Green", "Green"),
  sex = "male"
)
```

``` r
genome_factor
#> [1] 0.5847343
```

## Input case data

Next we can choose to either load the case data from a file (supported
formats are `.csv`, `.dat`, and `.txt`) or to input the data manually.
If needed, the user can select to use confounders (either using
Sigurdson’s method, or by inputting the translocation frequency per
cell). Once the table is generated and filled, the “Calculate
parameters” button will calculate the total number of cells ($N$), total
number of aberrations ($X$), as well as mean (${\bar{F}}_{p}$), standard
error ($\sigma_{p}$), dispersion index ($\sigma^{2}/\bar{y}$),
$u$-value, expected translocation rate ($X_{c}$), full genome mean
(${\bar{F}}_{g}$), and full genome error ($\sigma_{g}$). An ID column
will also appear to identify each case. Multiple cases are supported.

!['Data input options' and 'Data input' boxes in the dose estimation
module.](figures/screenshot-translocations-estimate-03.png)

‘Data input options’ and ‘Data input’ boxes in the dose estimation
module.

This step is accomplished in R by calling the
[`calculate_aberr_table()`](../reference/calculate_aberr_table.md)
function:

``` r
case_data <- data.frame(
  ID = 'Case1', C0 = 288, C1 = 52, C2 = 9, C3 = 1
) %>%
  calculate_aberr_table(
    type = "case",
    assessment_u = 1,
    aberr_module = "translocations"
  ) %>%
  dplyr::mutate(
    Xc = calculate_trans_rate_sigurdson(
      cells = N,
      genome_factor = genome_factor,
      age_value = 30,
      smoker_bool = TRUE
    ),
    Fg = (X - Xc) / (N * genome_factor),
    Fg_err = Fp_err / sqrt(genome_factor)
  )
```

``` r
case_data
#> # A tibble: 1 × 14
#>   ID        N     X    C0    C1    C2    C3    Fp Fp_err    DI     u    Xc    Fg
#>   <chr> <int> <int> <int> <int> <int> <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Case1   350    73   288    52     9     1 0.209 0.0259  1.12  1.64  1.02 0.352
#> # ℹ 1 more variable: Fg_err <dbl>
```

## Perform dose estimation

The final step is to select the dose estimation options. In the “Dose
estimation options” box we can select type of exposure (acute,
protracted or highly protracted), type of assessment (whole-body,
partial-body or heterogeneous), and error methods for each type of
assessment (Merkle’s method or Delta method). Heterogeneous assessment
available for single case only.

!['Dose estimation options' box in the dose estimation
module.](figures/screenshot-translocations-estimate-04.png)

‘Dose estimation options’ box in the dose estimation module.

!['Results' tabbed box, 'Curve plot' and 'Save results' boxes in the
dose estimation
module.](figures/screenshot-translocations-estimate-05.png)

‘Results’ tabbed box, ‘Curve plot’ and ‘Save results’ boxes in the dose
estimation module.

To perform the dose estimation in R we can call the adequate
`estimate_*()` functions. In this example, we will use
[`estimate_whole_body_delta()`](../reference/estimate_whole_body_delta.md).
First of all, however, we will need to load the fit coefficients and
variance-covariance matrix:

``` r
fit_coeffs <- fit_results[["fit_coeffs"]]
fit_var_cov_mat <- fit_results[["fit_var_cov_mat"]]
```

``` r
results_whole_merkle <- estimate_whole_body_merkle(
  num_cases = 1,
  case_data,
  fit_coeffs,
  fit_var_cov_mat,
  conf_int_yield = 0.83, 
  conf_int_curve = 0.83,
  protracted_g_value = 1, 
  genome_factor,
  aberr_module = "translocations"
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
  conf_int_curve = 0.83,
  aberr_name = "Translocations",
  place = "UI"
)
```

![Plot of estimated doses generated by \\biodosetools\\. The grey
shading indicates the uncertainties associated with the calibration
curve.](trans-estimation_files/figure-html/trans-estimated-dose-curve-1.png)

Plot of estimated doses generated by {biodosetools}. The grey shading
indicates the uncertainties associated with the calibration curve.
