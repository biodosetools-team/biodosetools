# Translocations dose-effect fitting

## Calculate genomic conversion factor

To be able to fit the equivalent full genome dose-effect curve, we need
to calculate the genomic conversion factor.

To do this, in the “Stain color options” box we select the sex of the
individual, and the list of chromosomes and stains used for the
translocation assay. Clicking on “Generate table” will show a table in
the “Chromosome data” box in which we select the chromosome-stain pairs.
Clicking on the “Calculate fraction” will calculate the genomic
conversion factor.

!['Stains color options', 'Chromosome data' and 'Genomic conversion
factor' boxes in the dose-effect fitting
module.](figures/screenshot-translocations-fit-01.png)

‘Stains color options’, ‘Chromosome data’ and ‘Genomic conversion
factor’ boxes in the dose-effect fitting module.

To calculate the genomic conversion factor in R we call the
[`calculate_genome_factor()`](../reference/calculate_genome_factor.md)
function:

``` r
genome_factor <- calculate_genome_factor(
  dna_table = dna_content_fractions_morton,
  chromosome = c(1, 4, 11),
  color = rep("Red", 3),
  sex = "female"
)
```

``` r
genome_factor
#> [1] 0.3147797
```

## Input count data

Once the genomic conversion factor has been calculated, we can input the
count data. On the {shiny} app, as in the Dicentrics or Micronuclei
dose-effect fitting modules, we can select to either load the count data
from a file (supported formats are `.csv`, `.dat`, and `.txt`) or to
input the data manually. Once the table is generated and filled, the
“Calculate parameters” button will calculate the total number of cells
($N$), total number of aberrations ($X$), as well as mean ($\bar{y}$),
variance ($\sigma^{2}$), dispersion index ($\sigma^{2}/\bar{y}$), and
$u$-value.

This step is accomplished in R by calling the
[`calculate_aberr_table()`](../reference/calculate_aberr_table.md)
function:

``` r
count_data <- system.file("extdata", "count-data-rodriguez-2004.csv", package = "biodosetools") %>%
  utils::read.csv() %>%
  calculate_aberr_table(type = "count") %>%
  dplyr::mutate(N = N * genome_factor)
```

``` r
count_data
#> # A tibble: 11 × 13
#>        D     N     X    C0    C1    C2    C3    C4    C5    mean     var    DI
#>    <dbl> <dbl> <dbl> <int> <int> <int> <int> <int> <int>   <dbl>   <dbl> <dbl>
#>  1  0    1371.     6  4350     6     0     0     0     0 0.00138 0.00138 0.999
#>  2  0.1  1046.    15  3309    15     0     0     0     0 0.00451 0.00449 0.996
#>  3  0.25  966.    18  3051    18     0     0     0     0 0.00587 0.00583 0.994
#>  4  0.5   967.    33  3039    33     0     0     0     0 0.0107  0.0106  0.990
#>  5  0.75  664.    40  2072    38     1     0     0     0 0.0189  0.0195  1.03 
#>  6  1     669.    50  2075    48     1     0     0     0 0.0235  0.0239  1.02 
#>  7  1.5   328.    56   990    50     3     0     0     0 0.0537  0.0566  1.05 
#>  8  2     226.    71   650    65     3     0     0     0 0.0989  0.0976  0.987
#>  9  3     246.   157   649   108    23     1     0     0 0.201   0.227   1.13 
#> 10  4     125.   147   265   117    15     0     0     0 0.370   0.310   0.836
#> 11  5     124.   180   246   122    23     4     0     0 0.456   0.426   0.936
#> # ℹ 1 more variable: u <dbl>
```

## Irradiation conditions

Because irradiation conditions during calibration may influence future
dose estimates, and for a better traceability, the user can input the
conditions under which the samples used to construct the curve were
irradiated. This option is only available in the Shiny app, so that
these can be saved into the generated reports (For more information see
Dicentrics or Micronuclei dose-effect fitting modules).

## Perform fitting

To perform the fitting the user needs to select the appropriate fitting
options to click the “Calculate fitting” button on the “Data input” box.
The fit can be done either using the full genome translocations, or
those measured by FISH. This will not impact any future dose estimation,
as the results internally use the full genome translocations.

The fitting results and summary statistics are shown in the “Results”
tabbed box, and the dose-effect curve is displayed in the “Curve plot”
box.

The “Export results” box shows two buttons: (a) “Save fitting data”, and
(b) “Download report”. The “Save fitting data” will generate an `.rds`
file that contains all information about the count data, irradiation
conditions, and options selected when performing the fitting. This file
can be then loaded in the dose estimation module to load the dose-effect
curve coefficients.

Similarly, the “Download report” will generate a `.pdf` or a `.docx`
report containing all inputs and fitting results.

!['Results' tabbed box, 'Curve plot' and 'Export results' boxes in the
dose-effect fitting
module.](figures/screenshot-translocations-fit-04.png)

‘Results’ tabbed box, ‘Curve plot’ and ‘Export results’ boxes in the
dose-effect fitting module.

To perform the fitting in R we call the [`fit()`](../reference/fit.md)
function:

``` r
fit_results <- fit(
  count_data = count_data,
  model_formula = "lin-quad",
  model_family = "automatic",
  fit_link = "identity",
  aberr_module = "translocations"
)
```

The `fit_results` object is a list that contains all necessary
information about the count data as well as options selected when
performing the fitting. This is a vital step to ensure traceability and
reproducibility. Below we can see its elements:

``` r
names(fit_results)
#>  [1] "fit_raw_data"         "fit_formula_raw"      "fit_formula_tex"     
#>  [4] "fit_coeffs"           "fit_cor_mat"          "fit_var_cov_mat"     
#>  [7] "fit_dispersion"       "fit_model_statistics" "fit_algorithm"       
#> [10] "fit_model_summary"
```

In particular, we can see how `fit_coeffs` matches the results obtained
in the UI:

``` r
fit_results$fit_coeffs
#>                estimate   std.error statistic      p.value
#> coeff_C     0.006560406 0.002052834  3.195780 1.269259e-02
#> coeff_alpha 0.027197296 0.009922177  2.741061 2.540751e-02
#> coeff_beta  0.057982322 0.004630926 12.520675 1.550057e-06
```

To visualise the dose-effect curve, we call the
[`plot_fit_dose_curve()`](../reference/plot_fit_dose_curve.md) function:

``` r
plot_fit_dose_curve(
  fit_results,
  aberr_name = "Translocations",
  place = "UI"
)
```

![Plot of dose-effect curve generated by \\biodosetools\\. The grey
shading indicates the uncertainties associated with the calibration
curve.](trans-fitting_files/figure-html/trans-fit-dose-curve-1.png)

Plot of dose-effect curve generated by {biodosetools}. The grey shading
indicates the uncertainties associated with the calibration curve.
