# Perform dose-effect fitting algorithm

Perform dose-effect fitting. A generalized linear model (GLM) is used by
default, with a maximum likelihood estimation (MLE) as a fallback
method.

## Usage

``` r
fit(
  count_data,
  model_formula,
  model_family,
  fit_link = "identity",
  aberr_module = c("dicentrics", "translocations", "micronuclei"),
  algorithm = c("glm", "maxlik")
)
```

## Arguments

- count_data:

  Count data in data frame form.

- model_formula:

  Model formula.

- model_family:

  Model family.

- fit_link:

  Family link.

- aberr_module:

  Aberration module.

- algorithm:

  Optional selection of algorithm to be used, either "glm" (for GLM) or
  "maxlik" (for MLE). By default, "glm" is used, with "maxlik" as a
  fallback method.

## Value

List object containing fit results either using GLM or maxLik
optimization.

## Details

The GLM method is based on the paper by Edwards, A. A. et al. (1979).
Radiation induced chromosome aberrations and the Poisson distribution.
Radiation and Environmental Biophysics, 16(2), 89-100.
\<doi:10.1007/BF01323216\>.

The MLE method is based on the paperby Oliveira, M. et al. (2016).
Zero-inflated regression models for radiation-induced chromosome
aberration data: A comparative study. Biometrical Journal, 58(2),
259-279. \<doi:10.1002/bimj.201400233\>.

## Examples

``` r
count_data <- data.frame(
  D = c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5),
  N = c(5000, 5002, 2008, 2002, 1832, 1168, 562, 333, 193, 103, 59),
  X = c(8, 14, 22, 55, 100, 109, 100, 103, 108, 103, 107),
  C0 = c(4992, 4988, 1987, 1947, 1736, 1064, 474, 251, 104, 35, 11),
  C1 = c(8, 14, 20, 55, 92, 99, 76, 63, 72, 41, 19),
  C2 = c(0, 0, 1, 0, 4, 5, 12, 17, 15, 21, 11),
  C3 = c(0, 0, 0, 0, 0, 0, 0, 2, 2, 4, 9),
  C4 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 6),
  C5 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3),
  mean = c(0.0016, 0.0028, 0.0110, 0.0275, 0.0546, 0.0933, 0.178, 0.309, 0.560, 1, 1.81),
  var = c(0.00160, 0.00279, 0.0118, 0.0267, 0.0560, 0.0933, 0.189, 0.353, 0.466, 0.882, 2.09),
  DI = c(0.999, 0.997, 1.08, 0.973, 1.03, 0.999, 1.06, 1.14, 0.834, 0.882, 1.15),
  u = c(-0.0748, -0.135, 2.61, -0.861, 0.790, -0.0176, 1.08, 1.82, -1.64, -0.844, 0.811)
)


fit(count_data = count_data,
    model_formula = "lin-quad",
    model_family = "automatic",
    fit_link = "identity",
    aberr_module = "dicentrics",
    algorithm = "maxlik")
#> $fit_raw_data
#>          D    N   X   C0 C1 C2 C3 C4 C5   mean     var    DI       u
#>  [1,] 0.00 5000   8 4992  8  0  0  0  0 0.0016 0.00160 0.999 -0.0748
#>  [2,] 0.10 5002  14 4988 14  0  0  0  0 0.0028 0.00279 0.997 -0.1350
#>  [3,] 0.25 2008  22 1987 20  1  0  0  0 0.0110 0.01180 1.080  2.6100
#>  [4,] 0.50 2002  55 1947 55  0  0  0  0 0.0275 0.02670 0.973 -0.8610
#>  [5,] 0.75 1832 100 1736 92  4  0  0  0 0.0546 0.05600 1.030  0.7900
#>  [6,] 1.00 1168 109 1064 99  5  0  0  0 0.0933 0.09330 0.999 -0.0176
#>  [7,] 1.50  562 100  474 76 12  0  0  0 0.1780 0.18900 1.060  1.0800
#>  [8,] 2.00  333 103  251 63 17  2  0  0 0.3090 0.35300 1.140  1.8200
#>  [9,] 3.00  193 108  104 72 15  2  0  0 0.5600 0.46600 0.834 -1.6400
#> [10,] 4.00  103 103   35 41 21  4  2  0 1.0000 0.88200 0.882 -0.8440
#> [11,] 5.00   59 107   11 19 11  9  6  3 1.8100 2.09000 1.150  0.8110
#> 
#> $fit_formula_raw
#> [1] "aberr ~ -1 + coeff_C + coeff_alpha + coeff_beta"
#> 
#> $fit_formula_tex
#> [1] "\\lambda = C + \\alpha D + \\beta D^{2}"
#> 
#> $fit_coeffs
#>                estimate    std.error statistic      p.value
#> coeff_C     0.001281389 0.0004354802  2.942473 3.727799e-02
#> coeff_alpha 0.021040447 0.0051768738  4.064315 7.223528e-03
#> coeff_beta  0.063026217 0.0040877861 15.418179 6.226154e-07
#> 
#> $fit_cor_mat
#>                coeff_C coeff_alpha coeff_beta
#> coeff_C      1.0000000  -0.3342101  0.1829917
#> coeff_alpha -0.3342101   1.0000000 -0.7396223
#> coeff_beta   0.1829917  -0.7396223  1.0000000
#> 
#> $fit_var_cov_mat
#>                   coeff_C   coeff_alpha    coeff_beta
#> coeff_C      1.896430e-07 -7.534521e-07  3.257526e-07
#> coeff_alpha -7.534521e-07  2.680002e-05 -1.565185e-05
#> coeff_beta   3.257526e-07 -1.565185e-05  1.670999e-05
#> 
#> $fit_dispersion
#> [1] 1.010601
#> 
#> $fit_model_statistics
#>         logLik deviance df      AIC      BIC
#> [1,] -35.71196 6.809766  8 77.42392 78.61761
#> 
#> $fit_algorithm
#> [1] "constraint-maxlik-optimization"
#> 
#> $fit_model_summary
#> [1] "A quasi-Poisson model accounting for overdispersion was used as the model dispersion (=1.01) > 1."
#> 
```
