# Calculate Sigurdson's translocation rate

Method based on the paper by Sigurdson, A. J. et al. (2008).
International study of factors affecting human chromosome
translocations. Mutation Research/Genetic Toxicology and Environmental
Mutagenesis, 652(2), 112-121. \<doi:10.1016/j.mrgentox.2008.01.005\>.

## Usage

``` r
calculate_trans_rate_sigurdson(
  cells,
  genome_factor,
  age_value,
  sex_bool = FALSE,
  sex_value = "none",
  smoker_bool = FALSE,
  ethnicity_value = "none",
  region_value = "none"
)
```

## Arguments

- cells:

  Number of cells `N`.

- genome_factor:

  Genomic conversion factor.

- age_value:

  Age of the individual.

- sex_bool:

  If `TRUE`, `sex_value` will be used.

- sex_value:

  Sex of the individual, either "male" of "female".

- smoker_bool:

  Whether the individual smokes or not.

- ethnicity_value:

  Ethnicity of the individual.

- region_value:

  Region of the individual.

## Value

Numeric value of translocation rate.

## Examples

``` r
calculate_trans_rate_sigurdson(
  cells = 1000,
  genome_factor = 0.58,
  age_value = 30,
  sex_bool = FALSE,
  sex_value = "none",
  smoker_bool = FALSE,
  ethnicity_value = "none",
  region_value = "none"
)
#> [1] 2.432651
```
