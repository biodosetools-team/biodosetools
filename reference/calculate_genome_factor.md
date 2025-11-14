# Calculate genomic conversion factor

Method based on the paper by Lucas, J. N. et al. (1992). Rapid
Translocation Frequency Analysis in Humans Decades after Exposure to
Ionizing Radiation. International Journal of Radiation Biology, 62(1),
53-63. \<doi:10.1080/09553009214551821\>.

## Usage

``` r
calculate_genome_factor(dna_table, chromosomes, colors, sex)
```

## Arguments

- dna_table:

  DNA content fractions table. Can be `dna_content_fractions_morton` or
  `dna_content_table_ihgsc`.

- chromosomes:

  Vector of stained chromosomes.

- colors:

  Vector of colors of the stains.

- sex:

  Sex of the individual.

## Value

Numeric value of genomic conversion factor.

## Examples

``` r
#dna_table can be: dna_content_fractions_morton OR dna_content_fractions_ihgsc

calculate_genome_factor(
  dna_table = dna_content_fractions_morton,
  chromosome = c(1, 2, 3, 4, 5, 6),
  color = c("Red", "Red", "Green", "Red", "Green", "Green"),
  sex = "male"
)
#> [1] 0.5847343
```
