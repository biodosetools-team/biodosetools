# Libraries ---------------------------------------------------------------
library(dplyr)


# Read raw data (Morton) --------------------------------------------------
dna_content_table_morton <- data.table::fread("data-raw/dna-content-table-morton-raw.csv")

# Calculate total DNA content
total_dna_morton <- dna_content_table_morton %>%
  summarise(
    male_dna = sum(dna_content * male),
    female_dna = sum(dna_content * female)
  )

# Calculate fraction of DNA
dna_content_fractions_morton <- dna_content_table_morton %>%
  dplyr::mutate(
    fraction_male = dna_content * male / total_dna_morton[["male_dna"]],
    fraction_female = dna_content * female / total_dna_morton[["female_dna"]],
    fraction_female = ifelse(fraction_female == 0, NA, fraction_female)
  ) %>%
  dplyr::select(
    c(-dna_content, -male, -female)
  )


# Read raw data (IHGSC) ---------------------------------------------------
dna_content_table_ihgsc <- data.table::fread("data-raw/dna-content-table-ihgsc-raw.csv")

# Calculate total DNA content
total_dna_ihgsc <- dna_content_table_ihgsc %>%
  summarise(
    male_dna = sum(dna_content * male),
    female_dna = sum(dna_content * female)
  )

# Calculate fraction of DNA
dna_content_fractions_ihgsc <- dna_content_table_ihgsc %>%
  dplyr::mutate(
    fraction_male = dna_content * male / total_dna_ihgsc[["male_dna"]],
    fraction_female = dna_content * female / total_dna_ihgsc[["female_dna"]],
    fraction_female = ifelse(fraction_female == 0, NA, fraction_female)
  ) %>%
  dplyr::select(
    c(-dna_content, -male, -female)
  )


# Export data -------------------------------------------------------------
usethis::use_data(dna_content_fractions_morton, overwrite = TRUE)
usethis::use_data(dna_content_fractions_ihgsc, overwrite = TRUE)
