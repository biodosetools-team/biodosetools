# Libraries ---------------------------------------------------------------
library(dplyr)

# Read raw data -----------------------------------------------------------
dna_content_table <- data.table::fread("data-raw/dna-content-table-raw.csv")

# Calculate total DNA content
total_dna <- dna_content_table %>%
  summarise(
    male_dna = sum(dna_content * male),
    female_dna = sum(dna_content * female)
  )

# Calculate fraction of DNA
dna_content_fractions <- dna_content_table %>%
  dplyr::mutate(
    fraction_male = dna_content * male / total_dna[["male_dna"]],
    fraction_female = dna_content * female / total_dna[["female_dna"]],
    fraction_female = ifelse(fraction_female == 0, NA, fraction_female)
  ) %>%
  dplyr::select(
    c(-dna_content, -male, -female)
  )


# Export data -------------------------------------------------------------
usethis::use_data(dna_content_fractions, overwrite = TRUE)
