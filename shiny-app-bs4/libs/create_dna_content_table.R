library(tidyverse)

# Read RAW data --------------------------------------------

dna <- data.table::fread("shiny-app-bs4/libs/dna-content-table-raw.csv")



# Calculations ---------------------------------------------

# Calculate total DNA content
total_dna <- dna %>%
  summarise(
    male_dna = sum(dna_content * male),
    female_dna = sum(dna_content * female)
  )

# Calculate fraction of DNA
dna <- dna %>%
  dplyr::mutate(
    fraction_male = dna_content * male / total_dna[["male_dna"]],
    fraction_female = dna_content * female / total_dna[["female_dna"]],
    fraction_female = ifelse(fraction_female == 0, NA, fraction_female)
  ) %>%
  dplyr::select(
    c(-dna_content, -male, -female)
  )

# Save into CSV file
write.csv(dna, "shiny-app-bs4/libs/dna-content-fractions.csv", row.names = FALSE)
