library(tidyverse)


# Read data --------------------------------------------

dna_table <- data.table::fread("shiny-app-bs4/libs/dna-content-fractions.csv")


# Calculate fraction ---------------------------------------

get_fraction <- function(dna_table, chromosome, color, sex) {
  # Construct color/chromosome table
  color_table <-
    cbind(
      color,
      chromosome
    ) %>%
    as.data.frame() %>%
    mutate(
      chromosome = as.character(chromosome)
    )

  # Full table
  full_table <- inner_join(color_table, dna_table, by = "chromosome") %>%
    group_by(color) %>%
    summarise(frac = sum(base::get(paste0("fraction_", sex))))

  # Calculate first sum
  single_sum <- full_table %>%
    dplyr::select(frac) %>%
    summarise(sum(frac * (1 - frac))) %>%
    unname() %>%
    unlist()

  # Calculate second sum
  if (nrow(full_table) >= 2) {
    cross_sum <- full_table[["frac"]] %>%
      combn(2) %>%
      t() %>%
      as.data.frame() %>%
      summarise(sum(V1 * V2)) %>%
      unname() %>%
      unlist()
  } else {
    cross_sum <- 0
  }

  return(2 / 0.974 * (single_sum - cross_sum))
}


# Test function --------------------------------------------

# From Excel sheet
get_fraction(
  dna_table,
  c(1, 4),
  c("red", "green"),
  "male"
)

get_fraction(
  dna_table,
  c(1, 4),
  c("red", "red"),
  "male"
)

# From IAEA
get_fraction(
  dna_table,
  c(1, 2, 4, 3, 5, 6),
  c(rep("red", 3), rep("green", 3)),
  "female"
)

get_fraction(
  dna_table,
  c(1, 2, 4),
  c(rep("red", 3)),
  "female"
)
