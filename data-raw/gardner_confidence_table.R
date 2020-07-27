# Libraries ---------------------------------------------------------------
library(dplyr)

# Read raw data -----------------------------------------------------------
gardner_confidence_table <- data.table::fread("data-raw/gardner-confidence-table.csv") %>%
  `colnames<-`(c("s_est", "s_low", "s_upp"))


# Export data -------------------------------------------------------------
usethis::use_data(gardner_confidence_table, overwrite = TRUE)
