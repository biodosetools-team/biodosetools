test_that("get_translocation_rate works", {
  # Default parameters for confounders
  default_rate <- get_translocation_rate(
    cells = 100,
    genome_fraction = 0.3,
    age_value = 25,
    sex_bool = FALSE,
    sex_value = "none",
    smoker_bool = FALSE,
    ethnicity_value = "none",
    region_value = "none"
  )

  # Adding confounders will result in a higher rate
  other_rate <- get_translocation_rate(
    cells = 100,
    genome_fraction = 0.3,
    age_value = 25,
    sex_bool = TRUE,
    sex_value = "male",
    smoker_bool = TRUE,
    ethnicity_value = "white",
    region_value = "w-europe"
  )

  # Expected output
  expect_lt(default_rate, other_rate)
})


test_that("get_genome_fraction works", {
  # Example from IAEA (2011)
  genome_fraction <- get_genome_fraction(
    dna_table = dna_content_fractions_morton,
    chromosome = c(1, 2, 4, 3, 5, 6),
    color = c(rep("Red", 3), rep("Green", 3)),
    sex = "male"
  )

  # Expected output
  expect_equal(round(genome_fraction, 3), 0.585)
})
