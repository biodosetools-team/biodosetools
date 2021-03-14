test_that("get_translocation_rate works", {
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

  expect_lt(default_rate, other_rate)
})
