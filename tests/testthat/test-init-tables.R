test_that("init_aberr_table for cases works", {
  num_cases <- 1
  num_aberrs <- as.numeric(5) + 1

  # Base data frame
  full_data <- data.frame(
    matrix(
      0,
      nrow = num_cases,
      ncol = num_aberrs
    )
  ) %>%
    `colnames<-`(paste0("C", seq(0, num_aberrs - 1, 1)))

  full_data <- init_aberr_table(
    data = full_data,
    type = "case",
    aberr_module = "dicentrics"
  )

  # Expected outputs
  expect_equal(names(full_data), c("N", "X", "C0", "C1", "C2", "C3", "C4", "C5", "y", "y_err", "DI", "u"))
  expect_equal(unname(unlist(full_data)), rep(0, num_cases * 12))
  expect_true(all(dim(full_data) == c(num_cases, 12)))
})

test_that("init_aberr_table for translocations case data works", {
  num_cases <- 1
  num_aberrs <- as.numeric(5) + 1

  # Base data frame
  full_data <- data.frame(
    matrix(
      0,
      nrow = num_cases,
      ncol = num_aberrs
    )
  ) %>%
    `colnames<-`(paste0("C", seq(0, num_aberrs - 1, 1)))

  full_data <- init_aberr_table(
    data = full_data,
    type = "case",
    aberr_module = "translocations"
  )

  # Expected outputs
  expect_equal(names(full_data), c("N", "X", "C0", "C1", "C2", "C3", "C4", "C5", "Fp", "Fp_err", "DI", "u", "Xc", "Fg", "Fg_err"))
  expect_equal(unname(unlist(full_data)), rep(0, num_cases * 15))
  expect_true(all(dim(full_data) == c(num_cases, 15)))
})
