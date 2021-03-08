test_that("names_from_formula works", {
  # Correct argument matching
  expect_error(names_from_formula("lon-quod"))
  # Output class
  expect_type(names_from_formula("lin-quad"), "character")
})

test_that("fix_coeff_names works", {
  # Expected output
  fit_coeffs <- system.file("extdata", "dicentrics-fitting-data-2020-10-10.rds", package = "biodosetools") %>%
    readRDS() %>%
    .$fit_coeffs

  fit_coeffs <- fix_coeff_names(fit_coeffs, "rows", "kable")

  expect_equal(rownames(fit_coeffs), c("$C$", "$\\alpha$", "$\\beta$"))
})

test_that("fix_count_data_names works", {
  # Expected output
  count_data <- system.file("extdata", "dicentrics-fitting-data-2020-10-10.rds", package = "biodosetools") %>%
    readRDS() %>%
    .$fit_raw_data

  cols <- fix_count_data_names(count_data, type = "count", output = "kable") %>%
    colnames()

  expect_equal(cols[1:4], c("$D$ (Gy)", "$N$", "$X$", "$C_{0}$"))
  expect_equal(cols[(seq(length(cols) - 3, length(cols), 1))],c("$\\bar{y}$", "$\\sigma^{2}$", "$\\sigma^{2} / \\bar{y}$", "$u$"))
})

test_that("to title works", {
  expect_equal(to_title("hello world"), "Hello World")
})
