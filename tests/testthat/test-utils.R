# Basic utils ----

test_that("to title works", {
  expect_equal(
    to_title("hello world"),
    "Hello World"
  )
})

test_that("names_from_model_formula works", {
  # Correct argument matching
  expect_error(names_from_model_formula("lon-quod"))

  # Expected names
  expect_equal(
    names_from_model_formula("lin-quad"),
    c("coeff_C", "coeff_alpha", "coeff_beta")
  )
  expect_equal(
    names_from_model_formula("lin"),
    c("coeff_C", "coeff_alpha")
  )
  expect_equal(
    names_from_model_formula("lin-quad-no-int"),
    c("coeff_alpha", "coeff_beta")
  )
  expect_equal(
    names_from_model_formula("lin-no-int"),
    c("coeff_alpha")
  )
})


# Formatting fixes ----

fit_results <- system.file("extdata", "dicentrics-fitting-data-2020-10-10.rds", package = "biodosetools") %>%
  readRDS()

test_that("fix_coeff_names works", {
  # Prepare data
  fit_var_cov_mat <- fit_results$fit_var_cov_mat %>%
    fix_coeff_names("rows", "kable") %>%
    fix_coeff_names("cols", "kable")

  # Expected outputs
  expect_equal(
    rownames(fit_var_cov_mat),
    c("$C$", "$\\alpha$", "$\\beta$")
  )
  expect_equal(
    colnames(fit_var_cov_mat),
    c("$C$", "$\\alpha$", "$\\beta$")
  )
})

test_that("fix_count_data_names works", {
  # Prepare data
  count_data <- fit_results$fit_raw_data

  count_data_cols <- fix_count_data_names(count_data, type = "count", output = "kable") %>%
    colnames()
  count_data_cols_len <- length(count_data_cols)

  # Expected outputs
  expect_equal(
    count_data_cols[1:4],
    c("$D$ (Gy)", "$N$", "$X$", "$C_{0}$")
  )
  expect_equal(
    count_data_cols[seq(count_data_cols_len - 3, count_data_cols_len, 1)],
    c("$\\bar{y}$", "$\\sigma^{2}$", "$\\sigma^{2} / \\bar{y}$", "$u$")
  )
})
