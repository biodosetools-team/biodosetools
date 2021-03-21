# Basic utils ----

test_that("to_title works", {
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

test_that("parse_model_formula works", {
  # Correct argument matching
  expect_error(parse_model_formula("lon-quod"))

  # Expected names
  parsed_model_formula <-parse_model_formula("lin-quad")
  expect_equal(parsed_model_formula$fit_formula_raw, "aberr ~ -1 + coeff_C + coeff_alpha + coeff_beta")
  expect_equal(parsed_model_formula$fit_formula_tex, "Y = C + \\alpha D + \\beta D^{2}")

  parsed_model_formula <-parse_model_formula("lin")
  expect_equal(parsed_model_formula$fit_formula_raw, "aberr ~ -1 + coeff_C + coeff_alpha")
  expect_equal(parsed_model_formula$fit_formula_tex, "Y = C + \\alpha D")

  parsed_model_formula <-parse_model_formula("lin-quad-no-int")
  expect_equal(parsed_model_formula$fit_formula_raw, "aberr ~ -1 + coeff_alpha + coeff_beta")
  expect_equal(parsed_model_formula$fit_formula_tex, "Y = \\alpha D + \\beta D^{2}")

  parsed_model_formula <-parse_model_formula("lin-no-int")
  expect_equal(parsed_model_formula$fit_formula_raw, "aberr ~ -1 + coeff_alpha")
  expect_equal(parsed_model_formula$fit_formula_tex, "Y = \\alpha D")
})


# Formatting fixes ----

fit_results <- app_sys("extdata", "dicentrics-fitting-data-2020-10-10.rds") %>%
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
