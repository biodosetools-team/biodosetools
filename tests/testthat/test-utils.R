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
  parsed_model_formula <- parse_model_formula("lin-quad")
  expect_equal(parsed_model_formula$fit_formula_raw, "aberr ~ -1 + coeff_C + coeff_alpha + coeff_beta")
  expect_equal(parsed_model_formula$fit_formula_tex, "Y = C + \\alpha D + \\beta D^{2}")

  parsed_model_formula <- parse_model_formula("lin")
  expect_equal(parsed_model_formula$fit_formula_raw, "aberr ~ -1 + coeff_C + coeff_alpha")
  expect_equal(parsed_model_formula$fit_formula_tex, "Y = C + \\alpha D")

  parsed_model_formula <- parse_model_formula("lin-quad-no-int")
  expect_equal(parsed_model_formula$fit_formula_raw, "aberr ~ -1 + coeff_alpha + coeff_beta")
  expect_equal(parsed_model_formula$fit_formula_tex, "Y = \\alpha D + \\beta D^{2}")

  parsed_model_formula <- parse_model_formula("lin-no-int")
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

test_that("fix_count_data_names for count data works", {
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

test_that("fix_count_data_names for case data works", {
  # Prepare data
  case_data <- app_sys("extdata", "cases-data-hetero.csv") %>%
    utils::read.csv(header = TRUE) %>%
    dplyr::rename_with(
      .fn = toupper,
      .cols = dplyr::everything()
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with("C"),
        .fns = as.integer
      )
    ) %>%
    dplyr::select(
      dplyr::starts_with("C")
    )

  case_data <- calculate_aberr_table(
    data = case_data,
    type = "case",
    assessment_u = 1
  )

  # Specific to translocations
  genome_fraction <- 0.585

  case_data <- case_data %>%
    dplyr::mutate(
      Fp = .data$mean,
      Fp_err = .data$std_err
    ) %>%
    dplyr::select(-.data$mean, -.data$std_err) %>%
    dplyr::mutate(
      Xc = dplyr::case_when(
        # "sigurdson" ~ calculate_trans_rate_sigurdson(...),
        # "manual" ~ calculate_trans_rate_manual(...),
        TRUE ~ 0
      ),
      Fg = (.data$X - .data$Xc) / (.data$N * genome_fraction),
      Fg_err = .data$Fp_err / sqrt(genome_fraction)
    )

  case_data_cols <- fix_count_data_names(case_data, type = "case", output = "kable") %>%
    colnames()
  case_data_cols_len <- length(case_data_cols)

  # Expected outputs
  expect_equal(
    case_data_cols[1:3],
    c("$N$", "$X$", "$C_{0}$")
  )
  expect_equal(
    case_data_cols[seq(case_data_cols_len - 6, case_data_cols_len, 1)],
    c("$\\sigma^{2} / \\bar{y}$", "$u$", "$F_{P}$", "$\\sigma_{P} / \\sqrt{N}$", "$X_{C}$", "$F_{G}$", "$\\sigma_{G} / \\sqrt{N}$")
  )
})


# Dose estimation ----

test_that("protracted_g_function works", {
  # Analytical solution
  product_log <- 0.2784645427610738

  # Expected output
  expect_equal(protracted_g_function(time = 4 * (1 + product_log), time_0 = 2), 0.5)
})

test_that("correct_boundary works", {
  # Expected outputs
  expect_equal(correct_boundary(-0.5), 0)
  expect_equal(correct_boundary(0.5), 0.5)
  expect_equal(correct_boundary(1.5), 1)
})
