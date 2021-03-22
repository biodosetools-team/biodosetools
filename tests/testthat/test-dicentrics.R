# Dicentrics fitting ----

test_that("get_fit_results with full count data works", {
  # Example from IAEA (2011)
  dic_count_data <- app_sys("extdata", "count-data-IAEA.csv") %>%
    utils::read.csv() %>%
    calculate_aberr_table(type = "count")

  # Expected outputs
  expect_equal(dic_count_data$N, c(5000L, 5002L, 2008L, 2002L, 1832L, 1168L, 562L, 333L, 193L, 103L, 59L))
  expect_equal(dic_count_data$X, c(8L, 14L, 22L, 55L, 100L, 109L, 100L, 103L, 108L, 103L, 107L))
  expect_equal(which(unname(dic_count_data$u) > 1.96), c(3))

  # Fitting (glm)
  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"
  aberr_module <- "dicentrics"

  fit_results_list <- get_fit_results(
    count_data = dic_count_data,
    model_formula,
    model_family,
    fit_link = "identity",
    aberr_module
  )

  gg_curve <- get_fit_dose_curve(
    fit_results_list,
    aberr_name = to_title(aberr_module)
  )

  # Expected outputs
  expect_gt(ncol(dic_count_data), 3)
  expect_equal(fit_results_list$fit_raw_data, as.matrix(dic_count_data))
  expect_equal(fit_results_list$fit_algorithm, "glm")
  expect_equal(round(unname(fit_results_list$fit_model_statistics[, "logLik"]), 2), -4.66)
  expect_equal(gg_curve$data$dose, dic_count_data$D)

  # Fitting (maxlik)
  model_formula <- list_fitting_formulas()[[2]]

  # Expected glm warning in tryCatch()
  expect_warning(
    get_fit_results(
      count_data = dic_count_data,
      model_formula,
      model_family,
      fit_link = "identity",
      aberr_module
    )
  )
})

test_that("get_fit_results with aggregated count data works", {
  # Example from IAEA (2011)
  dic_count_data <- app_sys("extdata", "count-data-aggr-IAEA.csv") %>%
    utils::read.csv() %>%
    dplyr::mutate(
      D = as.numeric(.data$D)
    )

  # Expected outputs
  expect_equal(ncol(dic_count_data), 3)
  expect_equal(dic_count_data$N, c(5000L, 5002L, 2008L, 2002L, 1832L, 1168L, 562L, 333L, 193L, 103L, 59L))
  expect_equal(dic_count_data$X, c(8L, 14L, 22L, 55L, 100L, 109L, 100L, 103L, 108L, 103L, 107L))

  # Fitting (glm)
  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"
  aberr_module <- "dicentrics"

  fit_results_list <- get_fit_results(
    count_data = dic_count_data,
    model_formula,
    model_family,
    fit_link = "identity",
    aberr_module
  )

  gg_curve <- get_fit_dose_curve(
    fit_results_list,
    aberr_name = to_title(aberr_module)
  )

  # Expected outputs
  expect_equal(fit_results_list$fit_raw_data, as.matrix(dic_count_data))
  expect_equal(fit_results_list$fit_algorithm, "glm")
  expect_equal(round(unname(fit_results_list$fit_model_statistics[, "logLik"]), 2), -4.66)
  expect_equal(gg_curve$data$dose, dic_count_data$D)

  # Fitting (maxlik)
  model_formula <- list_fitting_formulas()[[2]]

  # Expected glm warning in tryCatch()
  expect_warning(
    get_fit_results(
      count_data = dic_count_data,
      model_formula,
      model_family,
      fit_link = "identity",
      aberr_module
    )
  )
})


# Dicentrics dose estimation ----

test_that("processing case data works", {
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

  # Specific to dicentrics/micronuclei
  case_data <- case_data %>%
    dplyr::mutate(
      y = .data$mean,
      y_err = .data$std_err
    ) %>%
    dplyr::select(-.data$mean, -.data$std_err)

  # Colnames validation
  case_data_cols <- colnames(case_data)
  case_data_cols_len <- length(case_data_cols)

  # Expected outcomes
  expect_equal(case_data_cols[1:2], c("N", "X"))
  expect_true(all(grepl("C", case_data_cols[seq(3, case_data_cols_len - 4, 1)])))
  expect_equal(case_data_cols[seq(case_data_cols_len - 3, case_data_cols_len, 1)], c("DI", "u", "y", "y_err"))

  # Dose estimation
  exposure <- "acute"
  assessment <- "whole-body"
  error_method <- "merkle-83"

  fit_results_list <- app_sys("extdata", "dicentrics-fitting-data-2020-10-10.rds") %>%
    readRDS()

  # Parse fitting data
  fit_coeffs <- fit_results_list[["fit_coeffs"]]
  fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]
  fit_formula_tex <- fit_results_list[["fit_formula_tex"]]

  # Generalised fit coefficients
  general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])

  # Generalised variance-covariance matrix
  general_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

  # Protracted variables
  if (exposure == "protracted") {
    # protracted_time <-
    # protracted_life_time <-
    protracted_g_value <- protracted_g_function(protracted_time, protracted_life_time)
  } else if (exposure == "protracted_high") {
    protracted_g_value <- 0
    # Used in report (dummy values)
    protracted_time <- NA
    protracted_life_time <- NA
  } else {
    protracted_g_value <- 1
    # Used in report (dummy values)
    protracted_time <- NA
    protracted_life_time <- NA
  }

  # Confidence intervals

  # Select whole-body CI depending on selected method
  if (grepl("merkle", error_method, fixed = TRUE)) {
    conf_int_curve <- paste0("0.", gsub("\\D", "", error_method)) %>% as.numeric()
    conf_int_yield <- conf_int_curve

    # Parse CI text for plot legend
    conf_int_text_whole <- paste0(
      "(", round(100 * conf_int_curve, 0), "%",
      "-", round(100 * conf_int_yield, 0), "%", ")"
    )
  } else if (error_method == "delta") {
    conf_int_curve <- 0.83
    conf_int_yield <- conf_int_curve
    conf_int_delta <- 0.95

    # Parse CI text for plot legend
    conf_int_text_whole <- paste0("(", round(100 * conf_int_delta, 0), "%", ")")
  }

  conf_int_curve <- conf_int_curve %>%
    correct_conf_int(general_var_cov_mat, protracted_g_value, type = "curve")
  conf_int_yield <- conf_int_yield %>%
    correct_conf_int(general_var_cov_mat, protracted_g_value, type = "yield")

  # Parse genome fraction
  parsed_genome_fraction <- 1

  # Calculations
  results_whole <- estimate_whole_body(
    case_data,
    general_fit_coeffs,
    general_var_cov_mat,
    conf_int_yield,
    conf_int_curve,
    protracted_g_value,
    parsed_genome_fraction,
    aberr_module = "dicentrics"
  )

  # Expected outputs
  expect_equal(colnames(results_whole$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_whole$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_whole$AIC, 3), 7.771)

})
