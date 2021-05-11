# Translocation utils ----

test_that("get_translocation_rate_sigurdson works", {
  # Default parameters for confounders
  default_rate <- get_translocation_rate_sigurdson(
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
  other_rate <- get_translocation_rate_sigurdson(
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

test_that("get_translocation_rate_manual works", {
  # Translocation frequency per cell
  rate <- get_translocation_rate_manual(
    cells = 100,
    genome_fraction = 0.3,
    expected_aberr_value = 0.00339
  )

  # Expected output
  expect_equal(round(rate, 3), 0.102)
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


# Translocations fitting ----

test_that("get_fit_results with full count data works", {
  # Example from IAEA (2011)
  trans_count_data <- app_sys("extdata", "count-data-IAEA.csv") %>%
    utils::read.csv() %>%
    calculate_aberr_table(type = "count")

  # Expected outputs
  expect_equal(trans_count_data$N, c(5000L, 5002L, 2008L, 2002L, 1832L, 1168L, 562L, 333L, 193L, 103L, 59L))
  expect_equal(trans_count_data$X, c(8L, 14L, 22L, 55L, 100L, 109L, 100L, 103L, 108L, 103L, 107L))
  expect_equal(which(unname(trans_count_data$u) > 1.96), c(3))

  # Fitting (glm)
  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"
  aberr_module <- "translocations"

  fit_results_list <- get_fit_results(
    count_data = trans_count_data,
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
  expect_gt(ncol(trans_count_data), 3)
  expect_equal(fit_results_list$fit_raw_data, as.matrix(trans_count_data))
  expect_equal(fit_results_list$fit_algorithm, "glm")
  expect_equal(round(unname(fit_results_list$fit_model_statistics[, "logLik"]), 2), -4.66)
  expect_equal(gg_curve$data$dose, trans_count_data$D)

  # Fitting (maxlik)
  model_formula <- list_fitting_formulas()[[2]]

  # Expected glm warning in tryCatch()
  expect_warning(
    get_fit_results(
      count_data = trans_count_data,
      model_formula,
      model_family,
      fit_link = "identity",
      aberr_module
    )
  )
})

test_that("get_fit_results with aggregated count data works", {
  # Example from IAEA (2011)
  trans_count_data <- app_sys("extdata", "count-data-aggr-IAEA.csv") %>%
    utils::read.csv() %>%
    dplyr::mutate(
      D = as.numeric(.data$D)
    )

  # Expected outputs
  expect_equal(ncol(trans_count_data), 3)
  expect_equal(trans_count_data$N, c(5000L, 5002L, 2008L, 2002L, 1832L, 1168L, 562L, 333L, 193L, 103L, 59L))
  expect_equal(trans_count_data$X, c(8L, 14L, 22L, 55L, 100L, 109L, 100L, 103L, 108L, 103L, 107L))

  # Fitting (glm)
  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"
  aberr_module <- "translocations"

  fit_results_list <- get_fit_results(
    count_data = trans_count_data,
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
  expect_equal(fit_results_list$fit_raw_data, as.matrix(trans_count_data))
  expect_equal(fit_results_list$fit_algorithm, "glm")
  expect_true(all(dim(fit_results_list$fit_cor_mat) == c(3, 3)))
  expect_true(all(dim(fit_results_list$fit_coeffs) == c(3, 4)))
  expect_equal(round(unname(fit_results_list$fit_model_statistics[, "logLik"]), 2), -4.66)
  expect_equal(gg_curve$data$dose, trans_count_data$D)

  # Fitting (maxlik)
  model_formula <- list_fitting_formulas()[[2]]

  # Expected glm warning in tryCatch()
  expect_warning(
    get_fit_results(
      count_data = trans_count_data,
      model_formula,
      model_family,
      fit_link = "identity",
      aberr_module
    )
  )
})


# Translocations dose estimation ----

test_that("processing case data works", {
  case_data <- app_sys("extdata", "cases-data-hetero.csv") %>%
    utils::read.csv(header = TRUE)

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
        # "sigurdson" ~ get_translocation_rate_sigurdson(...),
        # "manual" ~ get_translocation_rate_manual(...),
        TRUE ~ 0
      ),
      Fg = (.data$X - .data$Xc) / (.data$N * genome_fraction),
      Fg_err = .data$Fp_err / sqrt(genome_fraction)
    )

  # Colnames validation
  case_data_cols <- colnames(case_data)
  case_data_cols_len <- length(case_data_cols)

  # Expected outcomes
  expect_equal(case_data_cols[1:2], c("N", "X"))
  expect_true(all(grepl("C", case_data_cols[seq(3, case_data_cols_len - 7, 1)])))
  expect_equal(case_data_cols[seq(case_data_cols_len - 6, case_data_cols_len, 1)], c("DI", "u", "Fp", "Fp_err", "Xc", "Fg", "Fg_err"))

  # Dose estimation
  aberr_module <- "translocations"

  fit_results_list <- app_sys("extdata", "translocations-fitting-data-2020-10-10.rds") %>%
    readRDS()

  # Parse fitting data
  fit_coeffs <- fit_results_list[["fit_coeffs"]]
  fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]
  fit_formula_tex <- fit_results_list[["fit_formula_tex"]]

  # Generalised fit coefficients
  general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])

  # Generalised variance-covariance matrix
  general_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

  # Protraction (acute exposure)
  protracted_g_value <- 1

  # CI: Whole-body assessment (Merkle's method)
  conf_int_curve_merkle <- 0.83
  conf_int_yield_merkle <- conf_int_curve_merkle
  conf_int_text_whole <- paste0(
    "(", round(100 * conf_int_curve_merkle, 0), "%",
    "-", round(100 * conf_int_yield_merkle, 0), "%", ")"
  )

  # CI: Whole-body assessment (Delta method)
  conf_int_curve_delta <- 0.83
  conf_int_yield_delta <- conf_int_curve_delta
  conf_int_delta <- 0.95
  conf_int_text_whole <- paste0("(", round(100 * conf_int_delta, 0), "%", ")")

  # CI: Partial-body assessment
  conf_int_dolphin <- 0.95
  conf_int_text_partial <- paste0("(", round(100 * conf_int_dolphin, 0), "%", ")")

  # CI: corrections (Merkle's method)
  conf_int_curve_merkle <- conf_int_curve_merkle %>%
    correct_conf_int(general_var_cov_mat, protracted_g_value, type = "curve")
  conf_int_yield_merkle <- conf_int_yield_merkle %>%
    correct_conf_int(general_var_cov_mat, protracted_g_value, type = "yield")

  # CI: corrections (Delta method)
  conf_int_curve_delta <- conf_int_curve_delta %>%
    correct_conf_int(general_var_cov_mat, protracted_g_value, type = "curve")
  conf_int_yield_delta <- conf_int_yield_delta %>%
    correct_conf_int(general_var_cov_mat, protracted_g_value, type = "yield")

  # Parse genome fraction
  parsed_genome_fraction <- 0.585

  # Calculations
  results_whole_merkle <- estimate_whole_body(
    case_data,
    general_fit_coeffs,
    general_var_cov_mat,
    conf_int_yield = conf_int_curve_merkle,
    conf_int_curve = conf_int_yield_merkle,
    protracted_g_value,
    parsed_genome_fraction,
    aberr_module
  )

  results_whole_delta <- estimate_whole_body_delta(
    case_data,
    general_fit_coeffs,
    general_var_cov_mat,
    conf_int = conf_int_delta,
    protracted_g_value,
    cov = TRUE,
    aberr_module
  )

  results_partial <- estimate_partial_dolphin(
    case_data,
    general_fit_coeffs,
    general_var_cov_mat,
    conf_int = conf_int_dolphin,
    protracted_g_value,
    cov = TRUE,
    genome_fraction = parsed_genome_fraction,
    aberr_module,
    gamma = 1 / 2.7
  )

  # Expected outputs (whole-body)
  expect_equal(colnames(results_whole_merkle$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_whole_merkle$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_whole_merkle$AIC, 3), 8.223)

  expect_equal(colnames(results_whole_delta$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_whole_delta$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_whole_delta$AIC, 3), 8.223)

  expect_equal(results_whole_merkle$est_doses["estimate", "yield"], results_whole_delta$est_doses["estimate", "yield"])
  # TODO: check theory
  # expect_lt(results_whole_merkle$est_doses["lower", "yield"], results_whole_delta$est_doses["lower", "yield"])
  # expect_lt(results_whole_merkle$est_doses["upper", "yield"], results_whole_delta$est_doses["upper", "yield"])

  # Expected outputs (partial-body)
  expect_equal(colnames(results_partial$est_doses), c("yield", "dose"))
  expect_equal(rownames(results_partial$est_doses), c("lower", "estimate", "upper"))
  expect_equal(round(results_partial$AIC, 3), 8.838)
})
