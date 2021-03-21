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
})
