# Dicentrics fitting ----

test_that("processing full count data works", {
  # Example from IAEA (2011)
  dic_count_data <- app_sys("extdata", "count-data-IAEA.csv") %>%
    read.csv() %>%
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
  expect_equal(fit_results_list$fit_raw_data, as.matrix(dic_count_data))
  expect_equal(fit_results_list$fit_algorithm, "glm")
  expect_equal(gg_curve$data$dose, dic_count_data$D)

  # Fitting (maxlik)
  model_formula <- list_fitting_formulas()[[2]]

  # Expected outputs
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
