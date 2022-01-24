# Decision thresholds ----

test_that("calculate_decision_threshold_* for dicentrics work", {
  fit_results_list <- app_sys("extdata", "dicentrics-fitting-results.rds") %>%
    readRDS()

  count_data <- app_sys("extdata", "count-data-barquinero-1995.csv") %>%
    utils::read.csv() %>%
    calculate_aberr_table(type = "count")

  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"

  decision_threshold <- calculate_decision_threshold_table(
    fit_results_list,
    decision_threshold_cells = "100 500",
    count_data, model_formula, model_family,
    frequency_select = NULL,
    aberr_module = "dicentrics"
  )

  # Expected outputs
  expect_equal(dim(decision_threshold), c(2, 5))
  expect_equal(decision_threshold$X95, c(3, 5))
  expect_equal(decision_threshold$X83, c(2, 4))
  expect_equal(round(decision_threshold$D95, 0), c(528, 241))
  expect_equal(round(decision_threshold$D83, 0), c(403, 200))
})

test_that("calculate_decision_threshold_* for translocations work", {
  fit_results_list <- app_sys("extdata", "translocations-fitting-results.rds") %>%
    readRDS()

  count_data <- app_sys("extdata", "count-data-barquinero-1995.csv") %>%
    utils::read.csv() %>%
    calculate_aberr_table(type = "count")

  model_formula <- list_fitting_formulas()[[1]]
  model_family <- "automatic"

  decision_threshold <- calculate_decision_threshold_table(
    fit_results_list,
    decision_threshold_cells = "100 500",
    count_data, model_formula, model_family,
    frequency_select = "full_gen_freq",
    aberr_module = "translocations"
  )

  # Expected outputs
  expect_equal(dim(decision_threshold), c(2, 5))
  expect_equal(decision_threshold$X95, c(3, 5))
  expect_equal(decision_threshold$X83, c(2, 4))
  expect_equal(round(decision_threshold$D95, 0), c(528, 241))
  expect_equal(round(decision_threshold$D83, 0), c(403, 200))
})
