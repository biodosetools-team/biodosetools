test_that("processing full count data works", {
  # Example from IAEA (2011)
  dic_count_data <- app_sys("extdata", "count-data-IAEA.csv") %>%
    data.table::fread()

  dic_count_data <- dic_count_data %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      N = as.integer(rowSums(.[grep("C", names(.))])),
      X = calculate_aberr_power(., power = 1),
      X2 = calculate_aberr_power(., power = 2),
      mean = calculate_aberr_mean(X, N),
      var = calculate_aberr_var(X, X2, N),
      DI = calculate_aberr_disp_index(mean, var),
      u = calculate_aberr_u_value(X, N, mean, var, assessment_u = 1.00)
    ) %>%
    dplyr::select(-.data$X2) %>%
    dplyr::select(.data$D, .data$N, .data$X, dplyr::everything())

  # Expected outputs
  expect_equal(dic_count_data$N, c(5000L, 5002L, 2008L, 2002L, 1832L, 1168L, 562L, 333L, 193L, 103L, 59L))
  expect_equal(dic_count_data$X, c(8L, 14L, 22L, 55L, 100L, 109L, 100L, 103L, 108L, 103L, 107L))
  expect_equal(which(unname(dic_count_data$u) > 1.96), c(3))
})
