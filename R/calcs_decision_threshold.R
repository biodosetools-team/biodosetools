#' Calculate decision thresholds
#'
#' @param fit_results_list List of fit results
#' @param cells N
#' @param conf_int CI
#' @param aberr_module Aberration module
#' @param input UI input variable
#'
#' @return A vector with aberr_test and dose_est
#' @noRd
calculate_decision_threshold <- function(fit_results_list, cells, conf_int = 0.95, aberr_module, input) {

  # Use measured translocation frequency fit
  if (aberr_module == "translocations") {
    if (input$frequency_select == "full_gen_freq") {
      count_data <- rhandsontable::hot_to_r(input$count_data_hot)
      model_formula <- input$formula_select
      model_family <- input$family_select

      fit_results_list <- get_fit_results(count_data, model_formula, model_family, fit_link = "identity")
    }
  }

  # Summarise fit
  fit_coeffs <- fit_results_list[["fit_coeffs"]]
  fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]

  # Generalized variance-covariance matrix
  general_fit_coeffs <- numeric(length = 3L) %>%
    `names<-`(c("coeff_C", "coeff_alpha", "coeff_beta"))

  for (var in rownames(fit_coeffs)) {
    general_fit_coeffs[var] <- fit_coeffs[var, "estimate"]
  }

  # Generalized fit coefficients
  general_var_cov_mat <- matrix(0, nrow = 3, ncol = 3) %>%
    `row.names<-`(c("coeff_C", "coeff_alpha", "coeff_beta")) %>%
    `colnames<-`(c("coeff_C", "coeff_alpha", "coeff_beta"))

  for (x_var in rownames(fit_var_cov_mat)) {
    for (y_var in colnames(fit_var_cov_mat)) {
      general_var_cov_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var]
    }
  }

  # Generalized curves
  yield_fun <- function(d, G) {
    general_fit_coeffs[[1]] +
      general_fit_coeffs[[2]] * d +
      general_fit_coeffs[[3]] * d^2 * G
  }

  # R factor depeding on selected CI
  chisq_df <- nrow(fit_coeffs)
  R_factor <- function(conf_int = 0.95) {
    sqrt(stats::qchisq(conf_int, df = chisq_df))
  }

  yield_error_fun <- function(d, G) {
    res <- general_var_cov_mat[["coeff_C", "coeff_C"]] +
      general_var_cov_mat[["coeff_alpha", "coeff_alpha"]] * d^2 +
      general_var_cov_mat[["coeff_beta", "coeff_beta"]] * d^4 * G^2 +
      2 * general_var_cov_mat[["coeff_C", "coeff_alpha"]] * d +
      2 * general_var_cov_mat[["coeff_C", "coeff_beta"]] * d^2 * G +
      2 * general_var_cov_mat[["coeff_alpha", "coeff_beta"]] * d^3 * G
    if (sum(res < 0) > 0) {
      rep(0, length(res))
    } else {
      return(sqrt(res))
    }
  }

  dose <- 0.0
  aberr_min <- (yield_fun(dose, 1) + R_factor(conf_int) * yield_error_fun(dose, 1)) * cells

  found <- FALSE
  aberr_test <- 0
  while (!found) {
    aberr_test <- aberr_test + 1
    aberr_low <- stats::poisson.test(x = aberr_test, conf.level = conf_int)[["conf.int"]][1]

    if (aberr_low >= aberr_min) {
      found <- TRUE
    }
  }

  # Estimate dose
  project_yield_estimate <- function(yield, conf_int) {
    stats::uniroot(function(dose) {
      yield_fun(dose, 1) - yield
    }, c(1e-16, 100))$root
  }

  yield_est <- aberr_test / cells
  dose_est <- project_yield_estimate(yield_est, conf_int)

  return(c(aberr_test, dose_est))
}

#' Calculate decision thresholds table
#'
#' @param fit_results_list List of fit results
#' @param aberr_module Aberration module
#' @param input UI input variable
#'
#' @return A vector with aberr_test and dose_est
#' @noRd
#' @importFrom rlang .data
calculate_decision_threshold_table <- function(fit_results_list, decision_threshold_cells, aberr_module, input) {
  decision_threshold <- data.frame(
    N = decision_threshold_cells %>%
      strsplit(" ") %>%
      unlist() %>%
      as.numeric()
  )

  decision_threshold <- decision_threshold %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      X95 = calculate_decision_threshold(fit_results_list, cells = .data$N, conf_int = 0.95, aberr_module, input)[1],
      D95 = calculate_decision_threshold(fit_results_list, cells = .data$N, conf_int = 0.95, aberr_module, input)[2] * 1000,
      X83 = calculate_decision_threshold(fit_results_list, cells = .data$N, conf_int = 0.83, aberr_module, input)[1],
      D83 = calculate_decision_threshold(fit_results_list, cells = .data$N, conf_int = 0.83, aberr_module, input)[2] * 1000
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c(.data$N, grep("X", names(.), value = TRUE)),
        .fns = as.integer
      )
    )

  return(decision_threshold)
}
