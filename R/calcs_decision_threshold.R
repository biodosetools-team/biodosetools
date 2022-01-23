#' Calculate decision thresholds
#'
#' @param fit_results_list List of fit results.
#' @param cells Number of cells \code{N}.
#' @param count_data Count data in data frame form.
#' @param model_formula Model formula.
#' @param model_family Model family.
#' @param frequency_select Whether to use measured frequency or full genome frequency.
#' @param conf_int Confidence interval.
#' @param aberr_module Aberration module.
#' @name calculate_decision_threshold
NULL
# > NULL

#' @rdname calculate_decision_threshold
#' @return A vector with \code{aberr_test} and \code{dose_est}.
#' @noRd
calculate_decision_threshold <- function(fit_results_list, cells,
                                         count_data, model_formula, model_family,
                                         frequency_select = c(NULL, "measured_freq", "full_gen_freq"),
                                         conf_int = 0.95, aberr_module) {
  # Use measured translocation frequency fit
  if (aberr_module == "translocations") {
    if (frequency_select == "full_gen_freq") {
      fit_results_list <- fit(count_data, model_formula, model_family, fit_link = "identity", aberr_module)
    }
  }

  # Summarise fit
  fit_coeffs <- fit_results_list[["fit_coeffs"]]
  fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]

  # Generalised fit coefficients
  general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])

  # Generalised variance-covariance matrix
  general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

  # Generalised curves
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
    res <- general_fit_var_cov_mat[["coeff_C", "coeff_C"]] +
      general_fit_var_cov_mat[["coeff_alpha", "coeff_alpha"]] * d^2 +
      general_fit_var_cov_mat[["coeff_beta", "coeff_beta"]] * d^4 * G^2 +
      2 * general_fit_var_cov_mat[["coeff_C", "coeff_alpha"]] * d +
      2 * general_fit_var_cov_mat[["coeff_C", "coeff_beta"]] * d^2 * G +
      2 * general_fit_var_cov_mat[["coeff_alpha", "coeff_beta"]] * d^3 * G
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

#' @rdname calculate_decision_threshold
#' @return A vector with \code{aberr_test} and \code{dose_est}.
#' @noRd
#' @importFrom rlang .data
calculate_decision_threshold_table <- function(fit_results_list, decision_threshold_cells,
                                               count_data, model_formula, model_family,
                                               frequency_select = c(NULL, "measured_freq", "full_gen_freq"),
                                               aberr_module) {
  # Validate parameters
  frequency_select <- match.arg(frequency_select)

  decision_threshold <- data.frame(
    N = decision_threshold_cells %>%
      strsplit(" ") %>%
      unlist() %>%
      as.numeric()
  )

  decision_threshold <- decision_threshold %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      X95 = calculate_decision_threshold(
        fit_results_list,
        cells = .data$N,
        count_data,
        model_formula,
        model_family,
        frequency_select,
        conf_int = 0.95,
        aberr_module
      )[1],
      D95 = calculate_decision_threshold(
        fit_results_list,
        cells = .data$N,
        count_data,
        model_formula,
        model_family,
        frequency_select,
        conf_int = 0.95,
        aberr_module
      )[2] * 1000,
      X83 = calculate_decision_threshold(
        fit_results_list,
        cells = .data$N,
        count_data,
        model_formula,
        model_family,
        frequency_select,
        conf_int = 0.83,
        aberr_module
      )[1],
      D83 = calculate_decision_threshold(
        fit_results_list,
        cells = .data$N,
        count_data,
        model_formula,
        model_family,
        frequency_select,
        conf_int = 0.83,
        aberr_module
      )[2] * 1000
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = c(.data$N, grep("X", names(.), value = TRUE)),
        .fns = as.integer
      )
    )

  return(decision_threshold)
}
