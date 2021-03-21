#' Get fit dose curve
#'
#' @param fit_results_list List of fit results
#' @param aberr_name Name of the aberration to use in the y-axis
#'
#' @return ggplot object
#' @export
get_fit_dose_curve <- function(fit_results_list, aberr_name) {
  # Read objects from fit results list
  count_data <- fit_results_list[["fit_raw_data"]] %>% as.data.frame()
  fit_coeffs <- fit_results_list[["fit_coeffs"]]
  fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]

  # Generalized variance-covariance matrix
  general_fit_coeffs <- numeric(length = 3L) %>%
    `names<-`(c("coeff_C", "coeff_alpha", "coeff_beta"))

  for (var in row.names(fit_coeffs)) {
    general_fit_coeffs[[var]] <- fit_coeffs[var, "estimate"] %>% as.numeric()
  }

  # Generalized fit coefficients
  general_fit_var_cov_mat <- matrix(0, nrow = 3, ncol = 3) %>%
    `row.names<-`(c("coeff_C", "coeff_alpha", "coeff_beta")) %>%
    `colnames<-`(c("coeff_C", "coeff_alpha", "coeff_beta"))

  for (x_var in rownames(fit_var_cov_mat)) {
    for (y_var in colnames(fit_var_cov_mat)) {
      general_fit_var_cov_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var]
    }
  }

  # Generalized curves
  yield_fun <- function(d) {
    general_fit_coeffs[["coeff_C"]] +
      general_fit_coeffs[["coeff_alpha"]] * d +
      general_fit_coeffs[["coeff_beta"]] * d^2
  }

  chisq_df <- nrow(fit_coeffs)
  R_factor <- sqrt(stats::qchisq(0.95, df = chisq_df))

  yield_error_fun <- function(d) {
    sqrt(
      general_fit_var_cov_mat[["coeff_C", "coeff_C"]] +
        general_fit_var_cov_mat[["coeff_alpha", "coeff_alpha"]] * d^2 +
        general_fit_var_cov_mat[["coeff_beta", "coeff_beta"]] * d^4 +
        2 * general_fit_var_cov_mat[["coeff_C", "coeff_alpha"]] * d +
        2 * general_fit_var_cov_mat[["coeff_C", "coeff_beta"]] * d^2 +
        2 * general_fit_var_cov_mat[["coeff_alpha", "coeff_beta"]] * d^3
    )
  }

  # Plot data
  plot_data <- count_data %>%
    dplyr::mutate(
      yield = .data$X / .data$N,
      dose = .data$D
    ) %>%
    dplyr::select(.data$dose, .data$yield)

  curves_data <- data.frame(dose = seq(0, max(plot_data[["dose"]]), length.out = 100)) %>%
    dplyr::mutate(
      yield = yield_fun(.data$dose),
      yield_low = yield_fun(.data$dose) - R_factor * yield_error_fun(.data$dose),
      yield_upp = yield_fun(.data$dose) + R_factor * yield_error_fun(.data$dose)
    )

  # Make plot
  gg_curve <- ggplot2::ggplot(plot_data) +
    # Observed data
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = .data$dose, y = .data$yield)
    ) +
    # Fitted curve
    ggplot2::stat_function(
      data = data.frame(x = c(0, max(plot_data[["dose"]]))),
      mapping = ggplot2::aes(.data$x),
      fun = function(x) yield_fun(x),
      linetype = "dashed"
    ) +
    # Confidence bands (Merkle, 1983)
    ggplot2::geom_ribbon(
      data = curves_data,
      ggplot2::aes(x = .data$dose, ymin = .data$yield_low, ymax = .data$yield_upp),
      alpha = 0.25
    ) +
    ggplot2::labs(
      x = "Dose (Gy)",
      y = paste0(aberr_name, "/cells")
    ) +
    ggplot2::theme_bw()

  # Return object
  return(gg_curve)
}
