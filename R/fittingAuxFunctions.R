# Curve function ----
#' Title
#'
#' @param fit_results_list List of fit results
#'
#' @return ggplot object
#' @export
#'
get_fit_dose_curve <- function(fit_results_list) {
  # Read objects from fit results list
  count_data <- fit_results_list[["fit_raw_data"]] %>% as.data.frame()
  fit_coeffs <- fit_results_list[["fit_coeffs"]]
  fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]

  # Generalized variance-covariance matrix
  general_fit_coeffs <- numeric(length = 3L) %>%
    `names<-`(c("C", "α", "β"))

  for (var in row.names(fit_coeffs)) {
    general_fit_coeffs[[var]] <- fit_coeffs[var, "estimate"] %>% as.numeric()
  }

  # Generalized fit coefficients
  general_fit_var_cov_mat <- matrix(0, nrow = 3, ncol = 3) %>%
    `row.names<-`(c("C", "α", "β")) %>%
    `colnames<-`(c("C", "α", "β"))

  for (x_var in rownames(fit_var_cov_mat)) {
    for (y_var in colnames(fit_var_cov_mat)) {
      general_fit_var_cov_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var]
    }
  }

  # Generalized curves
  yield_fun <- function(d) {
    general_fit_coeffs[["C"]] +
      general_fit_coeffs[["α"]] * d +
      general_fit_coeffs[["β"]] * d^2
  }

  chisq_df <- nrow(fit_coeffs)
  R_factor <- sqrt(stats::qchisq(0.95, df = chisq_df))

  yield_error_fun <- function(d) {
    sqrt(
      general_fit_var_cov_mat[["C", "C"]] +
        general_fit_var_cov_mat[["α", "α"]] * d^2 +
        general_fit_var_cov_mat[["β", "β"]] * d^4 +
        2 * general_fit_var_cov_mat[["C", "α"]] * d +
        2 * general_fit_var_cov_mat[["C", "β"]] * d^2 +
        2 * general_fit_var_cov_mat[["α", "β"]] * d^3
    )
  }

  # Plot data
  plot_data <- count_data %>%
    dplyr::mutate(
      yield = X / N,
      dose = D
    ) %>%
    dplyr::select(dose, yield)

  curves_data <- data.frame(dose = seq(0, max(plot_data[["dose"]]), length.out = 100)) %>%
    dplyr::mutate(
      yield = yield_fun(dose),
      yield_low = yield_fun(dose) - R_factor * yield_error_fun(dose),
      yield_upp = yield_fun(dose) + R_factor * yield_error_fun(dose)
    )

  # Name of the aberration to use in the y-axis
  if (aberr_module == "dicentrics" | aberr_module == "micronuclei") {
    aberr_name <- stringr::str_to_title(aberr_module)
  } else if (aberr_module == "translocations") {
    if (nchar(input$trans_name) > 0) {
      aberr_name <- input$trans_name
    } else {
      aberr_name <- stringr::str_to_title(aberr_module)
    }
  }

  # Make plot
  gg_curve <- ggplot2::ggplot(plot_data) +
    # Observed data
    ggplot2::geom_point(ggplot2::aes(x = dose, y = yield)) +
    # Fitted curve
    ggplot2::stat_function(
      data = data.frame(x = c(0, max(plot_data[["dose"]]))),
      mapping = ggplot2::aes(x),
      fun = function(x) yield_fun(x),
      linetype = "dashed"
    ) +
    # Confidence bands (Merkle, 1983)
    ggplot2::geom_ribbon(data = curves_data, ggplot2::aes(x = dose, ymin = yield_low, ymax = yield_upp), alpha = 0.25) +
    ggplot2::labs(x = "Dose (Gy)", y = paste0(aberr_name, "/cells")) +
    ggplot2::theme_bw()

  # Return object
  return(gg_curve)
}

# Decision thresholds ----
#' Title
#'
#' @param fit_results_list List of fit results
#' @param cells N
#' @param conf_int CI
#'
#' @return A vector with aberr_test and dose_est
#' @export
#'
get_decision_threshold <- function(fit_results_list, cells, conf_int = 0.95) {

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
    `names<-`(c("C", "α", "β"))

  for (var in rownames(fit_coeffs)) {
    general_fit_coeffs[var] <- fit_coeffs[var, "estimate"]
  }

  # Generalized fit coefficients
  general_var_cov_mat <- matrix(0, nrow = 3, ncol = 3) %>%
    `row.names<-`(c("C", "α", "β")) %>%
    `colnames<-`(c("C", "α", "β"))

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
    res <- general_var_cov_mat[["C", "C"]] +
      general_var_cov_mat[["α", "α"]] * d^2 +
      general_var_cov_mat[["β", "β"]] * d^4 * G^2 +
      2 * general_var_cov_mat[["C", "α"]] * d +
      2 * general_var_cov_mat[["C", "β"]] * d^2 * G +
      2 * general_var_cov_mat[["α", "β"]] * d^3 * G
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
