# Protracted function ----

#' Calculate protracted function G(x)
#'
#' @param time Time over which the irradiation occurred
#' @param time_0 The mean lifetime of the breaks, which has been shown to be on the order of ~ 2 hours
#'
#' @return G(x) numeric value
#' @export
protracted_g_function <- function(time, time_0) {
  x <- time / time_0
  g_value <- (2 / x^2) * (x - 1 + exp(-x))

  return(g_value)
}

# Generalized curves ----

#' Calculate yield
#'
#' @param dose Dose
#' @param general_fit_coeffs Generalized fit coefficients matrix
#' @param protracted_g_value Protracted G(x) value
#'
#' @return Yield
#' @export
yield_fun <- function(dose, general_fit_coeffs, protracted_g_value) {
  yield <- general_fit_coeffs[[1]] +
    general_fit_coeffs[[2]] * dose +
    general_fit_coeffs[[3]] * dose^2 * protracted_g_value

  return(yield)
}

#' Calculate R regression confidence factor
#'
#' Calculate R regression confidence factor depeding on selected confidence interval and type of fit.
#'
#' @param conf_int Confidence interval
#' @param general_fit_coeffs Generalized fit coefficients matrix
#'
#' @return R regression confidence factor
#' @export
R_factor <- function(general_fit_coeffs, conf_int = 0.95) {
  chisq_df <- sum(general_fit_coeffs != 0)
  r_factor <- sqrt(stats::qchisq(conf_int, df = chisq_df))

  return(r_factor)
}

#' Calculate yield error
#'
#' Calculate yield error using Merkle's method
#'
#' @param dose Dose
#' @param general_var_cov_mat Generalized variance-covariance matrix
#' @param protracted_g_value Protracted G(x) value
#'
#' @return Yield error
#' @export
yield_error_fun <- function(dose, general_var_cov_mat = NULL, protracted_g_value) {
  # Special case for yield estimate
  if (is.null(general_var_cov_mat)) {
    return(0)
  }

  # Calculation for lower and upper yields
  res <- general_var_cov_mat[["coeff_C", "coeff_C"]] +
    general_var_cov_mat[["coeff_alpha", "coeff_alpha"]] * dose^2 +
    general_var_cov_mat[["coeff_beta", "coeff_beta"]] * dose^4 * protracted_g_value^2 +
    2 * general_var_cov_mat[["coeff_C", "coeff_alpha"]] * dose +
    2 * general_var_cov_mat[["coeff_C", "coeff_beta"]] * dose^2 * protracted_g_value +
    2 * general_var_cov_mat[["coeff_alpha", "coeff_beta"]] * dose^3 * protracted_g_value
  if (sum(res < 0) > 0) {
    rep(0, length(res))
  } else {
    return(sqrt(res))
  }
}

#' Calculate yield from dose
#'
#' @param dose Dose
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper"
#' @param general_fit_coeffs Generalized fit coefficients matrix
#' @param general_var_cov_mat Generalized variance-covariance matrix
#' @param protracted_g_value Protracted G(x) value
#' @param conf_int Confidence interval
#'
#' @return Yield
#' @export
calculate_yield <- function(dose, type = c("estimate", "lower", "upper"), general_fit_coeffs, general_var_cov_mat = NULL, protracted_g_value, conf_int = 0.95) {
  # Validate parameters
  type <- match.arg(type)

  # Calculate factor per type
  type_factor <- switch(type, "estimate" = 0, "lower" = 1, "upper" = -1)

  # Calculate yield
  yield <- yield_fun(dose, general_fit_coeffs, protracted_g_value) +
    as.numeric(type_factor) *
      R_factor(general_fit_coeffs, conf_int) *
      yield_error_fun(dose, general_var_cov_mat, protracted_g_value)

  return(yield)
}

#' Calculate theoretical yield infimum
#'
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper"
#' @param general_fit_coeffs Generalized fit coefficients matrix
#' @param general_var_cov_mat Generalized variance-covariance matrix
#' @param conf_int Confidence interval
#'
#' @return Yield infimum
#' @export
calculate_yield_infimum <- function(type = "estimate", general_fit_coeffs, general_var_cov_mat = NULL, conf_int = 0.95) {
  # Calculate factor per type
  type_factor <- switch(type, list("estimate" = 0, "lower" = 1, "upper" = -1))

  # Calculate yield
  yield <- calculate_yield(0, type = "estimate", general_fit_coeffs, general_var_cov_mat, 1, conf_int)

  return(yield)
}

#' Correct yield confidence interval
#'
#' Correct yield confidence interval if simple method is required.
#'
#' @param conf_int Confidence interval
#' @param protracted_g_value Protracted G(x) value
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper"
#' @param dose Dose
#' @param general_var_cov_mat Generalized variance-covariance matrix
#'
#' @return Corrected confidence interval
#' @export
correct_conf_int <- function(conf_int, general_var_cov_mat, protracted_g_value, type, dose = seq(0, 10, 0.2)) {
  res <- general_var_cov_mat[["coeff_C", "coeff_C"]] +
    general_var_cov_mat[["coeff_alpha", "coeff_alpha"]] * dose^2 +
    general_var_cov_mat[["coeff_beta", "coeff_beta"]] * dose^4 * protracted_g_value^2 +
    2 * general_var_cov_mat[["coeff_C", "coeff_alpha"]] * dose +
    2 * general_var_cov_mat[["coeff_C", "coeff_beta"]] * dose^2 * protracted_g_value +
    2 * general_var_cov_mat[["coeff_alpha", "coeff_beta"]] * dose^3 * protracted_g_value
  if (sum(res <= 0) > 1) {
    if (type == "curve") {
      conf_int <- 0
    } else if (type == "yield") {
      conf_int <- 0.95
    }
  }

  return(conf_int)
}

# Projection functions ----

#' Project yield into dose-effect fitting curve
#'
#' @param yield Yield to be projected
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper"
#' @param general_fit_coeffs Generalized fit coefficients matrix
#' @param general_var_cov_mat Generalized variance-covariance matrix
#' @param protracted_g_value Protracted G(x) value
#' @param conf_int Confidence interval
#'
#' @return Numeric value of projected dose
#' @export
project_yield <- function(yield, type = "estimate", general_fit_coeffs, general_var_cov_mat = NULL, protracted_g_value, conf_int = 0.95) {
  yield_inf <- calculate_yield_infimum(type, general_fit_coeffs, general_var_cov_mat, conf_int)

  if (yield >= yield_inf) {
    projected_dose <- stats::uniroot(function(dose) {
      calculate_yield(dose, type, general_fit_coeffs, general_var_cov_mat, protracted_g_value, conf_int) - yield
    }, c(1e-16, 100))$root
  } else {
    projected_dose <- 0
  }

  return(projected_dose)
}


# Correction functions ----

#' Correct negative values
#'
#' @param x Numeric value
#'
#' @return Numeric value corrected to zero if negative
#' @export
correct_negative_vals <- function(x) {
  x_corrected <- ifelse(x < 0, 0, x)

  return(x_corrected)
}


#' Correct yields if they are below the curve
#'
#' @param yield Yield
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper"
#' @param general_fit_coeffs Generalized fit coefficients matrix
#' @param general_var_cov_mat Generalized variance-covariance matrix
#' @param conf_int Confidence interval
#'
#' @return Numeric value of corrected yield
#' @export
correct_yield <- function(yield, type = "estimate", general_fit_coeffs, general_var_cov_mat, conf_int) {
  yield_inf <- calculate_yield_infimum(type, general_fit_coeffs, general_var_cov_mat, conf_int)

  if (yield < yield_inf) {
    yield <- 0
  }
  yield <- correct_negative_vals(yield)

  return(yield)
}

#' Correct boundary of irradiated fractions to be bounded by 0 and 1
#'
#' @param x Numeric value
#'
#' @return Numeric value in [0, 1] range
#' @export
correct_boundary <- function(x) {
  if (x > 1) {
    return(1)
  } else if (x < 0) {
    return(0)
  } else {
    return(x)
  }
}

# Curve function ----

#' Get dose estimation curve
#'
#' @param est_full_doses Data frame with yields and dose estimations. It requires dose, yield, type, level columns
#' @param protracted_g_value Protracted G(x) value
#' @param conf_int_yield Confidence interval of the yield
#' @param conf_int_curve Confidence interval of the curve
#' @param general_fit_coeffs Generalized fit coefficients matrix
#' @param general_var_cov_mat Generalized variance-covariance matrix
#' @param conf_int_text_whole Text to display confidence interval for whole-body estimation
#' @param conf_int_text_partial Text to display confidence interval for partial-body estimation
#' @param conf_int_text_hetero Text to display confidence interval for heterogeneous estimation
#' @param aberr_name Name of the aberration to use in the y-axis
#'
#' @return ggplot object
#' @export
get_estimated_dose_curve <- function(est_full_doses, general_fit_coeffs, general_var_cov_mat,
                                     protracted_g_value, conf_int_yield, conf_int_curve,
                                     conf_int_text_whole, conf_int_text_partial, conf_int_text_hetero,
                                     aberr_name) {
  # Rightmost limit of the plot
  max_dose <- 1.05 * est_full_doses[["dose"]] %>%
    ifelse(is.na(.), 0, .) %>%
    max()

  # Plot data from curves
  curves_data <- data.frame(
    dose = seq(0, max_dose, length.out = 100)
  ) %>%
    dplyr::mutate(
      yield = calculate_yield(.data$dose, type = "estimate", general_fit_coeffs, NULL, protracted_g_value, 0),
      yield_low = calculate_yield(.data$dose, type = "lower", general_fit_coeffs, general_var_cov_mat, protracted_g_value, conf_int_curve),
      yield_upp = calculate_yield(.data$dose, type = "upper", general_fit_coeffs, general_var_cov_mat, protracted_g_value, conf_int_curve)
    )

  # Make base plot
  gg_curve <- ggplot2::ggplot(curves_data) +
    # Fitted curve
    ggplot2::stat_function(
      data = data.frame(x = c(0, max_dose)),
      mapping = ggplot2::aes(x = .data$x),
      fun = function(x) yield_fun(x, general_fit_coeffs, protracted_g_value),
      linetype = "dashed"
    ) +
    # Confidence bands (Merkle, 1983)
    ggplot2::geom_ribbon(
      data = curves_data,
      mapping = ggplot2::aes(x = .data$dose, ymin = .data$yield_low, ymax = .data$yield_upp),
      alpha = 0.25
    ) +
    ggplot2::labs(x = "Dose (Gy)", y = paste0(aberr_name, "/cells")) +
    ggplot2::theme_bw()

  # Add doses to plot
  gg_curve <- gg_curve +
    # Estimated whole-body doses
    ggplot2::geom_point(
      data = est_full_doses,
      mapping = ggplot2::aes(x = .data$dose, y = .data$yield, color = .data$type, shape = .data$level),
      size = 2, na.rm = TRUE
    ) +
    # Assessment
    ggplot2::scale_color_manual(
      values = grDevices::hcl(
        h = seq(15, 375, length = 4 + 1),
        l = 65,
        c = 100
      ) %>%
        .[1:4] %>%
        `names<-`(c("Partial-body", "Heterogeneous 1", "Heterogeneous 2", "Whole-body")),
      breaks = c("Whole-body", "Partial-body", "Heterogeneous 1", "Heterogeneous 2"),
      labels = c(
        paste("Whole-body", conf_int_text_whole),
        paste("Partial-body", conf_int_text_partial),
        paste("Heterogeneous 1", conf_int_text_hetero),
        paste("Heterogeneous 2", conf_int_text_hetero)
      )
    ) +
    # Estimation level
    ggplot2::scale_shape_manual(
      values = c("Lower" = 15, "Estimate" = 16, "Upper" = 17),
      breaks = c("Lower", "Estimate", "Upper")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1),
      shape = ggplot2::guide_legend(order = 2),
      fill = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::labs(color = "Assessment", shape = "Estimation") +
    # Tweak legend
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8),
      legend.spacing.y = ggplot2::unit(5, "points"),
      legend.key.height = ggplot2::unit(12, "points")
    )

  return(gg_curve)
}
