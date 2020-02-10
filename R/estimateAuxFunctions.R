# Protracted function ----

#' Title
#'
#' @param time
#' @param time_0
#'
#' @return
#' @export
#'
#' @examples
protracted_g_function <- function(time, time_0) {
  x <- time / time_0
  g <- (2 / x^2) * (x - 1 + exp(-x))
  return(g)
}

# Generalized curves ----
#' Title
#'
#' @param dose
#' @param general_fit_coeffs
#' @param protracted_g_value
#'
#' @return
#' @export
#'
#' @examples
yield_fun <- function(dose, general_fit_coeffs, protracted_g_value) {
  general_fit_coeffs[[1]] +
    general_fit_coeffs[[2]] * dose +
    general_fit_coeffs[[3]] * dose^2 * protracted_g_value
}

# R factor depeding on selected CI
#' Title
#'
#' @param conf_int
#' @param general_fit_coeffs
#'
#' @return
#' @export
#'
#' @examples
R_factor <- function(general_fit_coeffs, conf_int = 0.95) {
  chisq_df <- sum(general_fit_coeffs != 0)
  sqrt(stats::qchisq(conf_int, df = chisq_df))
}

#' Title
#'
#' @param dose
#' @param general_var_cov_mat
#' @param protracted_g_value
#'
#' @return
#' @export
#'
#' @examples
yield_error_fun <- function(dose, general_var_cov_mat = NULL, protracted_g_value) {
  # Special case for yield estimate
  if (is.null(general_var_cov_mat)) {
    return(0)
  }

  # Calculation for lower and upper yields
  res <- general_var_cov_mat[["C", "C"]] +
    general_var_cov_mat[["α", "α"]] * dose^2 +
    general_var_cov_mat[["β", "β"]] * dose^4 * protracted_g_value^2 +
    2 * general_var_cov_mat[["C", "α"]] * dose +
    2 * general_var_cov_mat[["C", "β"]] * dose^2 * protracted_g_value +
    2 * general_var_cov_mat[["α", "β"]] * dose^3 * protracted_g_value
  if (sum(res < 0) > 0) {
    rep(0, length(res))
  } else {
    return(sqrt(res))
  }
}

#' Title
#'
#' @param dose Dose
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper"
#' @param general_fit_coeffs
#' @param general_var_cov_mat
#' @param protracted_g_value
#' @param conf_int
#'
#' @return
#' @export
#'
#' @examples
calculate_yield <- function(dose, type = "estimate", general_fit_coeffs, general_var_cov_mat = NULL, protracted_g_value, conf_int = 0.95) {
  # Calculate factor per type
  type_factor <- switch(type, list("estimate" = 0, "lower" = 1, "upper" = -1))

  # Calculate yield
  yield <- biodosetools::yield_fun(dose, general_fit_coeffs, protracted_g_value) +
    as.numeric(type_factor) *
      biodosetools::R_factor(general_fit_coeffs, conf_int) *
      biodosetools::yield_error_fun(dose, general_var_cov_mat, protracted_g_value)

  return(yield)
}

#' Title
#'
#' @param type Type of yield calculation. Can be "estimate", "lower", or "upper"
#' @param general_fit_coeffs
#' @param general_var_cov_mat
#' @param conf_int
#'
#' @return
#' @export
#'
#' @examples
calculate_yield_infimum <- function(type = "estimate", general_fit_coeffs, general_var_cov_mat = NULL, conf_int = 0.95) {
  # Calculate factor per type
  type_factor <- switch(type, list("estimate" = 0, "lower" = 1, "upper" = -1))

  # Calculate yield
  yield <- calculate_yield(0, type = "estimate", general_fit_coeffs, general_var_cov_mat, 1, conf_int)

  return(yield)
}

# Correct conf_int_yield if simple method is required
#' Title
#'
#' @param conf_int
#' @param protracted_g_value
#' @param type
#' @param dose
#' @param general_var_cov_mat
#'
#' @return
#' @export
#'
#' @examples
correct_conf_int <- function(conf_int, general_var_cov_mat, protracted_g_value, type, dose = seq(0, 10, 0.2)) {
  res <- general_var_cov_mat[["C", "C"]] +
    general_var_cov_mat[["α", "α"]] * dose^2 +
    general_var_cov_mat[["β", "β"]] * dose^4 * protracted_g_value^2 +
    2 * general_var_cov_mat[["C", "α"]] * dose +
    2 * general_var_cov_mat[["C", "β"]] * dose^2 * protracted_g_value +
    2 * general_var_cov_mat[["α", "β"]] * dose^3 * protracted_g_value
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

#' Title
#'
#' @param yield
#' @param type
#' @param general_fit_coeffs
#' @param general_var_cov_mat
#' @param protracted_g_value
#' @param conf_int
#'
#' @return
#' @export
#'
#' @examples
project_yield <- function(yield, type = "estimate", general_fit_coeffs, general_var_cov_mat = NULL, protracted_g_value, conf_int = 0.95) {
  yield_inf <- calculate_yield_infimum(type, general_fit_coeffs, general_var_cov_mat, conf_int)

  if (yield >= yield_inf) {
    stats::uniroot(function(dose) {
      calculate_yield(dose, type, general_fit_coeffs, general_var_cov_mat, protracted_g_value, conf_int) - yield
    }, c(1e-16, 100))$root
  } else {
    0
  }
}


# Correction functions ----

# Correct negative values
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
correct_negative_vals <- function(x) {
  ifelse(x < 0, 0, x)
}

# Correct yields if they are below the curve
#' Title
#'
#' @param yield
#' @param type
#' @param general_fit_coeffs
#' @param general_var_cov_mat
#' @param conf_int
#'
#' @return
#' @export
#'
#' @examples
correct_yield <- function(yield, type = "estimate", general_fit_coeffs, general_var_cov_mat, conf_int) {
  yield_inf <- calculate_yield_infimum(type, general_fit_coeffs, general_var_cov_mat, conf_int)

  if (yield < yield_inf) {
    yield <- 0
  }
  yield <- biodosetools::correct_negative_vals(yield)

  return(yield)
}

# Function to make sure F is bounded by 0 and 1
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
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
#' Title
#'
#' @param est_full_doses
#' @param protracted_g_value
#' @param conf_int_yield
#' @param conf_int_curve
#' @param general_fit_coeffs
#' @param general_var_cov_mat
#' @param conf_int_text_whole
#' @param conf_int_text_partial
#' @param conf_int_text_hetero
#' @param aberr_module
#' @param input
#'
#' @return
#' @export
#'
#' @examples
get_estimated_dose_curve <- function(est_full_doses, general_fit_coeffs, general_var_cov_mat,
                                     protracted_g_value, conf_int_yield, conf_int_curve,
                                     conf_int_text_whole, conf_int_text_partial, conf_int_text_hetero,
                                     aberr_module, input) {
  # Rightmost limit of the plot
  max_dose <- 1.05 * est_full_doses[["dose"]] %>%
    ifelse(is.na(.), 0, .) %>%
    max()

  # Plot data from curves
  curves_data <- data.frame(dose = seq(0, max_dose, length.out = 100)) %>%
    dplyr::mutate(
      yield = calculate_yield(.data$dose, type = "estimate", general_fit_coeffs, NULL, protracted_g_value, 0),
      yield_low = calculate_yield(.data$dose, type = "lower", general_fit_coeffs, general_var_cov_mat, protracted_g_value, conf_int_curve),
      yield_upp = calculate_yield(.data$dose, type = "upper", general_fit_coeffs, general_var_cov_mat, protracted_g_value, conf_int_curve)
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

  # Make base plot
  gg_curve <- ggplot2::ggplot(curves_data) +
    # Fitted curve
    ggplot2::stat_function(
      data = data.frame(x = c(0, max_dose)),
      mapping = ggplot2::aes(x = .data$x),
      fun = function(x) biodosetools::yield_fun(.data$x, general_fit_coeffs, protracted_g_value),
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
