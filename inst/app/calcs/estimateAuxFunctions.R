# Protracted function ----

protracted_g_function <- function(time, time_0) {
  x <- time / time_0
  g <- (2 / x^2) * (x - 1 + exp(-x))
  return(g)
}

# Generalized curves ----
yield_fun <- function(d, G) {
  general_fit_coeffs[[1]] +
    general_fit_coeffs[[2]] * d +
    general_fit_coeffs[[3]] * d^2 * G
}

# R factor depeding on selected CI
R_factor <- function(conf_int = 0.95) {
  chisq_df <- sum(general_fit_coeffs != 0)
  sqrt(qchisq(conf_int, df = chisq_df))
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

# Correct conf_int_yield if simple method is required
correct_conf_int <- function(conf_int, G, type, d = seq(0, 10, 0.2)) {
  res <- general_var_cov_mat[["C", "C"]] +
    general_var_cov_mat[["α", "α"]] * d^2 +
    general_var_cov_mat[["β", "β"]] * d^4 * G^2 +
    2 * general_var_cov_mat[["C", "α"]] * d +
    2 * general_var_cov_mat[["C", "β"]] * d^2 * G +
    2 * general_var_cov_mat[["α", "β"]] * d^3 * G
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

project_yield_estimate <- function(yield) {
  if (yield >= yield_est_inf) {
    uniroot(function(dose) {
      yield_fun(dose, protracted_g_value) - yield
    }, c(1e-16, 100))$root
  } else {
    0
  }
}

project_yield_lower <- function(yield, conf_int) {
  if (yield >= yield_low_inf) {
    uniroot(function(dose) {
      yield_fun(dose, protracted_g_value) + R_factor(conf_int) * yield_error_fun(dose, protracted_g_value) - yield
    }, c(1e-16, 100))$root
  } else {
    0
  }
}

project_yield_upper <- function(yield, conf_int) {
  if (yield >= yield_upp_inf) {
    uniroot(function(dose) {
      yield_fun(dose, protracted_g_value) - R_factor(conf_int) * yield_error_fun(dose, protracted_g_value) - yield
    }, c(1e-16, 100))$root
  } else {
    0
  }
}


# Correction functions ----

# Correct negative values
correct_negative_vals <- function(x) {
  ifelse(x < 0, 0, x)
}

# Correct yields if they are below the curve
correct_yield <- function(yield) {
  yield_name <- deparse(substitute(yield))
  suffix <- stringr::str_extract(yield_name, "est|low|upp")
  yield_inf <- get(paste("yield", suffix, "inf", sep = "_"))

  if (yield < yield_inf) {
    yield <- 0
  }
  yield <- correct_negative_vals(yield)

  return(yield)
}

# Function to make sure F is bounded by 0 and 1
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
get_dose_curve <- function(est_full_doses, protracted_g_value, conf_int_yield, conf_int_curve) {
  # Rightmost limit of the plot
  max_dose <- 1.05 * est_full_doses[["dose"]] %>%
    ifelse(is.na(.), 0, .) %>%
    max()

  # Plot data from curves
  curves_data <- data.frame(dose = seq(0, max_dose, length.out = 100)) %>%
    dplyr::mutate(
      yield = yield_fun(dose, protracted_g_value),
      yield_low = yield_fun(dose, protracted_g_value) - R_factor(conf_int_curve) * yield_error_fun(dose, protracted_g_value),
      yield_upp = yield_fun(dose, protracted_g_value) + R_factor(conf_int_curve) * yield_error_fun(dose, protracted_g_value)
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
  gg_curve <- ggplot(curves_data) +
    # Fitted curve
    stat_function(
      data = data.frame(x = c(0, max_dose)),
      mapping = aes(x),
      fun = function(x) yield_fun(x, protracted_g_value),
      linetype = "dashed"
    ) +
    # Confidence bands (Merkle, 1983)
    geom_ribbon(
      data = curves_data,
      aes(x = dose, ymin = yield_low, ymax = yield_upp),
      alpha = 0.25
    ) +
    labs(x = "Dose (Gy)", y = paste0(aberr_name, "/cells")) +
    theme_bw()

  # Add doses to plot
  gg_curve <- gg_curve +
    # Estimated whole-body doses
    geom_point(
      data = est_full_doses,
      aes(x = dose, y = yield, color = type, shape = level),
      size = 2, na.rm = TRUE
    ) +
    # Assessment
    scale_color_manual(
      values = hcl(
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
    scale_shape_manual(
      values = c("Lower" = 15, "Estimate" = 16, "Upper" = 17),
      breaks = c("Lower", "Estimate", "Upper")
    ) +
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2),
      fill = guide_legend(order = 3)
    ) +
    labs(color = "Assessment", shape = "Estimation") +
    # Tweak legend
    theme(
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.spacing.y = unit(5, "points"),
      legend.key.height = unit(12, "points")
    )

  return(gg_curve)
}
