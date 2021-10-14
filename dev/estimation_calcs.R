load_all()
library(tidyverse)

# Case data ----
case_data <- data.frame(
  C0 = 1036, C1 = 1, C2 = 0, C3 = 0, C4 = 0, C5 = 1
) %>%
  dplyr::rename_with(
    .fn = toupper,
    .cols = dplyr::everything()
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::starts_with("C"),
      .fns = as.integer
    )
  ) %>%
  dplyr::select(
    dplyr::starts_with("C")
  )

case_data <- calculate_aberr_table(
  data = case_data,
  type = "case",
  assessment_u = 1
)

case_data <- case_data %>%
  dplyr::mutate(
    y = .data$mean,
    y_err = .data$std_err
  ) %>%
  dplyr::select(-.data$mean, -.data$std_err)

# Parse parameters ----

exposure <- "acute"
assessment <- "whole-body"
error_method <- "merkle-83"
# error_method_partial <-
error_method_hetero <- "merkle-83"

fit_results_list <- app_sys("extdata", "dicentrics-fitting-data-2020-10-10.rds") %>%
  readRDS()

# Parse fitting data
fit_coeffs <- fit_results_list[["fit_coeffs"]]
fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]
fit_formula_tex <- fit_results_list[["fit_formula_tex"]]

# Generalized variance-covariance matrix
general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])

# Generalized fit coefficients
general_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

# Protracted variables
if (exposure == "protracted") {
  protracted_time <- #input$protracted_time
  protracted_life_time <- #input$protracted_life_time
  protracted_g_value <- protracted_g_function(protracted_time, protracted_life_time)
} else if (exposure == "protracted_high") {
  protracted_g_value <- 0
  # Used in report (dummy values)
  protracted_time <- NA
  protracted_life_time <- NA
} else {
  protracted_g_value <- 1
  # Used in report (dummy values)
  protracted_time <- NA
  protracted_life_time <- NA
}

# Confidence intervals

# Select whole-body CI depending on selected method
if (grepl("merkle", error_method, fixed = TRUE)) {
  conf_int_curve <- paste0("0.", gsub("\\D", "", error_method)) %>% as.numeric()
  conf_int_yield <- conf_int_curve

  # Parse CI text for plot legend
  conf_int_text_whole <- paste0(
    "(", round(100 * conf_int_curve, 0), "%",
    "-", round(100 * conf_int_yield, 0), "%", ")"
  )
} else if (error_method == "delta") {
  conf_int_curve <- 0.83
  conf_int_yield <- conf_int_curve
  conf_int_delta <- 0.95

  # Parse CI text for plot legend
  conf_int_text_whole <- paste0("(", round(100 * conf_int_delta, 0), "%", ")")
}

# Initialize partial-body and heterogeneous CI text for plot legend
conf_int_text_partial <- NULL
conf_int_text_hetero <- NULL

# Select partial-body and heterogeneous CI depending on selected method
if (assessment == "partial-body") {
  conf_int_dolphin <- 0.95

  # Parse CI text for plot legend
  conf_int_text_partial <- paste0("(", round(100 * conf_int_dolphin, 0), "%", ")")
} else if (assessment == "hetero") {
  conf_int_curve_hetero <- paste0("0.", gsub("\\D", "", error_method_hetero)) %>% as.numeric()
  conf_int_yield_hetero <- conf_int_curve_hetero

  # Parse CI text for plot legend
  conf_int_text_hetero <- paste0(
    "(", round(100 * conf_int_curve_hetero, 0), "%",
    "-", round(100 * conf_int_yield_hetero, 0), "%", ")"
  )
}

conf_int_curve <- conf_int_curve %>%
  correct_conf_int(general_var_cov_mat, protracted_g_value, type = "curve")
conf_int_yield <- conf_int_yield %>%
  correct_conf_int(general_var_cov_mat, protracted_g_value, type = "yield")


# Parse genome fraction
parsed_genome_factor <- 1

# Calculations ----

results_whole <- estimate_whole_body(
  case_data,
  general_fit_coeffs,
  general_var_cov_mat,
  conf_int_yield,
  conf_int_curve,
  protracted_g_value,
  parsed_genome_factor,
  aberr_module = "dicentrics"
)
