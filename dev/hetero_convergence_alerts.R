load_all()

case_data <- app_sys("extdata", "cases-data-partial.csv") %>%
  utils::read.csv(header = TRUE)

case_data <- calculate_aberr_table(
  data = case_data,
  type = "case",
  assessment_u = 1
)

# Specific to dicentrics/micronuclei
case_data <- case_data %>%
  dplyr::rename(
    y = .data$mean,
    y_err = .data$std_err
  )

# Colnames validation
case_data_cols <- colnames(case_data)
case_data_cols_len <- length(case_data_cols)


# Dose estimation
aberr_module <- "dicentrics"

fit_results_list <- app_sys("extdata", "dicentrics-fitting-data-2020-10-10.rds") %>%
  readRDS()

# Parse fitting data
fit_coeffs <- fit_results_list[["fit_coeffs"]]
fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]
fit_formula_tex <- fit_results_list[["fit_formula_tex"]]

# Protraction (acute exposure)
protracted_g_value <- 1

# Parse genome fraction
parsed_genome_factor <- 1

# Calculations
set.seed(6) # Doesn't converge
for (i in 1:5) {
  # set.seed(6) # Doesn't converge
  message("Intent ", i)
  try(
    {
      results_hetero <- estimate_hetero_mixed_poisson(
        case_data,
        fit_coeffs,
        fit_var_cov_mat,
        conf_int_yield = 0.83,
        conf_int_curve = 0.83,
        protracted_g_value,
        gamma = 1 / 2.7,
        gamma_error = 0
      )
      break # break/exit the for-loop
    },
    silent = TRUE
  )
}

rm(results_hetero)

if (!exists("results_hetero")) {
  cli::cli_alert_danger("The algorithm did not converge!")
  showNotification(
    ui = "The algorithm did not converge!",
    type = "warning"
  )
}
