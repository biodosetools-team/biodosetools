# Parse case data ----

devtools::load_all()

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

lambda_est <- case_data[["y"]]
lambda_est_sd <- case_data[["y_err"]]

# Fitting results (LQ) ----
fit_results_list <- readRDS(app_sys("extdata", "dicentrics-fitting-data-2020-10-10.rds"))

# Parse fitting data
fit_coeffs <- fit_results_list[["fit_coeffs"]]
fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]

# Generalised fit coefficients and variance-covariance matrix
general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

coeff_C <- general_fit_coeffs[[1]]
coeff_alpha <- general_fit_coeffs[[2]]
coeff_beta_raw <- general_fit_coeffs[[3]]

protracted_g_value <- 1

# Delta method (manual) ----

# Update coeff_beta to correct for protracted exposures
coeff_beta <- coeff_beta_raw * protracted_g_value

# Auxiliary variable
z <- coeff_alpha^2 + 4 * coeff_beta * (lambda_est - coeff_C)

# Get estimate for dose
dose_est <- (-coeff_alpha + sqrt(z)) / (2 * coeff_beta)

# Derivatives of regression curve coefs
deriv_lambda <- (z)^(-0.5)
deriv_coeff_C <- -(z)^(-0.5)
deriv_coeff_alpha <- (1 / (2 * coeff_beta)) * (-1 + coeff_alpha * z^(-0.5))
deriv_coeff_beta <- protracted_g_value * (4 * coeff_beta * (lambda_est - coeff_C) * (z^(-0.5)) + 2 * coeff_alpha - 2 * z^(0.5)) / (4 * coeff_beta^2)

dose_est_var_manual <-
  (deriv_coeff_C^2) * general_fit_var_cov_mat[1, 1] +
  (deriv_coeff_alpha^2) * general_fit_var_cov_mat[2, 2] +
  (deriv_coeff_beta^2) * general_fit_var_cov_mat[3, 3] +
  (deriv_lambda^2) * (lambda_est_sd^2) +
  2 * (deriv_coeff_C * deriv_coeff_alpha) * general_fit_var_cov_mat[1, 2] +
  2 * (deriv_coeff_C * deriv_coeff_beta) * general_fit_var_cov_mat[1, 3] +
  2 * (deriv_coeff_alpha * deriv_coeff_beta) * general_fit_var_cov_mat[2, 3]

# Delta method (msm package) ----

formula <- paste(
  "~", "(-x2 + sqrt(x2^2 + 4 * x3 *", protracted_g_value, "* (x4 - x1)))", "/",
  "(2 * x3 *", protracted_g_value, ")",
  sep = ""
)

cov_extended <- matrix(0, ncol = 4, nrow = 4)
cov_extended[1:3, 1:3] <- general_fit_var_cov_mat
cov_extended[4, 4] <- lambda_est_sd^2

dose_est_sd_delta <- msm::deltamethod(
  g = stats::as.formula(formula),
  mean = c(coeff_C, coeff_alpha, coeff_beta_raw, lambda_est),
  cov = cov_extended
)

sqrt(dose_est_var_manual)
dose_est_sd_delta

# microbenchmark::microbenchmark(
#   manual = (deriv_coeff_C^2) * general_fit_var_cov_mat[1, 1] +
#     (deriv_coeff_alpha^2) * general_fit_var_cov_mat[2, 2] +
#     (deriv_coeff_beta^2) * general_fit_var_cov_mat[3, 3] +
#     (deriv_lambda^2) * (lambda_est_sd^2) +
#     2 * (deriv_coeff_C * deriv_coeff_alpha) * general_fit_var_cov_mat[1, 2] +
#     2 * (deriv_coeff_C * deriv_coeff_beta) * general_fit_var_cov_mat[1, 3] +
#     2 * (deriv_coeff_alpha * deriv_coeff_beta) * general_fit_var_cov_mat[2, 3],
#   msm = msm::deltamethod(
#     stats::as.formula(formula),
#     mean = c(coeff_C, coeff_alpha, coeff_beta_raw, lambda_est),
#     cov = cov_extended
#   ),
#   times = 1000
# )

# Fitting results (L) ----
fit_results_list <- readRDS("~/Downloads/dicentrics-fitting-data-2022-01-17.rds")

# Parse fitting data
fit_coeffs <- fit_results_list[["fit_coeffs"]]
fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]

# Generalised fit coefficients and variance-covariance matrix
general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

coeff_C <- general_fit_coeffs[[1]]
coeff_alpha <- general_fit_coeffs[[2]]
coeff_beta <- general_fit_coeffs[[3]]

protracted_g_value <- 1

# Delta method (manual) ----

# Get estimate for dose
dose_est <- (lambda_est - coeff_C) / coeff_alpha

# Derivatives of regression curve coefs
deriv_lambda <- 1 / coeff_alpha
deriv_coeff_C <- -(1 / coeff_alpha)
deriv_coeff_alpha <- (coeff_C - lambda_est) / coeff_alpha^2
deriv_coeff_beta <- 0

dose_est_var_manual <-
  (deriv_coeff_C^2) * general_fit_var_cov_mat[1, 1] +
  (deriv_coeff_alpha^2) * general_fit_var_cov_mat[2, 2] +
  (deriv_coeff_beta^2) * general_fit_var_cov_mat[3, 3] +
  (deriv_lambda^2) * (lambda_est_sd^2) +
  2 * (deriv_coeff_C * deriv_coeff_alpha) * general_fit_var_cov_mat[1, 2] +
  2 * (deriv_coeff_C * deriv_coeff_beta) * general_fit_var_cov_mat[1, 3] +
  2 * (deriv_coeff_alpha * deriv_coeff_beta) * general_fit_var_cov_mat[2, 3]

# Delta method (msm package) ----

# D = (Y - C) / alpha
formula <- paste(
  "~", "(x4 - x1) / x2",
  sep = ""
)

cov_extended <- matrix(0, ncol = 4, nrow = 4)
cov_extended[1:3, 1:3] <- general_fit_var_cov_mat
cov_extended[4, 4] <- lambda_est_sd^2

dose_est_sd_delta <- msm::deltamethod(
  g = stats::as.formula(formula),
  mean = c(coeff_C, coeff_alpha, coeff_beta, lambda_est),
  cov = cov_extended
)

sqrt(dose_est_var_manual)
dose_est_sd_delta

# microbenchmark::microbenchmark(
#   manual = (deriv_coeff_C^2) * general_fit_var_cov_mat[1, 1] +
#     (deriv_coeff_alpha^2) * general_fit_var_cov_mat[2, 2] +
#     (deriv_coeff_beta^2) * general_fit_var_cov_mat[3, 3] +
#     (deriv_lambda^2) * (lambda_est_sd^2) +
#     2 * (deriv_coeff_C * deriv_coeff_alpha) * general_fit_var_cov_mat[1, 2] +
#     2 * (deriv_coeff_C * deriv_coeff_beta) * general_fit_var_cov_mat[1, 3] +
#     2 * (deriv_coeff_alpha * deriv_coeff_beta) * general_fit_var_cov_mat[2, 3],
#   msm = msm::deltamethod(
#     stats::as.formula(formula),
#     mean = c(coeff_C, coeff_alpha, coeff_beta_raw, lambda_est),
#     cov = cov_extended
#   ),
#   times = 1000
# )
