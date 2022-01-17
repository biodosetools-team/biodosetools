# AIC and logLik from data ----

#' Calculate AIC (Akaike's ‘An Information Criterion’)
#'
#' @param general_fit_coeffs Generalised fit coefficients matrix
#' @param data Data (dose, yield) to calculate AIC from
#' @param dose_var Name of the dose variable (enquoted)
#' @param yield_var Name of the yield variable (enquoted)
#' @param fit_link A specification for the model link function
#'
#' @return Numeric value with the corresponding AIC
AIC_from_data <- function(general_fit_coeffs, data, dose_var = "dose", yield_var = "yield", fit_link = "identity") {

  # Manual log-likelihood function
  loglik_from_data <- function(data, fit_link) {
    if (fit_link == "identity") {
      loglik <- -yield_fun(data[[dose_var]], general_fit_coeffs, 1) +
        log(yield_fun(data[[dose_var]], general_fit_coeffs, 1)) * data[[yield_var]] -
        log(factorial(data[[yield_var]]))
    } else if (fit_link == "log") {
      loglik <- -exp(yield_fun(data[[dose_var]], general_fit_coeffs, 1)) +
        yield_fun(data[[dose_var]], general_fit_coeffs, 1) * data[[yield_var]] -
        log(factorial(data[[yield_var]]))
    }
    return(sum(loglik))
  }

  logLik <- loglik_from_data(data, fit_link)

  num_params <- sum(general_fit_coeffs != 0)
  AIC <- 2 * num_params - 2 * logLik

  return(AIC)
}

# Dose estimation functions ----

#' Whole-body dose estimation (Merkle's method)
#'
#' Whole-body dose estimation using Merkle's method.
#'
#' @param case_data Case data in data frame form
#' @param conf_int_yield Confidence interval of the yield, 83\% by default
#' @param conf_int_curve Confidence interval of the curve, 83\% by default
#' @param protracted_g_value Protracted G(x) value
#' @param fit_coeffs Fitting coefficients matrix
#' @param fit_var_cov_mat Fitting variance-covariance matrix
#' @param genome_factor Genomic conversion factor used in translocations, else 1
#' @param aberr_module Aberration module Aberration module
#'
#' @return List containing estimated doses data frame and AIC
#' @export
estimate_whole_body_merkle <- function(case_data, fit_coeffs, fit_var_cov_mat, conf_int_yield = 0.83, conf_int_curve = 0.83, protracted_g_value, genome_factor = 1, aberr_module) {
  # Parse aberrations and cells
  aberr <- case_data[["X"]]
  cells <- case_data[["N"]]

  if (aberr_module == "dicentrics") {
    yield_est <- case_data[["y"]]
  }

  if (aberr_module == "translocations") {
    aberr <- correct_negative_vals(aberr - case_data[["Xc"]])
    yield_est <- case_data[["Fg"]]
  }

  # Generalised fit coefficients and variance-covariance matrix
  general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
  general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

  # Correct CIs
  conf_int_curve <- conf_int_curve %>%
    correct_conf_int(general_fit_var_cov_mat, protracted_g_value, type = "curve")
  conf_int_yield <- conf_int_yield %>%
    correct_conf_int(general_fit_var_cov_mat, protracted_g_value, type = "yield")

  # Calculate CI using Exact Poisson tests
  aberr_row <- stats::poisson.test(x = round(aberr, 0), conf.level = conf_int_yield)[["conf.int"]]

  aberr_low <- aberr_row[1]
  aberr_upp <- aberr_row[2]

  yield_low <- aberr_low / (cells * genome_factor)
  yield_upp <- aberr_upp / (cells * genome_factor)
  # TODO: possible modification IAEA§9.7.3

  # Correct "unrootable" yields
  yield_est_corr <- correct_yield(yield_est, "estimate", general_fit_coeffs, general_fit_var_cov_mat, conf_int_curve)
  if (yield_est_corr < yield_est) {
    yield_est <- 0
    yield_low <- 0
    yield_upp <- 0
  }

  # Calculate projections
  dose_est <- project_yield(
    yield = yield_est,
    type = "estimate",
    general_fit_coeffs = general_fit_coeffs,
    general_fit_var_cov_mat = NULL,
    protracted_g_value = protracted_g_value,
    conf_int = 0
  )

  dose_low <- project_yield(
    yield = yield_low,
    type = "lower",
    general_fit_coeffs = general_fit_coeffs,
    general_fit_var_cov_mat = general_fit_var_cov_mat,
    protracted_g_value = protracted_g_value,
    conf_int = conf_int_curve
  )

  dose_upp <- project_yield(
    yield = yield_upp,
    type = "upper",
    general_fit_coeffs = general_fit_coeffs,
    general_fit_var_cov_mat = general_fit_var_cov_mat,
    protracted_g_value = protracted_g_value,
    conf_int = conf_int_curve
  )

  # Whole-body estimation results
  est_doses <- data.frame(
    yield = c(yield_low, yield_est, yield_upp),
    dose = c(dose_low, dose_est, dose_upp)
  ) %>%
    `row.names<-`(c("lower", "estimate", "upper"))

  # Calculate AIC as a GOF indicator
  AIC <- AIC_from_data(
    general_fit_coeffs, est_doses["estimate", ],
    dose_var = "dose", yield_var = "yield", fit_link = "identity"
  )


  # Return objects
  results_list <- list(
    est_doses = est_doses,
    AIC = AIC,
    conf_int = c(yield = conf_int_yield, curve = conf_int_curve)
  )

  return(results_list)
}

#' Whole-body dose estimation (delta method)
#'
#' Whole-body dose estimation using delta method.
#'
#' @param case_data Case data in data frame form
#' @param fit_coeffs Fitting coefficients matrix
#' @param fit_var_cov_mat Fitting variance-covariance matrix
#' @param conf_int Confidence interval, 95\% by default
#' @param protracted_g_value Protracted G(x) value
#' @param aberr_module Aberration module
#'
#' @return List containing estimated doses data frame and AIC
#' @export
estimate_whole_body_delta <- function(case_data, fit_coeffs, fit_var_cov_mat,
                                      conf_int = 0.95, protracted_g_value, aberr_module) {
  # Parse parameters and coefficients
  if (aberr_module == "dicentrics" | aberr_module == "micronuclei") {
    lambda_est <- case_data[["y"]]
  } else if (aberr_module == "translocations") {
    lambda_est <- case_data[["Fg"]]
  }

  # Generalised fit coefficients and variance-covariance matrix
  general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
  general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

  coeff_C <- general_fit_coeffs[[1]]
  coeff_alpha <- general_fit_coeffs[[2]]
  coeff_beta <- general_fit_coeffs[[3]]

  # Detect fitting model
  fit_is_lq <- isFALSE(coeff_beta == 0)

  # Get estimate for dose depending on linear/linear-quadratic fitting model
  if (fit_is_lq) {
    z <- coeff_alpha^2 + 4 * coeff_beta * (lambda_est - coeff_C)
    dose_est <- (-coeff_alpha + sqrt(z)) / (2 * coeff_beta)
  } else {
    dose_est <- (lambda_est - coeff_C) / coeff_alpha
  }

  # Calculate variance of lambda
  disp <- case_data[["DI"]]

  # Correct value when there's no aberrations
  if (is.nan(disp) | is.na(disp)) {
    disp <- Inf
  }

  if (disp >= 1) {
    # Use empirical error sqrt(var / N) if disp >= 1
    if (aberr_module == "dicentrics" | aberr_module == "micronuclei") {
      lambda_est_sd <- case_data[["y_err"]]
    } else if (aberr_module == "translocations") {
      lambda_est_sd <- case_data[["Fg_err"]]
    }
  } else {
    # Use Poisson error if disp < 1
    lambda_est_sd <- sqrt(case_data[["X"]]) / case_data[["N"]]
  }

  # Get confidence interval of lambda estimates
  lambda_low <- lambda_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd
  lambda_upp <- lambda_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd

  # Get standard error of fraction irradiated by deltamethod()
  if (fit_is_lq) {
    # Formula parameters: {x1, x2, x3, x4} = {C, alpha, beta, lambda_est}
    formula <- paste(
      "~", "(-x2 + sqrt(x2^2 + 4 * x3 *", protracted_g_value, "* (x4 - x1)))", "/",
      "(2 * x3 *", protracted_g_value, ")",
      sep = ""
    )
  } else {
    # Formula parameters: {x1, x2, x4} = {C, alpha, lambda_est}
    formula <- "~ (x4 - x1) / x2"
  }

  cov_extended <- matrix(0, nrow = 4, ncol = 4)
  cov_extended[1:3, 1:3] <- general_fit_var_cov_mat
  cov_extended[4, 4] <- lambda_est_sd^2

  dose_est_sd <- msm::deltamethod(
    g = stats::as.formula(formula),
    mean = c(coeff_C, coeff_alpha, coeff_beta, lambda_est),
    cov = cov_extended
  )

  # Get confidence interval of dose estimates
  dose_low <- dose_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * dose_est_sd
  dose_upp <- dose_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * dose_est_sd

  # Correct negative values
  lambda_low <- correct_negative_vals(lambda_low)
  lambda_upp <- correct_negative_vals(lambda_upp)
  dose_low <- correct_negative_vals(dose_low)
  dose_est <- correct_negative_vals(dose_est)
  dose_upp <- correct_negative_vals(dose_upp)

  # Correct "unrootable" yields and respective doses
  lambda_est_corr <- correct_yield(lambda_est, "estimate", general_fit_coeffs, general_fit_var_cov_mat, conf_int = 0)
  if (lambda_est_corr < lambda_est) {
    lambda_est <- 0
    lambda_low <- 0
    lambda_upp <- 0
    dose_est <- 0
    dose_low <- 0
    dose_upp <- 0
  }

  # Whole-body estimation results
  est_doses <- data.frame(
    yield = c(lambda_low, lambda_est, lambda_upp),
    dose = c(dose_low, dose_est, dose_upp)
  ) %>%
    `row.names<-`(c("lower", "estimate", "upper"))

  # Calculate AIC as a GOF indicator
  AIC <- AIC_from_data(
    general_fit_coeffs, est_doses["estimate", ],
    dose_var = "dose", yield_var = "yield", fit_link = "identity"
  )


  # Return objects
  results_list <- list(
    est_doses = est_doses,
    AIC = AIC,
    conf_int = conf_int
  )

  return(results_list)
}

#' Partial-body dose estimation (Dolphin's method)
#'
#' Partial-body dose estimation using Dolphin's method.
#'
#' @param case_data Case data in data frame form
#' @param fit_coeffs Fitting coefficients matrix
#' @param fit_var_cov_mat Fitting variance-covariance matrix
#' @param conf_int Confidence interval, 95\% by default
#' @param protracted_g_value Protracted G(x) value
#' @param cov Whether the covariances of the regression coefficients should be considered, otherwise only the diagonal of the covariance matrix is used
#' @param genome_factor Genomic conversion factor used in translocations, else 1
#' @param aberr_module Aberration module
#' @param gamma Survival coefficient of irradiated cells
#'
#' @return List containing estimated doses data frame, estimated fraction of irradiated blood data frame, and AIC
#' @export
estimate_partial_body_dolphin <- function(case_data, fit_coeffs, fit_var_cov_mat,
                                          conf_int = 0.95, protracted_g_value, cov = TRUE,
                                          genome_factor = 1, aberr_module, gamma) {

  # Function to get the fisher information matrix
  get_cov_ZIP_ML <- function(lambda, pi, cells) {
    # For the parameters of a ZIP distribution (lambda and pi) where 1-p is the fraction of extra zeros
    info_mat <- matrix(NA, nrow = 2, ncol = 2)
    info_mat[1, 1] <- cells * pi * ((pi - 1) * exp(-lambda) / (1 - pi + pi * exp(-lambda)) + 1 / lambda)
    info_mat[1, 2] <- cells * exp(-lambda) / (1 - pi + pi * exp(-lambda))
    info_mat[2, 1] <- info_mat[1, 2]
    info_mat[2, 2] <- cells * (1 - exp(-lambda)) / (pi * (1 - pi + pi * exp(-lambda)))

    # Solve system
    cov_est <- base::solve(info_mat)

    return(cov_est)
  }

  # Input of the parameter gamma and its variance
  d0 <- 1 / gamma

  # Get fitting model variables
  aberr <- case_data[["X"]]
  cells <- case_data[["N"]]
  cells_0 <- case_data[["C0"]]
  cells_1 <- case_data[["C1"]]

  # Modify results for translocations
  if (aberr_module == "translocations") {
    aberr <- aberr - case_data[["Xc"]]
  }

  # Generalised fit coefficients and variance-covariance matrix
  general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
  general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

  # Parse fitting coefficients
  coeff_C <- general_fit_coeffs[[1]]
  coeff_alpha <- general_fit_coeffs[[2]]
  coeff_beta <- general_fit_coeffs[[3]]

  # Detect fitting model
  fit_is_lq <- isFALSE(coeff_beta == 0)

  # If there are no cells with > 1 dic, the results include only NAs
  # This should be handled somewhere downstream
  if (cells - (cells_0 + cells_1) == 0) {
    # Partial estimation results
    est_doses <- data.frame(
      yield = rep(NA, 3),
      dose = rep(NA, 3)
    )

    # Estimated fraction
    est_frac <- data.frame(
      fraction = rep(NA, 3)
    )
  } else {
    # Get estimates for pi and lambda
    lambda_est <- stats::uniroot(function(yield) {
      yield / (1 - exp(-yield)) - aberr / (cells - cells_0)
    }, c(1e-16, 100))$root

    if (aberr_module == "translocations") {
      lambda_est <- lambda_est / genome_factor
    }

    pi_est <- aberr / (lambda_est * cells)

    # Get the covariance matrix for the parameters of the ZIP distribution
    cov_est <- get_cov_ZIP_ML(lambda_est, pi_est, cells)

    # Calculate dose and derivatives dependig on linear/linear-quadratic fitting model
    if (fit_is_lq) {
      # Update coeff_beta to correct for protracted exposures
      coeff_beta <- coeff_beta * protracted_g_value

      # Auxiliary variable
      z <- coeff_alpha^2 + 4 * coeff_beta * (lambda_est - coeff_C)

      # Get estimate for dose
      dose_est <- (-coeff_alpha + sqrt(z)) / (2 * coeff_beta)

      # Derivatives of regression curve coefs
      deriv_lambda <- (z)^(-0.5)
      deriv_coeff_C <- -(z)^(-0.5)
      deriv_coeff_alpha <- (1 / (2 * coeff_beta)) * (-1 + coeff_alpha * z^(-0.5))
      deriv_coeff_beta <- protracted_g_value * (4 * coeff_beta * (lambda_est - coeff_C) * (z^(-0.5)) + 2 * coeff_alpha - 2 * z^(0.5)) / (4 * coeff_beta^2)
    } else {
      # Get estimate for dose
      dose_est <- (lambda_est - coeff_C) / coeff_alpha

      # Derivatives of regression curve coefs
      deriv_lambda <- 1 / coeff_alpha
      deriv_coeff_C <- -(1 / coeff_alpha)
      deriv_coeff_alpha <- (coeff_C - lambda_est) / coeff_alpha^2
      deriv_coeff_beta <- 0
    }

    if (fit_is_lq) {
      cov_extended <- matrix(0, nrow = 5, ncol = 5)
      cov_extended[1:3, 1:3] <- general_fit_var_cov_mat
      cov_extended[4:5, 4:5] <- cov_est
    } else {
      cov_extended <- matrix(0, nrow = 4, ncol = 4)
      cov_extended[1:3, 1:3] <- general_fit_var_cov_mat
      cov_extended[3:4, 3:4] <- cov_est
    }

    # Get variance of lambda based on delta methods (see Savage et al.)
    lambda_est_sd <- sqrt(cov_est[1, 1])

    # Get confidence interval of lambda estimates
    lambda_low <- lambda_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd
    lambda_upp <- lambda_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd

    if (cov) {
      dose_est_var <-
        (deriv_coeff_C^2) * general_fit_var_cov_mat[1, 1] +
        (deriv_coeff_alpha^2) * general_fit_var_cov_mat[2, 2] +
        (deriv_coeff_beta^2) * general_fit_var_cov_mat[3, 3] +
        (deriv_lambda^2) * (lambda_est_sd^2) +
        2 * (deriv_coeff_C * deriv_coeff_alpha) * general_fit_var_cov_mat[1, 2] +
        2 * (deriv_coeff_C * deriv_coeff_beta) * general_fit_var_cov_mat[1, 3] +
        2 * (deriv_coeff_alpha * deriv_coeff_beta) * general_fit_var_cov_mat[2, 3]
    } else {
      dose_est_var <-
        (deriv_coeff_C^2) * general_fit_var_cov_mat[1, 1] +
        (deriv_coeff_alpha^2) * general_fit_var_cov_mat[2, 2] +
        (deriv_coeff_beta^2) * general_fit_var_cov_mat[3, 3] +
        (deriv_lambda^2) * (lambda_est_sd^2)
    }

    # Get confidence interval of dose estimates
    dose_low <- dose_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * sqrt(dose_est_var)
    dose_upp <- dose_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * sqrt(dose_est_var)

    # Correct negative values
    lambda_low <- correct_negative_vals(lambda_low)
    lambda_upp <- correct_negative_vals(lambda_upp)
    dose_low <- correct_negative_vals(dose_low)
    dose_est <- correct_negative_vals(dose_est)
    dose_upp <- correct_negative_vals(dose_upp)

    # Partial estimation results
    est_doses <- data.frame(
      yield = c(lambda_low, lambda_est, lambda_upp),
      dose = c(dose_low, dose_est, dose_upp)
    ) %>%
      `row.names<-`(c("lower", "estimate", "upper"))

    # Calculate AIC as a GOF indicator
    AIC <- AIC_from_data(
      general_fit_coeffs, est_doses["estimate", ],
      dose_var = "dose", yield_var = "yield", fit_link = "identity"
    )

    # Get estimate for fraction irradiated
    F_est <- pi_est * exp(dose_est / d0) / (1 - pi_est + pi_est * exp(dose_est / d0))

    # Get standard error of fraction irradiated by deltamethod()
    if (fit_is_lq) {
      # Formula parameters: {x1, x2, x3, x4, x5} = {C, alpha, beta, lambda_est, pi_est}
      formula <- paste(
        "~", "x5 * exp((-x2 + sqrt(x2^2 + 4 * x3 * (x4 - x1))) / (2 * x3 *", d0, "))", "/",
        "(1 - x5 + x5 * exp((-x2 + sqrt(x2^2 + 4 * x3 * (x4 - x1))) / (2 * x3 *", d0, ")))",
        sep = ""
      )
      F_est_sd <- msm::deltamethod(
        g = stats::as.formula(formula),
        mean = c(coeff_C, coeff_alpha, coeff_beta, lambda_est, pi_est),
        cov = cov_extended
      )
    } else {
      # Formula parameters: {x1, x2, x3, x4} = {C, alpha, lambda_est, pi_est}
      formula <- paste(
        "~", "x4 * exp((x3 - x1) / (x2 *", d0, "))", "/",
        "1 - x4 + x4 * exp((x3 - x1) / (x2 *", d0, ")))",
        sep = ""
      )
      F_est_sd <- msm::deltamethod(
        g = stats::as.formula(formula),
        mean = c(coeff_C, coeff_alpha, lambda_est, pi_est),
        cov = cov_extended
      )
    }

    # Get confidence interval of fraction irradiated
    F_upp <- F_est + stats::qnorm(conf_int + (1 - conf_int) / 2) * F_est_sd
    F_low <- F_est - stats::qnorm(conf_int + (1 - conf_int) / 2) * F_est_sd

    # Set to zero if F < 0 and to 1 if F > 1
    F_low <- correct_boundary(F_low)
    F_est <- correct_boundary(F_est)
    F_upp <- correct_boundary(F_upp)

    # Estimated fraction
    est_frac <- data.frame(
      fraction = c(F_low, F_est, F_upp)
    ) %>%
      `row.names<-`(c("lower", "estimate", "upper"))
  }

  # Return objects
  results_list <- list(
    est_doses = est_doses,
    est_frac = est_frac,
    AIC = AIC,
    conf_int = conf_int
  )

  return(results_list)
}

#' Heterogeneous dose estimation (Mixed Poisson model)
#'
#' Heterogeneous dose estimation.
#'
#' @param case_data Case data in data frame form
#' @param fit_coeffs Fitting coefficients matrix
#' @param fit_var_cov_mat Fitting variance-covariance matrix
#' @param conf_int_yield Confidence interval of the yield
#' @param conf_int_curve Confidence interval of the curve
#' @param protracted_g_value Protracted G(x) value
#' @param gamma Survival coefficient of irradiated cells
#' @param gamma_error Error of the survival coefficient of irradiated cells
#'
#' @return List containing estimated mixing proportions data frame, estimated yields data frame, estimated doses data frame, estimated fraction of irradiated blood data frame, and AIC
#' @export
estimate_hetero_mixed_poisson <- function(case_data, fit_coeffs, fit_var_cov_mat,
                                          conf_int_yield, conf_int_curve, protracted_g_value,
                                          gamma, gamma_error) {

  # Select translocation counts
  counts <- case_data[1, ] %>%
    dplyr::select(dplyr::contains("C")) %>%
    as.numeric()

  # Get fitting model variables
  cells <- case_data[["N"]]
  cells_0 <- case_data[["C0"]]
  cells_1 <- case_data[["C1"]]

  # Likelihood function
  loglik <- function(coeffs) {
    loglik <- sum(log(coeffs[1] * stats::dpois(y, coeffs[2]) + (1 - coeffs[1]) * stats::dpois(y, coeffs[3])))

    return(-loglik)
  }

  # Function to calculate fractions of irradiated blood
  get_fraction <- function(g, f, mu1, mu2) {
    dose1_est <- project_yield(
      yield = mu1,
      type = "estimate",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = NULL,
      protracted_g_value = protracted_g_value,
      conf_int = 0
    )

    if (mu2 <= 0.01) {
      dose2_est <- 0
    } else {
      dose2_est <- project_yield(
        yield = mu2,
        type = "estimate",
        general_fit_coeffs = general_fit_coeffs,
        general_fit_var_cov_mat = NULL,
        protracted_g_value = protracted_g_value,
        conf_int = 0
      )
    }

    frac <- f / (f + (1 - f) * exp(g * (dose2_est - dose1_est)))

    return(frac)
  }

  # If there are no cells with > 1 dic, the results include only NAs
  # This should be handled somewhere downstream
  if (cells - (cells_0 + cells_1) == 0) {
    # Estimated yields
    est_yields <- data.frame(
      yield1 = rep(NA, 3),
      yield2 = rep(NA, 3)
    )

    # Estimated mixing proportion
    est_mixing_prop <- data.frame(
      y_estimate = rep(NA, 2),
      y_std_err = rep(NA, 2),
      f_estimate = rep(NA, 2),
      f_std_err = rep(NA, 2)
    )

    # Estimated doses
    est_doses <- data.frame(
      dose1 = rep(NA, 3),
      dose2 = rep(NA, 3)
    )

    # Estimated fraction
    est_frac <- data.frame(
      estimate = rep(NA, 2),
      std_err = rep(NA, 2)
    )
  } else {
    # Get cases data and store in vector y
    y <- rep(seq(0, length(counts) - 1, 1), counts)
    x <- c(rep(1, length(y)))

    fit <- mixtools::poisregmixEM(y, x, addintercept = FALSE, k = 2)

    # Generalised fit coefficients and variance-covariance matrix
    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs[, "estimate"])
    general_fit_var_cov_mat <- generalise_fit_var_cov_mat(fit_var_cov_mat)

    # Parse fitting coefficients
    coeff_C <- general_fit_coeffs[[1]]
    coeff_alpha <- general_fit_coeffs[[2]]
    coeff_beta <- general_fit_coeffs[[3]]

    # Input of the variance-covariance matrix of the parameters
    sigma <- matrix(0, nrow = 7, ncol = 7)
    sigma[1:3, 1:3] <- general_fit_var_cov_mat
    sigma[4, 4] <- gamma_error

    # Calculate Maximum Likielihood Estimation
    MLE <- stats::optim(
      par = c(fit$lambda[1], exp(fit$beta)[1], exp(fit$beta)[2]),
      fn = loglik,
      method = c("L-BFGS-B"),
      lower = c(0.01, 0.01, 0.01),
      upper = c(0.99, Inf, Inf),
      hessian = TRUE
    )

    st <- solve(MLE$hessian)
    yield1_est <- MLE$par[2]
    yield2_est <- MLE$par[3]
    frac1 <- MLE$par[1]

    if (yield1_est < yield2_est) {
      yield1_est <- MLE$par[3]
      yield2_est <- MLE$par[2]
      frac1 <- 1 - frac1
      stm <- st
      stm[2, 2] <- st[3, 3]
      stm[3, 3] <- st[2, 2]
      stm[1, 2] <- st[1, 3]
      stm[1, 3] <- st[1, 2]
      stm[2, 1] <- stm[1, 2]
      stm[3, 1] <- stm[1, 3]
      st <- stm
    }

    # WIP: This is not requiered yet
    # sigma[5, 5] <- st[1, 1]
    # sigma[6, 6] <- st[2, 2]
    # sigma[7, 7] <- st[3, 3]
    # sigma[5, 6] <- st[1, 2]
    # sigma[5, 7] <- st[1, 3]
    # sigma[6, 7] <- st[2, 3]
    # sigma[6, 5] <- st[1, 2]
    # sigma[7, 5] <- st[1, 3]
    # sigma[7, 6] <- st[2, 3]

    # Estimated parameters and its standard errors
    estim <- c(frac1, yield1_est, yield2_est)
    std_estim <- sqrt(diag(st))

    yield1_low <- yield1_est - std_estim[2]
    yield1_upp <- yield1_est + std_estim[2]

    yield2_low <- yield2_est - std_estim[3]
    yield2_upp <- yield2_est + std_estim[3]

    # Correct "unrootable" yields
    yield1_est <- correct_yield(yield1_est, "estimate", general_fit_coeffs, general_fit_var_cov_mat, conf_int_curve)
    yield1_low <- correct_yield(yield1_low, "lower", general_fit_coeffs, general_fit_var_cov_mat, conf_int_curve)
    yield1_upp <- correct_yield(yield1_upp, "upper", general_fit_coeffs, general_fit_var_cov_mat, conf_int_curve)

    yield2_est <- correct_yield(yield2_est, "estimate", general_fit_coeffs, general_fit_var_cov_mat, conf_int_curve)
    yield2_low <- correct_yield(yield2_low, "lower", general_fit_coeffs, general_fit_var_cov_mat, conf_int_curve)
    yield2_upp <- correct_yield(yield2_upp, "upper", general_fit_coeffs, general_fit_var_cov_mat, conf_int_curve)

    est_yields <- data.frame(
      yield1 = c(yield1_low, yield1_est, yield1_upp),
      yield2 = c(yield2_low, yield2_est, yield2_upp)
    ) %>%
      `row.names<-`(c("lower", "estimate", "upper"))

    # Estimated mixing proportion
    est_mixing_prop <- data.frame(
      y_estimate = c(estim[2], estim[3]),
      y_std_err = c(std_estim[2], std_estim[3]),
      f_estimate = c(estim[1], 1 - estim[1]),
      f_std_err = rep(std_estim[1], 2)
    ) %>%
      `row.names<-`(c("dose1", "dose2"))

    # Estimated received doses
    dose1_est <- project_yield(
      yield = yield1_est,
      type = "estimate",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = NULL,
      protracted_g_value = protracted_g_value,
      conf_int = 0
    )
    dose1_low <- project_yield(
      yield = yield1_low,
      type = "lower",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = general_fit_var_cov_mat,
      protracted_g_value = protracted_g_value,
      conf_int = conf_int_curve
    )
    dose1_upp <- project_yield(
      yield = yield1_upp,
      type = "upper",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = general_fit_var_cov_mat,
      protracted_g_value = protracted_g_value,
      conf_int = conf_int_curve
    )

    dose2_est <- project_yield(
      yield = yield2_est,
      type = "estimate",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = NULL,
      protracted_g_value = protracted_g_value,
      conf_int = 0
    )
    dose2_low <- project_yield(
      yield = yield2_low,
      type = "lower",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = general_fit_var_cov_mat,
      protracted_g_value = protracted_g_value,
      conf_int = conf_int_curve
    )
    dose2_upp <- project_yield(
      yield = yield2_upp,
      type = "upper",
      general_fit_coeffs = general_fit_coeffs,
      general_fit_var_cov_mat = general_fit_var_cov_mat,
      protracted_g_value = protracted_g_value,
      conf_int = conf_int_curve
    )

    est_doses <- data.frame(
      dose1 = c(dose1_low, dose1_est, dose1_upp),
      dose2 = c(dose2_low, dose2_est, dose2_upp)
    ) %>%
      `row.names<-`(c("lower", "estimate", "upper"))

    # Estimated fraction of irradiated blood for dose dose1
    F1_est <- get_fraction(gamma, frac1, yield1_est, yield2_est)
    F1_est <- correct_boundary(F1_est)
    F2_est <- 1 - F1_est

    # Approximated standard error
    F1_est_sd <- F1_est * (1 - F1_est) * sqrt((dose2_est - dose1_est)^2 * sigma[4, 4] + st[1, 1] / (frac1^2 * (1 - frac1)^2))

    est_frac <- data.frame(
      estimate = c(F1_est, F2_est),
      std_err = rep(F1_est_sd, 2)
    ) %>%
      `row.names<-`(c("dose1", "dose2"))

    # Calculate AIC as a GOF indicator
    est_doses_AIC <- data.frame(
      dose = est_doses["estimate", ] %>% as.numeric(),
      yield = est_yields["estimate", ] %>% as.numeric()
    )

    AIC <- AIC_from_data(
      general_fit_coeffs, est_doses_AIC,
      dose_var = "dose", yield_var = "yield", fit_link = "identity"
    )

    # WIP: This is not required yet
    # Gradient
    # h <- 0.000001
    # if (yield2_est > 0.01) {
    #   c1 <- (get_fraction(coeff_C + h, coeff_alpha, coeff_beta, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c2 <- (get_fraction(coeff_C, coeff_alpha + h, coeff_beta, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c3 <- (get_fraction(coeff_C, coeff_alpha, coeff_beta + h, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c5 <- (get_fraction(coeff_C, coeff_alpha, coeff_beta, gam + h, frac1, yield1_est, yield2_est) - F) / h
    #   c6 <- (get_fraction(coeff_C, coeff_alpha, coeff_beta, gamma, frac1 + h, yield1_est, yield2_est) - F) / h
    #   c7 <- (get_fraction(coeff_C, coeff_alpha, coeff_beta, gamma, frac1, yield1_est + h, yield2_est) - F) / h
    #   c8 <- (get_fraction(coeff_C, coeff_alpha, coeff_beta, gamma, frac1, yield1_est, yield2_est + h) - F) / h
    #   grad <- c(c1, c2, c3, c5, c6, c7, c8)
    #   sqrt(t(grad) %*% sigma %*% grad)
    # }
    # if (yield2_est <= 0.01) {
    #   c1 <- (get_fraction(coeff_C + h, coeff_alpha, coeff_beta, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c2 <- (get_fraction(coeff_C, coeff_alpha + h, coeff_beta, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c3 <- (get_fraction(coeff_C, coeff_alpha, coeff_beta + h, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c5 <- (get_fraction(coeff_C, coeff_alpha, coeff_beta, gam + h, frac1, yield1_est, yield2_est) - F) / h
    #   c6 <- (get_fraction(coeff_C, coeff_alpha, coeff_beta, gamma, frac1 + h, yield1_est, yield2_est) - F) / h
    #   c7 <- (get_fraction(coeff_C, coeff_alpha, coeff_beta, gamma, frac1, yield1_est + h, yield2_est) - F) / h
    #   grad <- c(c1, c2, c3, c5, c6, c7)
    #   sigma2 <- sigma[1:6, 1:6]
    #   sqrt(t(grad) %*% sigma2 %*% grad)
    # }
  }

  # Return objects
  results_list <- list(
    est_mixing_prop = est_mixing_prop,
    est_yields = est_yields,
    est_doses = est_doses,
    est_frac = est_frac,
    AIC = AIC,
    conf_int = c(yield = conf_int_yield, curve = conf_int_curve)
  )

  return(results_list)
}
