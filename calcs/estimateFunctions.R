# AIC and logLik from data ----
AIC_from_data <- function(general_fit_coeffs, data, dose_var = "dose", yield_var = "yield", fit_link = "identity") {

  # Manual log-likelihood function
  loglik_from_data <- function(data, fit_link) {
    if (fit_link == "identity") {
      loglik <- - yield_fun(data[[dose_var]], 1) +
        log(yield_fun(data[[dose_var]], 1)) * data[[yield_var]] -
        log(factorial(data[[yield_var]]))
    } else if (fit_link == "log") {
      loglik <- - exp(yield_fun(data[[dose_var]], 1)) +
        yield_fun(data[[dose_var]], 1) * data[[yield_var]] -
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

# Calcs: whole-body estimation
estimate_whole_body <- function(case_data, conf_int_yield, conf_int_curve, protracted_g_value) {

  aberr <- case_data[["X"]]
  cells <- case_data[["N"]]

  if (aberr_module == "dicentrics") {
    yield_est <- case_data[["y"]]
  }

  # Modify results for translocations
  if (aberr_module == "translocations") {
    aberr <- aberr - case_data[["Xc"]]
    yield_est <- case_data[["Fg"]]
    genome_fraction <- genome_fraction$genome_fraction()
  } else {
    genome_fraction <- 1
  }

  # Calculate CI using Exact Poisson tests
  aberr_row <-  poisson.test(x = round(aberr, 0), conf.level = conf_int_yield)[["conf.int"]]

  aberr_low <- aberr_row[1]
  aberr_upp <- aberr_row[2]

  yield_low <- aberr_low / (cells * genome_fraction)
  yield_upp <- aberr_upp / (cells * genome_fraction)
  # TODO: possible modification IAEA§9.7.3

  # Correct "unrootable" yields
  yield_est <- correct_yield(yield_est)
  yield_low <- correct_yield(yield_low)
  yield_upp <- correct_yield(yield_upp)

  # Calculate projections
  dose_est <- project_yield_estimate(yield_est)
  dose_low <- project_yield_lower(yield_low, conf_int_curve)
  dose_upp <- project_yield_upper(yield_upp, conf_int_curve)

  # Whole-body estimation results
  est_doses <- data.frame(
    yield = c(yield_low, yield_est, yield_upp),
    dose  = c(dose_low, dose_est, dose_upp)
  ) %>%
    `row.names<-`(c("lower", "estimate", "upper"))

  # Calculate AIC as a GOF indicator
  AIC <- AIC_from_data(general_fit_coeffs, est_doses["estimate",],
                       dose_var = "dose", yield_var = "yield", fit_link = "identity")


  # Return objects
  results_list <- list(
    est_doses = est_doses,
    AIC       = AIC
  )

  return(results_list)
}

# Calcs: whole-body estimation
estimate_whole_body_delta <- function(case_data, general_fit_coeffs, general_var_cov_mat,
                                      conf_int, protracted_g_value, cov = TRUE) {
  # cov: TRUE if the covariances of the regression coefficients should be considered,
  #      otherwise only the diagonal of the covariance matrix is used

  if (aberr_module == "dicentrics" | aberr_module == "micronuclei") {
    lambda_est <- case_data[["y"]]
  } else if (aberr_module == "translocations") {
    lambda_est <- case_data[["Fg"]]
  }

  C <- general_fit_coeffs[[1]]
  α <- general_fit_coeffs[[2]]
  β <- general_fit_coeffs[[3]]

  var_cov_mat <- general_var_cov_mat

  # Detect fitting model
  fit_is_lq <- isFALSE(β == 0)

  # Calculate dose and derivatives dependig on linear/linear-quadratic fitting model
  if (fit_is_lq) {
    # Update β to correct for protracted exposures
    β <- β * protracted_g_value

    # Auxiliary variable
    z <- α^2 + 4 * β * (lambda_est - C)

    # Get estimate for dose
    dose_est <- (-α + sqrt(z)) / (2 * β)

    # Derivatives of regression curve coefs
    deriv_lambda <- (z)^(-0.5)
    deriv_C <- -(z)^(-0.5)
    deriv_α <- (1 / (2 * β)) * (-1 + α * z^(-0.5))
    deriv_β <- protracted_g_value * (4 * β * (lambda_est - C) * (z^(-0.5)) + 2 * α - 2 * z^(0.5)) / (4 * β^2)
  } else {
    # Get estimate for dose
    dose_est <- (lambda_est - C) / α

    # Derivatives of regression curve coefs
    deriv_lambda <- 1 / α
    deriv_C <- - (1 / α)
    deriv_α <- (C - lambda_est) / α^2
    deriv_β <- 0
  }

  # Get variance of lambda assuming Poisson
  if (aberr_module == "dicentrics" | aberr_module == "micronuclei") {
    lambda_est_sd <- case_data[["y_err"]]
  } else if (aberr_module == "translocations") {
    lambda_est_sd <- case_data[["Fg_err"]]
  }

  # Get confidence interval of lambda estimates
  lambda_low <- lambda_est - qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd
  lambda_upp <- lambda_est + qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd

  if (cov) {
    dose_est_var <-
      (deriv_C^2) * var_cov_mat[1, 1] +
      (deriv_α^2) * var_cov_mat[2, 2] +
      (deriv_β^2) * var_cov_mat[3, 3] +
      (deriv_lambda^2) * (lambda_est_sd^2) +
      2 * (deriv_C * deriv_α) * var_cov_mat[1, 2] +
      2 * (deriv_C * deriv_β) * var_cov_mat[1, 3] +
      2 * (deriv_α * deriv_β) * var_cov_mat[2, 3]
  } else {
    dose_est_var <-
      (deriv_C^2) * var_cov_mat[1, 1] +
      (deriv_α^2) * var_cov_mat[2, 2] +
      (deriv_β^2) * var_cov_mat[3, 3] +
      (deriv_lambda^2) * (lambda_est_sd^2)
  }

  # Get confidence interval of dose estimates
  dose_low <- dose_est - qnorm(conf_int + (1 - conf_int) / 2) * sqrt(dose_est_var)
  dose_upp <- dose_est + qnorm(conf_int + (1 - conf_int) / 2) * sqrt(dose_est_var)

  # Correct negative values
  lambda_low <- correct_negative_vals(lambda_low)
  lambda_upp <- correct_negative_vals(lambda_upp)
  dose_low <- correct_negative_vals(dose_low)
  dose_est <- correct_negative_vals(dose_est)
  dose_upp <- correct_negative_vals(dose_upp)

  # Whole-body estimation results
  est_doses <- data.frame(
    yield = c(lambda_low, lambda_est, lambda_upp),
    dose  = c(dose_low, dose_est, dose_upp)
  ) %>%
    `row.names<-`(c("lower", "estimate", "upper"))

  # Calculate AIC as a GOF indicator
  AIC <- AIC_from_data(general_fit_coeffs, est_doses["estimate",],
                       dose_var = "dose", yield_var = "yield", fit_link = "identity")


  # Return objects
  results_list <- list(
    est_doses = est_doses,
    AIC       = AIC
  )

  return(results_list)
}

# Calcs: partial dose estimation
estimate_partial_dolphin <- function(case_data, general_fit_coeffs, general_var_cov_mat, fraction_coeff,
                                     conf_int, protracted_g_value, cov = TRUE) {
  # cov: TRUE if the covariances of the regression coefficients should be considered,
  #      otherwise only the diagonal of the covariance matrix is used

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
  if (fraction_coeff == "gamma") {
    d0 <- 1 / input$gamma_coeff
  } else if (fraction_coeff == "d0") {
    d0 <- input$d0_coeff
  }

  # Get fitting model variables
  aberr <- case_data[["X"]]
  cells <- case_data[["N"]]
  cells_0 <- case_data[["C0"]]
  cells_1 <- case_data[["C1"]]

  # Modify results for translocations
  if (aberr_module == "translocations") {
    aberr <- aberr - case_data[["Xc"]]
  }

  C <- general_fit_coeffs[[1]]
  α <- general_fit_coeffs[[2]]
  β <- general_fit_coeffs[[3]]
  var_cov_mat <- general_var_cov_mat

  # Detect fitting model
  fit_is_lq <- isFALSE(β == 0)

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
    lambda_est <- uniroot(function(yield) {
      yield / (1 - exp(-yield)) - aberr / (cells - cells_0)
    }, c(1e-16, 100))$root

    if (aberr_module == "translocations") {
      genome_fraction <- genome_fraction$genome_fraction()
      lambda_est <- lambda_est / genome_fraction
    }

    pi_est <- aberr / (lambda_est * cells)

    # Get the covariance matrix for the parameters of the ZIP distribution
    cov_est <- get_cov_ZIP_ML(lambda_est, pi_est, cells)

    # Calculate dose and derivatives dependig on linear/linear-quadratic fitting model
    if (fit_is_lq) {
      # Update β to correct for protracted exposures
      β <- β * protracted_g_value

      # Auxiliary variable
      z <- α^2 + 4 * β * (lambda_est - C)

      # Get estimate for dose
      dose_est <- (-α + sqrt(z)) / (2 * β)

      # Derivatives of regression curve coefs
      deriv_lambda <- (z)^(-0.5)
      deriv_C <- -(z)^(-0.5)
      deriv_α <- (1 / (2 * β)) * (-1 + α * z^(-0.5))
      deriv_β <- protracted_g_value * (4 * β * (lambda_est - C) * (z^(-0.5)) + 2 * α - 2 * z^(0.5)) / (4 * β^2)
    } else {
      # Get estimate for dose
      dose_est <- (lambda_est - C) / α

      # Derivatives of regression curve coefs
      deriv_lambda <- 1 / α
      deriv_C <- - (1 / α)
      deriv_α <- (C - lambda_est) / α^2
      deriv_β <- 0
    }

    # Get the covariance matrix for the parameters of the ZIP distribution
    cov_est <- get_cov_ZIP_ML(lambda_est, pi_est, cells)

    if (fit_is_lq) {
      cov_extended <- matrix(0, nrow = 5, ncol = 5)
      cov_extended[1:3, 1:3] <- var_cov_mat
      cov_extended[4:5, 4:5] <- cov_est
    } else {
      cov_extended <- matrix(0, nrow = 4, ncol = 4)
      cov_extended[1:3, 1:3] <- var_cov_mat
      cov_extended[3:4, 3:4] <- cov_est
    }

    # Get variance of lambda based on delta methods (see Savage et al.)
    lambda_est_sd <- sqrt(cov_est[1, 1])

    # Get confidence interval of lambda estimates
    lambda_low <- lambda_est - qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd
    lambda_upp <- lambda_est + qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd

    if (cov) {
      dose_est_var <-
        (deriv_C^2) * var_cov_mat[1, 1] +
        (deriv_α^2) * var_cov_mat[2, 2] +
        (deriv_β^2) * var_cov_mat[3, 3] +
        (deriv_lambda^2) * (lambda_est_sd^2) +
        2 * (deriv_C * deriv_α) * var_cov_mat[1, 2] +
        2 * (deriv_C * deriv_β) * var_cov_mat[1, 3] +
        2 * (deriv_α * deriv_β) * var_cov_mat[2, 3]
    } else {
      dose_est_var <-
        (deriv_C^2) * var_cov_mat[1, 1] +
        (deriv_α^2) * var_cov_mat[2, 2] +
        (deriv_β^2) * var_cov_mat[3, 3] +
        (deriv_lambda^2) * (lambda_est_sd^2)
    }

    # Get confidence interval of dose estimates
    dose_low <- dose_est - qnorm(conf_int + (1 - conf_int) / 2) * sqrt(dose_est_var)
    dose_upp <- dose_est + qnorm(conf_int + (1 - conf_int) / 2) * sqrt(dose_est_var)

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
    AIC <- AIC_from_data(general_fit_coeffs, est_doses["estimate",],
                         dose_var = "dose", yield_var = "yield", fit_link = "identity")


    # Get estimate for fraction irradiated
    F_est <- pi_est * exp(dose_est / d0) / (1 - pi_est + pi_est * exp(dose_est / d0))

    # Get standard error of fraction irradiated by deltamethod
    if (fit_is_lq) {
      # x5: pi_est, x4: lambda_est, x1: C, x2: alpha, x3: beta
      formula <- paste(
        "~ x5 * exp((-x2 + sqrt(x2^2 + 4 * x3 * (x4 - x1))) / (2 * x3 *", d0,")) /
            (1 - x5 + x5 * exp((-x2 + sqrt(x2^2 + 4 * x3 * (x4 - x1))) / (2 * x3 *", d0,")))",
        sep = ""
      )
      F_est_sd <- msm::deltamethod(as.formula(formula), mean = c(C, α, β, lambda_est, pi_est), cov = cov_extended)
    } else {
      # x4: pi_est, x3: lambda_est, x1: C, x2: alpha
      formula <- paste(
        "~ x4 * exp((x3 - x1) / (x2 *", d0,")) /
            (1 - x4 + x4 * exp((x3 - x1) / (x2 *", d0,")))",
        sep = ""
      )
      F_est_sd <- msm::deltamethod(as.formula(formula), mean = c(C, α, lambda_est, pi_est), cov = cov_extended)
    }

    # Get confidence interval of fraction irradiated
    F_upp <- F_est + qnorm(conf_int + (1 - conf_int) / 2) * F_est_sd
    F_low <- F_est - qnorm(conf_int + (1 - conf_int) / 2) * F_est_sd

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
    est_frac  = est_frac,
    AIC       = AIC
  )

  return(results_list)
}

# Calcs: heterogeneous dose estimation
estimate_hetero <- function(case_data, general_fit_coeffs, general_var_cov_mat, fraction_coeff,
                            conf_int_yield, conf_int_curve, protracted_g_value) {

  # Select translocation counts
  counts <- case_data[1, ] %>%
    dplyr::select(contains("C")) %>%
    as.numeric()

  # Get fitting model variables
  cells <- case_data[["N"]]
  cells_0 <- case_data[["C0"]]
  cells_1 <- case_data[["C1"]]

  # Likelihood function
  loglik <- function(coeffs) {
    loglik <- sum(log(coeffs[1] * dpois(y, coeffs[2]) + (1 - coeffs[1]) * dpois(y, coeffs[3])))

    return(-loglik)
  }

  # Function to calculate fractions of irradiated blood
  get_fraction <- function(g, f, mu1, mu2) {
    dose1_est <- project_yield_estimate(mu1)

    if (mu2 <= 0.01) {
      dose2_est <- 0
    } else {
      dose2_est <- project_yield_estimate(mu2)
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
      y_std_err =  rep(NA, 2),
      f_estimate = rep(NA, 2),
      f_std_err =  rep(NA, 2)
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

    # Get fitting model variables
    C <- general_fit_coeffs[[1]]
    α <- general_fit_coeffs[[2]]
    β <- general_fit_coeffs[[3]]
    var_cov_mat <- general_var_cov_mat

    # Input of the variance-covariance matrix of the parameters
    sigma <- numeric(49)
    dim(sigma) <- c(7, 7)
    sigma[1, 1] <- var_cov_mat[1, 1]
    sigma[2, 2] <- var_cov_mat[2, 2]
    sigma[3, 3] <- var_cov_mat[3, 3]
    sigma[1, 2] <- var_cov_mat[1, 2]
    sigma[1, 3] <- var_cov_mat[1, 3]
    sigma[2, 3] <- var_cov_mat[2, 3]
    sigma[2, 1] <- sigma[1, 2]
    sigma[3, 1] <- sigma[1, 3]
    sigma[3, 2] <- sigma[2, 3]

    # Input of the parameter gamma and its variance
    if (fraction_coeff == "gamma") {
      gamma <- input$gamma_coeff
      sigma[4, 4] <- input$gamma_error
    } else if (fraction_coeff == "d0"){
      gamma <- 1 / input$d0_coeff
      sigma[4, 4] <- 0
    }

    # Calculate Maximum Likielihood Estimation
    MLE <- optim(
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
    yield1_est <- correct_yield(yield1_est)
    yield1_low <- correct_yield(yield1_low)
    yield1_upp <- correct_yield(yield1_upp)

    yield2_est <- correct_yield(yield2_est)
    yield2_low <- correct_yield(yield2_low)
    yield2_upp <- correct_yield(yield2_upp)

    est_yields <- data.frame(
      yield1 = c(yield1_low, yield1_est, yield1_upp),
      yield2 = c(yield2_low, yield2_est, yield2_upp)
    ) %>%
      `row.names<-`(c("lower", "estimate", "upper"))

    # Estimated mixing proportion
    est_mixing_prop <- data.frame(
      y_estimate = c(estim[2], estim[3]),
      y_std_err =  c(std_estim[2], std_estim[3]),
      f_estimate = c(estim[1], 1 - estim[1]),
      f_std_err =  rep(std_estim[1], 2)
    ) %>%
      `row.names<-`(c("dose1", "dose2"))

    # Estimated received doses
    dose1_est <- project_yield_estimate(yield1_est)
    dose1_low <- project_yield_lower(yield1_low, conf_int_curve)
    dose1_upp <- project_yield_upper(yield1_upp, conf_int_curve)

    dose2_est <- project_yield_estimate(yield2_est)
    dose2_low <- project_yield_lower(yield2_low, conf_int_curve)
    dose2_upp <- project_yield_upper(yield2_upp, conf_int_curve)

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
      dose = est_doses["estimate",] %>% as.numeric(),
      yield = est_yields["estimate",] %>% as.numeric()
    )

    AIC <- AIC_from_data(general_fit_coeffs, est_doses_AIC,
                         dose_var = "dose", yield_var = "yield", fit_link = "identity")

    # WIP: This is not requiered yet
    # Gradient
    # h <- 0.000001
    # if (yield2_est > 0.01) {
    #   c1 <- (get_fraction(C + h, α, β, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c2 <- (get_fraction(C, α + h, β, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c3 <- (get_fraction(C, α, β + h, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c5 <- (get_fraction(C, α, β, gam + h, frac1, yield1_est, yield2_est) - F) / h
    #   c6 <- (get_fraction(C, α, β, gamma, frac1 + h, yield1_est, yield2_est) - F) / h
    #   c7 <- (get_fraction(C, α, β, gamma, frac1, yield1_est + h, yield2_est) - F) / h
    #   c8 <- (get_fraction(C, α, β, gamma, frac1, yield1_est, yield2_est + h) - F) / h
    #   grad <- c(c1, c2, c3, c5, c6, c7, c8)
    #   sqrt(t(grad) %*% sigma %*% grad)
    # }
    # if (yield2_est <= 0.01) {
    #   c1 <- (get_fraction(C + h, α, β, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c2 <- (get_fraction(C, α + h, β, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c3 <- (get_fraction(C, α, β + h, gamma, frac1, yield1_est, yield2_est) - F) / h
    #   c5 <- (get_fraction(C, α, β, gam + h, frac1, yield1_est, yield2_est) - F) / h
    #   c6 <- (get_fraction(C, α, β, gamma, frac1 + h, yield1_est, yield2_est) - F) / h
    #   c7 <- (get_fraction(C, α, β, gamma, frac1, yield1_est + h, yield2_est) - F) / h
    #   grad <- c(c1, c2, c3, c5, c6, c7)
    #   sigma2 <- sigma[1:6, 1:6]
    #   sqrt(t(grad) %*% sigma2 %*% grad)
    # }
  }

  # Return objects
  results_list <- list(
    est_mixing_prop = est_mixing_prop,
    est_yields      = est_yields,
    est_doses       = est_doses,
    est_frac        = est_frac,
    AIC             = AIC
  )

  return(results_list)
}
