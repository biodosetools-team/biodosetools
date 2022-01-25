#' Aberration calculation functions
#'
#' @param data Count or case data.
#' @param aberr_prefix Prefix of the aberrations in the data.
#' @param power Power of aberration.
#' @param X Sum of detected aberrations.
#' @param X2 Quadratic sum of detected aberrations.
#' @param N Number of cells analysed.
#' @param mean Mean.
#' @param var Variance.
#' @param assessment_u Expected \eqn{u}-value of the assessment. For a Poisson distribution this should be unity.
#' @param type Type of input data. Either "count" and "case".
#' @param aberr_module Aberration module.
#' @name calculate_aberr
NULL
# > NULL

#' @rdname calculate_aberr
calculate_aberr_power <- function(data, aberr_prefix = "C", power = 1) {
  # Prepare data
  aberr_data <- data %>%
    .[grep(aberr_prefix, names(.))] %>%
    t() %>%
    as.data.frame()

  powers <- aberr_data %>%
    rownames() %>%
    regmatches(., regexpr("[0-9]+", .)) %>%
    as.numeric() %>%
    `^`(power)

  # Calculate aberration powers
  aberr <- numeric(length = ncol(aberr_data))

  for (i in 1:ncol(aberr_data)) {
    aberr[i] <- sum(aberr_data[, i] * powers)
  }

  return(aberr)
}

#' @rdname calculate_aberr
calculate_aberr_mean <- function(X, N) {
  mean <- X / N

  return(mean)
}

#' @rdname calculate_aberr
calculate_aberr_var <- function(X, X2, N) {
  var <- (X2 - X^2 / N) / (N - 1)

  return(var)
}

#' @rdname calculate_aberr
calculate_aberr_disp_index <- function(mean, var) {
  disp_index <- var / mean

  return(disp_index)
}

#' @rdname calculate_aberr
calculate_aberr_u_value <- function(X, N, mean, var, assessment_u = 1) {
  u_value <- (var / mean - assessment_u) * sqrt((N - 1) / (2 * (1 - 1 / X)))

  return(u_value)
}

#' @rdname calculate_aberr
#' @importFrom rlang .data
init_aberr_table <- function(data, type = c("count", "case"), aberr_module) {
  # Validate parameters
  type <- match.arg(type)

  if (type == "count") {
    data <- data %>%
      dplyr::mutate(
        N = 0,
        X = 0,
        mean = 0,
        var = 0,
        DI = 0,
        u = 0
      ) %>%
      dplyr::select(.data$D, .data$N, .data$X, dplyr::everything()) %>%
      dplyr::mutate(
        D = as.numeric(.data$D)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c(.data$N, .data$X, grep("C", names(.), value = TRUE)),
          .fns = as.integer
        )
      )
  } else if (type == "case") {
    if (aberr_module == "dicentrics" | aberr_module == "micronuclei") {
      data <- data %>%
        dplyr::mutate(
          N = 0,
          X = 0,
          y = 0,
          y_err = 0,
          DI = 0,
          u = 0
        ) %>%
        dplyr::select(.data$N, .data$X, dplyr::everything()) %>%
        dplyr::mutate(
          dplyr::across(
            .cols = c(.data$N, .data$X, grep("C", names(.), value = TRUE)),
            .fns = as.integer
          )
        )
    } else if (aberr_module == "translocations") {
      data <- data %>%
        dplyr::mutate(
          N = 0,
          X = 0,
          Fp = 0,
          Fp_err = 0,
          DI = 0,
          u = 0,
          Xc = 0,
          Fg = 0,
          Fg_err = 0
        ) %>%
        dplyr::select(.data$N, .data$X, dplyr::everything()) %>%
        dplyr::mutate(
          dplyr::across(
            .cols = c(.data$N, .data$X, grep("C", names(.), value = TRUE)),
            .fns = as.integer
          )
        )
    }
  }

  return(data)
}

#' Calculate aberrations table
#'
#' @param data Count or case data.
#' @param type Type of input data. Either "count" and "case".
#' @param assessment_u Expected \eqn{u}-value of the assessment. For a Poisson distribution this should be unity.
#'
#' @return Data frame containing cell count (\eqn{N}), aberrations (\eqn{X}),
#' and other coefficients (dispersion index, \eqn{u}-value, ...), as well as
#' raw count or case \code{data}.
#' @export
#' @importFrom rlang .data
calculate_aberr_table <- function(data, type = c("count", "case"), assessment_u = 1) {
  # Validate parameters
  type <- match.arg(type)

  if (type == "count") {
    data <- data %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        N = as.integer(rowSums(.[grep("C", names(.))])),
        X = calculate_aberr_power(., power = 1),
        X2 = calculate_aberr_power(., power = 2),
        mean = calculate_aberr_mean(.data$X, .data$N),
        var = calculate_aberr_var(.data$X, .data$X2, .data$N),
        DI = calculate_aberr_disp_index(.data$mean, .data$var),
        u = calculate_aberr_u_value(.data$X, .data$N, .data$mean, .data$var, assessment_u = assessment_u)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c(grep("C", names(.), value = TRUE)),
          .fns = as.integer
        )
      ) %>%
      dplyr::select(-.data$X2) %>%
      dplyr::select(.data$D, .data$N, .data$X, dplyr::everything())
  } else if (type == "case") {
    data <- data %>%
      dplyr::mutate(
        N = as.integer(rowSums(.[grep("C", names(.))])),
        X = calculate_aberr_power(., power = 1),
        X2 = calculate_aberr_power(., power = 2),
        var = calculate_aberr_var(.data$X, .data$X2, .data$N),
        mean = calculate_aberr_mean(.data$X, .data$N),
        std_err = sqrt(.data$var / .data$N),
        DI = calculate_aberr_disp_index(.data$mean, .data$var),
        u = calculate_aberr_u_value(.data$X, .data$N, .data$mean, .data$var, assessment_u = assessment_u)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c(.data$N, .data$X, grep("C", names(.), value = TRUE)),
          .fns = as.integer
        )
      ) %>%
      dplyr::select(-.data$X2, -.data$var) %>%
      dplyr::select(.data$N, .data$X, dplyr::everything())
  }

  return(data)
}
