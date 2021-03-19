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
#' @param assessment_u Expected u-value of the assessment. For a Poisson distribution this should be unity.
#' @param type Type of input data. Either "count" and "case".
#' @name calculate_aberr
NULL
# > NULL

#' @rdname calculate_aberr
#' @export
calculate_aberr_power <- function(data, aberr_prefix = "C", power = 1) {
  aberr <- purrr::map_df(
    data %>%
      .[grep(aberr_prefix, names(.))] %>%
      t() %>%
      as.data.frame(),
    ~ . *
      data %>%
        .[grep(aberr_prefix, names(.))] %>%
        names() %>%
        regmatches(., regexpr("[0-9]", .)) %>%
        as.numeric() %>%
        .^power
  ) %>%
    t() %>%
    rowSums() %>%
    as.integer()

  return(aberr)
}

#' @rdname calculate_aberr
#' @export
calculate_aberr_mean <- function(X, N) {
  mean <- X / N

  return(mean)
}

#' @rdname calculate_aberr
#' @export
calculate_aberr_var <- function(X, X2, N) {
  var <- (X2 - X^2 / N) / (N - 1)

  return(var)
}

#' @rdname calculate_aberr
#' @export
calculate_aberr_disp_index <- function(mean, var) {
  disp_index <- var / mean

  return(disp_index)
}

#' @rdname calculate_aberr
#' @export
calculate_aberr_u_value <- function(X, N, mean, var, assessment_u = 1) {
  u_value <- (var / mean - assessment_u) * sqrt((N - 1) / (2 * (1 - 1 / X)))

  return(u_value)
}

#' @rdname calculate_aberr
#' @export
#' @importFrom rlang .data
calculate_aberr_table <- function(data, type = c("count", "case")) {
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
        u = calculate_aberr_u_value(.data$X, .data$N, .data$mean, .data$var, assessment_u = 1.00)
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
        std_err = sqrt(.data$var / .data$N),
        mean = calculate_aberr_mean(.data$X, .data$N),
        DI = calculate_aberr_disp_index(.data$mean, .data$var),
        u = calculate_aberr_u_value(.data$X, .data$N, .data$mean, .data$var, assessment_u = 1.00)
      ) %>%
      dplyr::mutate_at(
        c("X", "N", grep("C", names(.), value = TRUE)),
        as.integer
      ) %>%
      dplyr::select(-.data$X2, -.data$var) %>%
      dplyr::select(.data$N, .data$X, dplyr::everything())
  }

  return(data)
}
