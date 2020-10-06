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
#' @name calculate_aberr
NULL
# > NULL

#' @rdname calculate_aberr
#' @export
calculate_aberr_power <- function(data, aberr_prefix = "C", power = 1) {
  purrr::map_df(
    data %>%
      .[grep(aberr_prefix, names(.))] %>%
      t() %>%
      as.data.frame(),
    ~ . *
      data %>%
      .[grep(aberr_prefix, names(.))] %>%
      names() %>%
      stringr::str_extract("[0-9]") %>%
      as.numeric() %>%
      .^power
  ) %>%
    t() %>%
    rowSums()
}

#' @rdname calculate_aberr
#' @export
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
