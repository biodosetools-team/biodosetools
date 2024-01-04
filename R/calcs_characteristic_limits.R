#' Calculate characteristic limits
#'
#' @param mu0 Background rate
#' @param n1 Number of cells that will be analysed.
#' @param alpha Type I error rate, 0.05 by default.
#' @param beta Type II error rate, 0.1 by default.
#'
#' @return List of characteristic limits (\code{decision_threshold}, \code{detection_limit}).
#' @export
calculate_characteristic_limits <- function(mu0, n1, alpha = 0.05, beta = 0.1) {
  # Decision threshold
  y <- 1:100
  y_d_const <- min(y[which(sapply(y, function(x) stats::ppois(x, mu0 * n1, lower.tail = FALSE)) <= alpha / 2)])

  # Detection limit
  y_z_const <- stats::qchisq(1 - beta, 2 * (y_d_const + 1)) / 2

  # Return objects
  results_list <- list(
    decision_threshold = y_d_const,
    detection_limit = y_z_const
  )

  return(results_list)
}
