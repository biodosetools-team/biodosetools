#' Fix coefficient matrix names
#'
#' Display coefficient names (`coeff_C`, `coeff_alpha`, `coeff_beta`) to display properly on reports.
#'
#' @param data Data frame or matrix.
#' @param type Type of name replacement. Either "rows" or "cols".
#'
#' @return Data with fixed rownames and colnames.
#'
#' @noRd
fix_coeff_names <- function(data, type = c("rows", "cols")) {
  type <- match.arg(type)
  if (type == "rows") {
    names_assign <- base::`rownames<-`
    names_read <- base::rownames
  } else if (type == "cols") {
    names_assign <- base::`colnames<-`
    names_read <- base::colnames
  }

  data <- data %>%
    names_assign(
      names_read(.) %>%
        gsub("coeff_C", "$C$", .) %>%
        gsub("coeff_alpha", "$\\\\alpha$", .) %>%
        gsub("coeff_beta", "$\\\\beta$", .)
    )
  return(data)
}
