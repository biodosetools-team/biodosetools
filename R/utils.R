.onLoad <- function(libname, pkgname) {
  # Supress YAML warning. See https://github.com/rstudio/rstudio/issues/7545
  options("yaml.eval.expr" = TRUE)
}

#' Fix coefficient matrix names
#'
#' Display coefficient names (`coeff_C`, `coeff_alpha`, `coeff_beta`) to display properly on reports.
#'
#' @param data Data frame or matrix.
#' @param type Type of name replacement. Either "rows" or "cols".
#' @param output Type of output in which the data is rendered. Either "kable" or "rhot".
#'
#' @return Data with fixed rownames and colnames.
#' @noRd
fix_coeff_names <- function(data, type = c("rows", "cols"), output = c("kable", "rhot")) {
  type <- match.arg(type)
  output <- match.arg(output)

  # Select name functions
  if (type == "rows") {
    names_assign <- base::`rownames<-`
    names_read <- base::rownames
  } else if (type == "cols") {
    names_assign <- base::`colnames<-`
    names_read <- base::colnames
  }

  # Select coefficient replacements
  coeffs_old <- c("coeff_C", "coeff_alpha", "coeff_beta")

  if (output == "rhot") {
    coeffs_new <- c("C", "\u03B1", "\u03B2")
  } else if (output == "kable") {
    coeffs_new <- c("$C$", "$\\\\alpha$", "$\\\\beta$")
  }

  # Replace coefficients
  data <- data %>%
    names_assign(
      names_read(.) %>%
        gsub(coeffs_old[[1]], coeffs_new[[1]], .) %>%
        gsub(coeffs_old[[2]], coeffs_new[[2]], .) %>%
        gsub(coeffs_old[[3]], coeffs_new[[3]], .)
    )
  return(data)
}

#' Convert case of a string to title case
#'
#' @param string String to modify.
#'
#' @return String as title case.
#' @noRd
to_title <- function(string) {
  split_string <- strsplit(string, " ")[[1]]
  string <- paste(
    toupper(substring(split_string, 1,1)),
    substring(split_string, 2),
    sep="",
    collapse=" "
  )

  return(string)
}
