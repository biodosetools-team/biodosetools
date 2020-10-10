.onLoad <- function(libname, pkgname) {
  # Supress YAML warning. See https://github.com/rstudio/rstudio/issues/7545
  options("yaml.eval.expr" = TRUE)
}

#' Fix coefficient matrix names
#'
#' Fix coefficient names (`coeff_C`, `coeff_alpha`, `coeff_beta`) to display properly on reports.
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

#' Fix data frame variable names
#'
#' Fix data colnames to display properly on reports.
#'
#' @param data Data frame or matrix.
#' @param type Type of input data. Either "count" and "case".
#' @param output Type of output in which the data is rendered. Only "kable" supported at the moment.
#'
#' @return Data with fixed colnames.
#' @noRd
fix_count_data_names <- function(data, type = c("count", "case"), output = "kable") {
  type <- match.arg(type)
  output <- match.arg(output)

  col_names <- colnames(data)

  counts_headers <- grep("C[0-9]", x = col_names, value = TRUE)
  counts_headers <- paste0("$C_{", regmatches(counts_headers, regexpr("[0-9]", counts_headers)), "}$")

  if (type == "count") {
    other_headers <- grep("C[0-9]", x = col_names, value = TRUE, invert = TRUE) %>%
      gsub("^D$", "$D$ (Gy)", .) %>%
      gsub("^N$", "$N$", .) %>%
      gsub("^X$", "$X$", .) %>%
      gsub("^mean$", "$\\\\bar{y}$", .) %>%
      gsub("^var$", "$\\\\sigma$", .) %>%
      gsub("^DI$", "$\\\\sigma^{2} / \\\\bar{y}$", .) %>%
      gsub("^u$", "$u$", .)

    colnames(data) <- c(other_headers[1:3], counts_headers, other_headers[4:length(other_headers)])
  } else if (type == "case") {
    other_headers <- grep("C[0-9]", x = col_names, value = TRUE, invert = TRUE) %>%
      gsub("^N$", "$N$", .) %>%
      gsub("^X$", "$X$", .) %>%
      gsub("^y$", "$y$", .) %>%
      gsub("^y_err$", "$y_{err}$", .) %>%
      gsub("^Fp$", "$F_{P}$", .) %>%
      gsub("^Fp_err$", "$F_{P,err}$", .) %>%
      gsub("^Fg$", "$F_{G}$", .) %>%
      gsub("^Xc$", "$X_{C}$", .) %>%
      gsub("^Fg_err$", "$F_{G,err}$", .) %>%
      gsub("^DI$", "$\\\\sigma^{2} / \\\\bar{y}$", .) %>%
      gsub("^u$", "$u$", .)

    colnames(data) <- c(other_headers[1:2], counts_headers, other_headers[3:length(other_headers)])
  }

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
