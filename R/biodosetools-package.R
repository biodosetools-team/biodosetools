#' \code{biodosetools} package
#'
#' Shiny App To Be Used By Biological Dosimetry Laboratories
#'
#' See the README on
#' \href{https://github.com/biodosetools-team/biodosetools/}{GitHub}
#'
#' @docType package
#' @name biodosetools
#' @aliases biodosetools-package
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(".")

  # Fix for tidyselect::where()
  # See https://github.com/r-lib/tidyselect/issues/201
  utils::globalVariables("where")
}
