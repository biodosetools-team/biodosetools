#' Include Markdown help
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
include_help <- function(file) {
  help_path <- system.file("app/help", file, package = "biodosetools")
  tmp_path <- file.path(tempdir(), basename(help_path))
  file.copy(help_path, tmp_path, overwrite = TRUE)

  htmltools::withMathJax(
    htmltools::includeMarkdown(tmp_path)
  )
}


#' Load RMarkdown report
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
load_rmd_report <- function(...) {
  system.file("app/reports", ..., package = "biodosetools")
}
