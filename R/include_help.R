#' Include Markdown help
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
#' @return
#' @export
#'
#' @examples
include_help <- function(...) {
  withMathJax(
    includeMarkdown(
      system.file(..., package = "biodosetools")
      )
    )

}
