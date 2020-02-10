#' Biodose Tools
#'
#' Function to run Biodose Tools.
#'
#' @export
runApp <- function() {
  appDir <- system.file("app", package = "biodosetools")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `biodosetools`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
