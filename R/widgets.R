#' Help modal trigger button
#'
#' @param inputId id of the button
#' @param id_modal id of the modal dialogue
#'
#' @noRd
help_modal_button <- function(inputId, id_modal) {
  shinyBS::bsButton(
    class = "modal-help-button",
    inputId,
    label = "",
    icon = icon("question"),
    style = "default", size = "default"
  ) %>%
    bsplus::bs_attach_modal(id_modal)
}

# Function: innerColumn ----
innerColumn <- function(width, ..., offset = 0) {
  if (!is.numeric(width)) { #|| (width < 1) || (width > 12)) {
    stop("column width must be between 1 and 12")
  }
  colClass <- paste0("col-inner-", width)
  if (offset > 0) {
    colClass <- paste0(colClass, " col-sm-offset-", offset)
  }
  div(class = colClass, ...)
}
