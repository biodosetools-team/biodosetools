# Global variables -----------------------------------------

global_fitting_formulas <- list(
  "Linear quadratic" = c(
    "Y = C + αD + βD²" = "lin-quad"
    # "Y = αD + βD²" = "lin-quad-no-int"
  ),
  "Linear" = c(
    "Y = C + αD" = "lin"
    # "Y = αD" = "lin-no-int"
  )
)

widgetLabel <- function(label, margin_bottom = 0) {
  div(
    style = paste0("margin-bottom: ", margin_bottom, "px;"),
    h6(strong(label))
  )
}
