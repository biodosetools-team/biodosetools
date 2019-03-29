# Libraries ------------------------------------------------

# Server Libs
library(rhandsontable)
library(shinyjs)
library(dplyr)
library(ggplot2)

# Server ---------------------------------------------------

server <- function(input, output) {

  # Fitting ----
  callModule(module = fittingTable, id = "fitting", reactive(input$button_fit))
  callModule(module = fittingResults, id = "fitting", reactive(input$button_fit))

  # Advanced Fitting ----
  callModule(module = fittingAdvHotTable, id = "adv_fitting")
  callModule(module = fittingAdvResults, id = "adv_fitting")

  callModule(module = transFittingAdvHotTable, id = "trans_adv_fitting")
  callModule(module = transChromosomeTable, id = "trans_adv_fitting")
  callModule(module = transFittingAdvResults, id = "trans_adv_fitting")

  # Estimate ----
  callModule(module = estimateHotTable, id = "estimate")
  callModule(module = estimateFittingCurve, id = "estimate")
  callModule(module = estimateResults, id = "estimate")

}
