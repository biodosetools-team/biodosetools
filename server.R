# Libraries ------------------------------------------------

# Server Libs
library(rhandsontable)
library(shinyjs)
library(dplyr)
library(ggplot2)

# Server ---------------------------------------------------

server <- function(input, output) {

  # Dicentrics Modules ----

  # Fitting
  callModule(module = dicentFittingTable, id = "fitting", reactive(input$button_fit))
  callModule(module = dicentFittingResults, id = "fitting", reactive(input$button_fit))

  # Advanced Fitting
  callModule(module = dicentFittingAdvHotTable, id = "adv_fitting")
  callModule(module = dicentFittingAdvResults, id = "adv_fitting")

  # Dose Estimation
  callModule(module = dicentEstimateHotTable, id = "estimate")
  callModule(module = dicentEstimateFittingCurve, id = "estimate")
  callModule(module = dicentEstimateResults, id = "estimate")

  # Translocations Modules ----

  # Advanced Fitting
  transFraction <-
    callModule(module = transFractionToFullGenomeCalc, id = "trans_adv_fitting")
  callModule(module = transFractionToFullGenome, id = "trans_adv_fitting", fraction_value = transFraction)

  callModule(module = transFittingAdvHotTable, id = "trans_adv_fitting", fraction_value = transFraction)
  callModule(module = transChromosomeTable, id = "trans_adv_fitting")
  callModule(module = transFittingAdvResults, id = "trans_adv_fitting")

  # Micronuclei Modules ----

}
