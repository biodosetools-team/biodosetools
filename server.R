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
  callModule(module = dicentFittingHotTable, id = "dicent_fitting")
  callModule(module = dicentFittingResults, id = "dicent_fitting")

  # Dose Estimation
  callModule(module = dicentEstimateHotTable, id = "dicent_estimate")
  callModule(module = dicentEstimateFittingCurveHotTable, id = "dicent_estimate")
  callModule(module = dicentEstimateFittingCurve, id = "dicent_estimate")
  callModule(module = dicentEstimateResults, id = "dicent_estimate")

  # Translocations Modules ----

  # Fitting
  transFraction <-
    callModule(module = transFractionToFullGenomeCalc, id = "trans_fitting")
  callModule(module = transFractionToFullGenome, id = "trans_fitting", fraction_value = transFraction)

  callModule(module = transFittingHotTable, id = "trans_fitting", fraction_value = transFraction)
  callModule(module = transChromosomeTable, id = "trans_fitting")
  callModule(module = transFittingResults, id = "trans_fitting")

  # Micronuclei Modules ----

}
