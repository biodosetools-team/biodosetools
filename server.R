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
  callModule(module = transFittingHotTable, id = "trans_fitting")
  callModule(module = transChromosomeTable, id = "trans_fitting")
  transFittingFraction <-
    callModule(module = transFractionToFullGenomeCalc, id = "trans_fitting")
  callModule(module = transFittingResults, id = "trans_fitting", fraction_value = transFittingFraction)

  # Dose Estimation
  callModule(module = transEstimateHotTable, id = "trans_estimate")

  callModule(module = transChromosomeTable, id = "trans_estimate")
  transEstimateFraction <-
    callModule(module = transFractionToFullGenomeCalc, id = "trans_estimate")

  callModule(module = transEstimateFittingCurveHotTable, id = "trans_estimate")
  callModule(module = transEstimateFittingCurve, id = "trans_estimate")
  callModule(module = transEstimateResults, id = "trans_estimate")

  # Micronuclei Modules ----

}
