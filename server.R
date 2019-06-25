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
  callModule(id = "dicent_fitting", module = generalFittingCountsHotTable)
  callModule(id = "dicent_fitting", module = generalFittingResults, aberr_module = "dicentrics")

  # Dose Estimation
  callModule(id = "dicent_estimate", module = generalEstimateCaseHotTable, aberr_module = "dicentrics")
  callModule(id = "dicent_estimate", module = generalEstimateFittingCurveHotTables)
  callModule(id = "dicent_estimate", module = generalEstimateFittingCurve, aberr_module = "dicentrics")
  callModule(id = "dicent_estimate", module = dicentEstimateResults)

  # Translocations Modules ----

  # Fitting
  callModule(id = "trans_fitting", module = transChromosomeTable)
  transFittingFraction <-
    callModule(id = "trans_fitting", module = transFractionToFullGenomeCalc)
  callModule(id = "trans_fitting", module = generalFittingCountsHotTable)
  callModule(id = "trans_fitting", module = generalFittingResults, aberr_module = "translocations", fraction_value = transFittingFraction)

  # Dose Estimation
  callModule(id = "trans_estimate", module = transChromosomeTable)
  transEstimateFraction <-
    callModule(id = "trans_estimate", module = transFractionToFullGenomeCalc)
  callModule(id = "trans_estimate", module = generalEstimateCaseHotTable, aberr_module = "translocations", fraction_value = transEstimateFraction)

  callModule(id = "trans_estimate", module = generalEstimateFittingCurveHotTables)
  callModule(id = "trans_estimate", module = generalEstimateFittingCurve, aberr_module = "translocations")
  callModule(id = "trans_estimate", module = transEstimateResults)

  # Micronuclei Modules ----
}
