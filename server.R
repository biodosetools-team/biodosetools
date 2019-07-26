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
  callModule(
    id = "dicent_fitting",
    module = generalFittingCountsHotTable
  )
  callModule(
    id = "dicent_fitting",
    module = generalFittingResults,
    aberr_module = "dicentrics"
  )

  # Dose Estimation
  callModule(
    id = "dicent_estimate",
    module = generalEstimateCaseHotTable,
    aberr_module = "dicentrics"
  )
  callModule(
    id = "dicent_estimate",
    module = generalEstimateFittingCurveHotTables
  )
  callModule(
    id = "dicent_estimate",
    module = generalEstimateFittingCurve,
    aberr_module = "dicentrics"
  )
  callModule(
    id = "dicent_estimate",
    module = generalEstimateResults,
    aberr_module = "dicentrics"
  )

  # Translocations Modules ----

  # Help
  callModule(
    id = "trans_fitting",
    module = helpChromosomeHotTable
  )
  callModule(
    id = "trans_estimate",
    module = helpChromosomeHotTable
  )

  # Fitting
  callModule(
    id = "trans_fitting",
    module = transChromosomeTable
  )
  callModule(
    id = "trans_fitting",
    module = transFractionToFullGenomeCalc
  ) -> transFittingFraction
  callModule(
    id = "trans_fitting",
    module = generalFittingCountsHotTable
  )
  callModule(
    id = "trans_fitting",
    module = generalFittingResults,
    aberr_module = "translocations",
    genome_fraction = transFittingFraction
  )

  # Dose Estimation
  callModule(
    id = "trans_estimate",
    module = transChromosomeTable
  )
  callModule(
    id = "trans_estimate",
    module = transFractionToFullGenomeCalc
  ) -> transEstimateFraction
  callModule(
    id = "trans_estimate",
    module = generalEstimateCaseHotTable,
    aberr_module = "translocations",
    genome_fraction = transEstimateFraction
  )

  callModule(
    id = "trans_estimate",
    module = generalEstimateFittingCurveHotTables
  )
  callModule(
    id = "trans_estimate",
    module = generalEstimateFittingCurve,
    aberr_module = "translocations"
  )
  callModule(
    id = "trans_estimate",
    module = generalEstimateResults,
    aberr_module = "translocations",
    genome_fraction = transEstimateFraction
  )

  # Micronuclei Modules ----

  # Fitting
  callModule(
    id = "micro_fitting",
    module = generalFittingCountsHotTable
  )
  callModule(
    id = "micro_fitting",
    module = generalFittingResults,
    aberr_module = "micronuclei"
  )

  # Dose Estimation
  callModule(
    id = "micro_estimate",
    module = generalEstimateCaseHotTable,
    aberr_module = "micronuclei"
  )
  callModule(
    id = "micro_estimate",
    module = generalEstimateFittingCurveHotTables
  )
  callModule(
    id = "micro_estimate",
    module = generalEstimateFittingCurve,
    aberr_module = "micronuclei"
  )
  callModule(
    id = "micro_estimate",
    module = generalEstimateResults,
    aberr_module = "micronuclei"
  )
}
