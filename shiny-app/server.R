# Libraries ------------------------------------------------

# Server Libs
library(rhandsontable)
library(shinyjs)
library(dplyr)

# Server ---------------------------------------------------

server <- function(input, output) {

  # Fitting ----
  callModule(module = fittingTable, id = "fitting", reactive(input$button_fit))
  callModule(module = fittingResults, id = "fitting", reactive(input$button_fit))

  # Advanced Fitting ----
  callModule(module = fittingAdvHotTable, id = "adv_fitting")
  # callModule(module = fittingAdvTable, id = "adv_fitting")
  callModule(module = fittingAdvResults, id = "adv_fitting")
}


