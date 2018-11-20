# Libraries ------------------------------------------------

# Server Libs
library(rhandsontable)
library(shinyjs)
library(dplyr)

# Server ---------------------------------------------------

server <- function(input, output) {

  # Fitting ####
  callModule(module = fittingTable, id = "model-b", reactive(input$button_fit))
  callModule(module = fittingResults, id = "model-b", reactive(input$button_fit))

  # Advanced Fitting ####
  callModule(module = fittingAdvTable, id = "model-c")

  # Legacy Code ####

  # Debugger
  output$debugger <- renderText({
    paste(input$plots_checkbox)
  })
}


