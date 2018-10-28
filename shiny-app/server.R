# Libraries ------------------------------------------------

# Server Libs
library(shinyjs)

# Server ---------------------------------------------------

server <- function(input, output) {

  # Fitting A ####
  input.vals <- reactiveValues()

  observe({
    input.vals$dose <- as.numeric(unlist(strsplit(input$dose, ",")))
    input.vals$aberr <- as.numeric(unlist(strsplit(input$aberr, ",")))
    input.vals$cells <- as.numeric(unlist(strsplit(input$cells, ",")))
  })

  output$table <- renderTable({
    data.frame(
      Dose = input.vals$dose,
      Aberrations = input.vals$aberr,
      Cells = input.vals$cells
      )
  })

  # Calculations ####
  output.vals <- reactiveValues()

  calculations <- observe({
    dose <- input.vals$dose
    aberr <- input.vals$aberr
    cells <- input.vals$cells

    x0 <- cells
    x1 <- cells * dose
    x2 <- cells * dose * dose
    modelldaten <- list(x0, x1, x2, aberr)

    # result <- glm(aberr ~  -1 + x1 + x2,
    #               family = poisson(link = "identity"),
    #               data = modelldaten
    # )

    result <- glm(aberr ~ -1 + x0 + x1 + x2,
                  family = poisson(link = "identity"),
                  data = modelldaten
    )

    smry <- summary(result, correlation = TRUE)
    smry$coefficients
    smry$correlation

    corma <- smry$correlation
    bstat <- smry$coefficients
    seb <- bstat[, 3]
    vakoma <- corma * outer(seb, seb)
    vakoma <- vcov(result)

    output.vals$result <- result
    output.vals$bstat <- bstat
    output.vals$vakoma <- vakoma
    output.vals$corma <- corma
  })

  # Output prints ####
  output$result <- renderPrint({
    # "Result of curve fit 'result'"
    print(output.vals$result)
  })

  output$bstat <- renderPrint({
    # "Coefficients 'bstat'"
    print(output.vals$bstat)
  })

  output$vakoma <- renderPrint({

    # "variance-covariance matrix 'vakoma'"
    print(output.vals$vakoma)
  })

  output$corma <- renderPrint({
    # "Correlation matrix 'corma'"
    print(output.vals$corma)
  })


  # Modular B ####
  # callModule(module = fittingTable, id = "model-b", reactive(input$button_fit))

  fittingServer <- callModule(module = fittingTable, id = "model-b", reactive(input$button_fit))

  output$table_b <- renderTable({
    fittingServer()
  })

  fittingServerB <- callModule(fittingResults, "model-b", reactive(input$button_fit))

  output$testtt <- renderPrint({
    fittingServerB()
  })





  # Test Code ####
  output$dose_num_value <- renderPrint({ input$dose_num })

  # Legacy Code ####

  # Debugger
  output$debugger <- renderText({
    paste(input$plots_checkbox)
  })
}


