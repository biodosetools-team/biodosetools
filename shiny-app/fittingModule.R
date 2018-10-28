# Fitting B ####
fittingTabUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  box(width = 12,
      title = "Inputs",
      status = "primary", solidHeader = F, collapsible = T,
      # Inputs
      textInput(inputId = ns("dose"),
                label = "Dose", value = "1"),
      # value = "0,0.1,0.25,0.5,0.75,1,1.5,2,3,4,5"),
      textInput(inputId = ns("aberr"),
                label = "Aberrations", value = "1"),
      # value = "8,14,22,55,100,109,100,103,108,103,107"),
      textInput(inputId = ns("cells"),
                label = "Cells", value = "1"),
      # value = "5000,5002,2008,2002,1832,1168,562,332,193,103,59"),

      # Tooltips
      bsTooltip(ns("dose"), "List of doses",
                "right", options = list(container = "body")),
      bsTooltip(ns("aberr"), "Aberrations count",
                "right", options = list(container = "body")),
      bsTooltip(ns("cells"), "Cells count",
                "right", options = list(container = "body")),

      # Button
      actionButton(ns("button_fit"), "Calculate")
  )

  # tabItem(tabName = label,
  #         h2("Dose-effect Fitting"),
  #         fluidRow(
  #           # Sidebar with input and data
  #           column(width = 4,
  #                  box(width = 12,
  #                      title = "Inputs",
  #                      status = "primary", solidHeader = F, collapsible = T,
  #                      # Inputs
  #                      textInput(inputId = ns("dose"),
  #                                label = "Dose"),
  #                      # value = "0,0.1,0.25,0.5,0.75,1,1.5,2,3,4,5"),
  #                      textInput(inputId = ns("aberr"),
  #                                label = "Aberrations"),
  #                      # value = "8,14,22,55,100,109,100,103,108,103,107"),
  #                      textInput(inputId = ns("cells"),
  #                                label = "Cells"),
  #                      # value = "5000,5002,2008,2002,1832,1168,562,332,193,103,59"),
  #
  #                      # Tooltips
  #                      bsTooltip(ns("dose"), "List of doses",
  #                                "right", options = list(container = "body")),
  #                      bsTooltip(ns("aberr"), "Aberrations count",
  #                                "right", options = list(container = "body")),
  #                      bsTooltip(ns("cells"), "Cells count",
  #                                "right", options = list(container = "body")),
  #
  #                      # Button
  #                      actionButton(ns("button_fit"), "Calculate")
  #                  ),
  #                  box(width = 12,
  #                      title = "Data",
  #                      status = "primary", solidHeader = F, collapsible = T, collapsed = F,
  #                      tableOutput('table_b')
  #                  )
  #           )
  #           # ,
  #           # # Main tabBox
  #           # column(width = 8,
  #           #        tabBox(width = 12,
  #           #               side = "left",
  #           #               # height = "500px",
  #           #               # selected = "Tab3",
  #           #               tabPanel("Result of curve fit", verbatimTextOutput("result_b")),
  #           #               tabPanel("Coefficients", verbatimTextOutput("bstat_b")),
  #           #               tabPanel("Variance-covariance matrix", verbatimTextOutput("vakoma_b")),
  #           #               tabPanel("Correlation matrix", verbatimTextOutput("corma_b"))
  #           #        )
  #           # )
  #
  #         )
  # )
}


fittingTable <- function(input, output, session, stringsAsFactors) {

  table <- reactive({

    input$button_fit

    isolate({
      dose = as.numeric(unlist(strsplit(input$dose, ",")))
      aberr = as.numeric(unlist(strsplit(input$aberr, ",")))
      cell = as.numeric(unlist(strsplit(input$cells, ",")))
    })

    data.frame(
      Dose = dose,
      Aberrations = aberr,
      Cells = cell
    )
  })

  return(table)
}


fittingResults <- function(input, output, session, stringsAsFactors) {

  # Calculations ####
  daten <- reactive({

    input$button_fit

    isolate({
      dose = as.numeric(unlist(strsplit(input$dose, ",")))
      aberr = as.numeric(unlist(strsplit(input$aberr, ",")))
      cell = as.numeric(unlist(strsplit(input$cells, ",")))
    })

    x0 <- cell
    x1 <- cell * dose
    x2 <- cell * dose * dose
    modelldaten <- list(x0, x1, x2, aberr)

  # print(x2)
  # return(x2)

    # result <- glm(
    #   aberr ~  -1 + x1 + x2,
    #   family = poisson(link = "identity"),
    #   data = modelldaten
    # )

    result <- glm(
      aberr ~ -1 + x0 + x1 + x2,
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

    res_list <- list(result, bstat, vakoma, corma)

    # ns("lost") <- list(result, bstat, vakoma, corma)

    # output.vals.b$result <- result
    # output.vals.b$bstat <- bstat
    # output.vals.b$vakoma <- vakoma
    # output.vals.b$corma <- corma
  # })

    # # Output prints ####
    # output$result_b <- renderPrint({
    #   # "Result of curve fit 'result'"
    #   print(output.vals.b$result)
    # })
    #
    # output$bstat_b <- renderPrint({
    #   # "Coefficients 'bstat'"
    #   print(output.vals.b$bstat)
    # })
    #
    # output$vakoma_b <- renderPrint({
    #
    #   # "variance-covariance matrix 'vakoma'"
    #   print(output.vals.b$vakoma)
    # })
    #
    # output$corma_b <- renderPrint({
    #   # "Correlation matrix 'corma'"
    #   print(output.vals.b$corma)
    # })

    return(res_list)
    # return(modelldaten)
  })

  # # Output prints ####
  # output$result_b <- renderPrint({
  #   # "Result of curve fit 'result'"
  #   print(output.vals.b$result)
  # })
  #
  # output$bstat_b <- renderPrint({
  #   # "Coefficients 'bstat'"
  #   print(output.vals.b$bstat)
  # })
  #
  # output$vakoma_b <- renderPrint({
  #
  #   # "variance-covariance matrix 'vakoma'"
  #   print(output.vals.b$vakoma)
  # })
  #
  # output$corma_b <- renderPrint({
  #   # "Correlation matrix 'corma'"
  #   print(output.vals.b$corma)
  # })

}
