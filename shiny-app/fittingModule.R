# Fitting Modules ------------------------------------------

fittingUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabItem(
    tabName = label,
    h2("Dose-effect Fitting"),
    fluidRow(
      column(
        width = 4,
        # Input box ----
        box(
          width = 12,
          title = "Inputs",
          status = "primary", solidHeader = F, collapsible = T,
          # Inputs
          textInput(
            inputId = ns("dose"),
            label = "Dose",
            value = "0,0.1,0.25,0.5,0.75,1,1.5,2,3,4,5"
          ),
          textInput(
            inputId = ns("aberr"),
            label = "Aberrations",
            value = "8,14,22,55,100,109,100,103,108,103,107"
          ),
          textInput(
            inputId = ns("cells"),
            label = "Cells",
            value = "5000,5002,2008,2002,1832,1168,562,332,193,103,59"
          ),

          # Tooltips
          bsTooltip(ns("dose"), "List of doses",
            "right",
            options = list(container = "body")
          ),
          bsTooltip(ns("aberr"), "Aberrations count",
            "right",
            options = list(container = "body")
          ),
          bsTooltip(ns("cells"), "Cells count",
            "right",
            options = list(container = "body")
          ),

          # Button
          actionButton(ns("button_fit"), "Calculate")
        ),
        # Data box ----
        box(
          width = 12,
          title = "Data",
          status = "success", solidHeader = F, collapsible = T, collapsed = T,
          tableOutput(outputId = ns("table"))
        )
      ),

      # Results tabBox ----
      column(
        width = 8,
        tabBox(
          width = 12,
          side = "left",
          # selected = "Tab3",
          tabPanel("Result of curve fit", verbatimTextOutput(ns("result"))),
          tabPanel("Coefficients", verbatimTextOutput(ns("bstat"))),
          tabPanel("Variance-covariance matrix", verbatimTextOutput(ns("var_cov_mat"))),
          tabPanel("Correlation matrix", verbatimTextOutput(ns("corma")))
        )
      )
    )
  )
}

fittingTable <- function(input, output, session, stringsAsFactors) {
  table <- reactive({
    input$button_fit

    isolate({
      dose <- as.numeric(unlist(strsplit(input$dose, ",")))
      aberr <- as.numeric(unlist(strsplit(input$aberr, ",")))
      cell <- as.numeric(unlist(strsplit(input$cells, ",")))
    })

    data_frame(
      Dose = dose,
      Aberrations = aberr,
      Cells = cell
    )
  })

  # Output ----
  output$table <- renderTable({
    table()
  })
}


fittingResults <- function(input, output, session, stringsAsFactors) {

  # Calculations ----
  data <- reactive({
    input$button_fit

    isolate({
      dose <- as.numeric(unlist(strsplit(input$dose, ",")))
      aberr <- as.numeric(unlist(strsplit(input$aberr, ",")))
      cell <- as.numeric(unlist(strsplit(input$cells, ",")))
    })

    # Construct predictors and model data
    x0 <- cell
    x1 <- cell * dose
    x2 <- cell * dose * dose
    model_data <- list(x0, x1, x2, aberr)

    # Calculate fit
    fit_results <- glm(
      aberr ~ -1 + x0 + x1 + x2,
      family = poisson(link = "identity"),
      data = model_data
    )

    # Summarise fit
    fit_summary <- summary(fit_results, correlation = TRUE)
    cor_mat <- fit_summary$correlation
    bstat <- fit_summary$coefficients
    var_cov_mat <- vcov(fit_results)

    results_list <- list(result, bstat, var_cov_mat, corma)

    return(results_list)
  })

  # Outputs ----
  output$result <- renderPrint({
    # "Result of curve fit 'result'"
    data()[[1]]
  })

  output$bstat <- renderPrint({
    # "Coefficients 'bstat'"
    data()[[2]]
  })

  output$var_cov_mat <- renderPrint({
    # "variance-covariance matrix 'var_cov_mat'"
    data()[[3]]
  })

  output$corma <- renderPrint({
    # "Correlation matrix 'corma'"
    data()[[4]]
  })
}
