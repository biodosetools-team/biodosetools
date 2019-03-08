# Fitting Modules ------------------------------------------

fittingUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  bs4TabItem(
    tabName = label,
    h2("Dose-effect Fitting"),
    fluidRow(
      # Input box ----
      bs4Card(
        width = 5,
        title = "Inputs",
        status = "inputs", solidHeader = T, collapsible = T, closable = F,
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
        actionButton(ns("button_fit"), class = "inputs-button", "Calculate fit")
      ),

      # Results tabBox ----
      # tabBox: Fit results ----
      bs4TabCard(
        width = 7,
        side = "left",
        # tabPanel(
        bs4TabPanel(
          # title = "Result of curve fit",
          tabName = "Result of curve fit",
          active = TRUE,
          # h4("Fit summary"),
          # verbatimTextOutput(ns("fit_results")),
          h4("Fit formula"),
          verbatimTextOutput(ns("fit_formula")),
          h4("Coefficients"),
          rHandsontableOutput(ns("fit_coeffs"))
        ),
        # tabPanel(
        bs4TabPanel(
          # title = "Summary statistics",
          tabName = "Summary statistics",
          h4("Model-level statistics"),
          rHandsontableOutput(ns("fit_statistics")),
          h4("Correlation matrix"),
          rHandsontableOutput(ns("cor_mat")),
          h4("Variance-covariance matrix"),
          rHandsontableOutput(ns("var_cov_mat"))
        )
      ),

      # Data box ----
      bs4Card(
        width = 5,
        title = "Data",
        status = "results", solidHeader = T, collapsible = T, closable = F, collapsed = T,
        rHandsontableOutput(outputId = ns("table"))
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

    data <- data.frame(
      D = dose,
      N = as.integer(cell),
      X = as.integer(aberr)
    )

    data <- rhandsontable(data)
  })

  # Output ----
  output$table <- renderRHandsontable({
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
    fit_coeffs <- fit_summary$coefficients
    var_cov_mat <- vcov(fit_results)

    results_list <- list(
      fit_results = fit_results,
      fit_coeffs = fit_coeffs,
      var_cov_mat = var_cov_mat,
      cor_mat = cor_mat
    )

    return(results_list)
  })

  # Results outputs ----
  output$fit_results <- renderPrint({
    # Result of curve fit 'fit_results'
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_results"]]
  })

  output$fit_formula <- renderPrint({
    # Fitting formula
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_results"]]$formula
    # TODO: transform this into LaTeX output?
  })

  output$fit_statistics <- renderRHandsontable({
    # Model-level statistics using broom::glance
    if (input$button_fit <= 0) return(NULL)
    broom::glance(data()[["fit_results"]]) %>%
      select(logLik, df.null, df.residual, null.deviance, deviance, AIC, BIC) %>%
      mutate(null.deviance = as.character(null.deviance)) %>%
      rhandsontable()
  })

  output$fit_coeffs <- renderRHandsontable({
    # Coefficients 'fit_coeffs'
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_coeffs"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 75) %>%
      hot_cols(format = "0.000")
  })

  output$var_cov_mat <- renderRHandsontable({
    # Variance-covariance matrix 'var_cov_mat'
    if (input$button_fit <= 0) return(NULL)
    data()[["var_cov_mat"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.0000000")
  })

  output$cor_mat <- renderRHandsontable({
    # Correlation matrix 'cor_mat'
    if (input$button_fit <= 0) return(NULL)
    data()[["cor_mat"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })
}
