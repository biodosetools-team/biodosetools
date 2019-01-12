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
          tabPanel("Variance-covariance matrix", verbatimTextOutput(ns("vakoma"))),
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

    data.frame(
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

    x0 <- cell
    x1 <- cell * dose
    x2 <- cell * dose * dose
    model.data <- list(x0, x1, x2, aberr)

    result <- glm(
      aberr ~ -1 + x0 + x1 + x2,
      family = poisson(link = "identity"),
      data = model.data
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

    return(res_list)
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

  output$vakoma <- renderPrint({
    # "variance-covariance matrix 'vakoma'"
    data()[[3]]
  })

  output$corma <- renderPrint({
    # "Correlation matrix 'corma'"
    data()[[4]]
  })
}


# Advanced Fitting Modules ---------------------------------

fittingAdvUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tabItem(
    tabName = label,
    h2("Dose-effect Fitting"),
    fluidRow(

      # Data input options ----
      box(
        width = 5,
        title = "Data Input Options",
        status = "warning", solidHeader = F, collapsible = T,
        fluidRow(
          column(
            width = 12,
            # Load data from file
            checkboxInput(
              inputId = ns("load_data"),
              label = "Load data from file",
              value = FALSE
            ),
            # Inputs
            conditionalPanel(
              condition = "!input.load_data",
              ns = ns,
              numericInput(ns("num.doses"), "Number of doses", value = 11),
              numericInput(ns("num.dicentrics"), "Maximum number of dicentrics per cell", value = 5)
            ),
            conditionalPanel(
              condition = "input.load_data",
              ns = ns,
              fileInput(ns("file"), label = "File input")
            ),
            # Button
            actionButton(ns("button_upd_table"), "Generate table")
          ),
          # Tooltip
          bsTooltip(ns("button_upd_table"),
            "Note that previously introduced data will be deleted.",
            "bottom",
            options = list(container = "body")
          )
        )
      ),

      # Fitting options ----
      box(
        width = 5,
        title = "Fitting Options",
        status = "warning", solidHeader = F, collapsible = T,
        fluidRow(
          column(
            width = 12,
            # Fitting formula
            selectInput(
              ns("formula_select"),
              label = "Fitting formula",
              choices = list(
                "Y = C + αD + βD²" = "lin-quad",
                "Y = C + αD" = "lin",
                "Y = αD + βD²" = "lin-quad-no-int",
                "Y = αD" = "lin-no-int"
              ),
              selected = "lin-quad"
            ),
            # Fitting model
            selectInput(
              ns("family_select"),
              label = "Fitting model",
              choices = list("Automatic" = 0, "Poisson" = 1, "Quasipoisson" = 2),
              selected = 1
            ),
            # Use dispersion factor
            checkboxInput(
              inputId = ns("slider_disp_select"),
              label = "Use σ²/y = 1",
              value = TRUE
            ),
            # Help button
            bsButton(ns("help_fit"),
              class = "rightAlign",
              label = "",
              icon = icon("question"),
              style = "default", size = "default"
            ),
            bsModal(
              id = ns("help_fit_dialog"),
              title = "Fitting Options Help",
              trigger = ns("help_fit"),
              size = "large",
              withMathJax(includeMarkdown("help/fitting_options.md"))
            )
          )
        )
      )
    ),

    # Hot Table ----
    fluidRow(
      box(
        width = 12,
        title = "Data Input",
        status = "primary", solidHeader = F, collapsible = T, collapsed = F,
        rHandsontableOutput(ns("hotable")),
        # Button
        br(),
        downloadButton(ns("save_count_data"), "Save Data"),
        actionButton(ns("button_fit"), "Calculate fitting")
      )
    ),

    # Main tabBox ----
    fluidRow(
      tabBox(
        width = 12,
        side = "left",
        # selected = "Tab3",
        tabPanel("Result of curve fit", verbatimTextOutput(ns("result"))),
        tabPanel("Coefficients", verbatimTextOutput(ns("bstat"))),
        tabPanel("Variance-covariance matrix", verbatimTextOutput(ns("vakoma"))),
        tabPanel("Correlation matrix", verbatimTextOutput(ns("corma"))),
        # tabPanel("Plot"),
        tabPanel("Report")
      )
    ),

    # Export data and results ----
    fluidRow(
      box(
        width = 5,
        title = "Export options",
        status = "danger", solidHeader = F, collapsible = T, collapsed = T,
        # Placeholder actionButtons
        downloadButton(ns("save_fit_data"), "Save fitting data"),
        downloadButton(ns("save_report"), "Download report")
      )
    )
  )
}


fittingAdvHotTable <- function(input, output, session, stringsAsFactors) {

  # Reset table ----
  table.reset <- reactiveValues(value = 0)

  observeEvent(input$button_upd_table, {
    table.reset$value <- 1
  })

  # Initialize data frame ----
  previous <- reactive({

    # Create button dependency for updating dimensions
    input$button_upd_table

    isolate({
      num.doses <- as.numeric(input$num.doses)
      num.dicentrics <- as.numeric(input$num.dicentrics) + 1
    })

    DF.dose <- data.frame(D = rep(0.0, num.doses))
    DF.base <- data.frame(matrix(0, nrow = num.doses, ncol = num.dicentrics))

    colnames(DF.base) <- paste0("C", seq(0, num.dicentrics - 1, 1))
    # DF.calc <- data.frame(N = rep(0, num.doses), X = rep(0, num.doses))

    DF <- cbind(DF.dose, DF.base) %>%
      dplyr::mutate(D = as.numeric(D))

    return(DF)
  })

  # Reactive data frame ----
  changed.data <- reactive({
    # Create button dependency for updating dimensions
    input$button_upd_table

    if (is.null(input$hotable) || isolate(table.reset$value == 1)) {
      table.reset$value <- 0
      return(previous())
    } else if (!identical(previous(), input$hotable)) {
      mytable <- as.data.frame(hot_to_r(input$hotable))

      # Calculated columns
      mytable <- mytable %>%
        mutate(
          N = 0,
          X = 0,
          DI = 0,
          u = 0
        ) %>%
        select(D, N, X, everything())

      last.dicent.index <- ncol(mytable) - 2
      num.rows <- nrow(mytable)

      mytable <- mytable %>%
        mutate(
          D = as.numeric(D),
          X = as.integer(X),
          N = as.integer(rowSums(.[4:last.dicent.index]))
        )

      # Ugly method to calculate index of dispersion
      for (row in 1:num.rows) {
        xf <- 0
        x2f <- 0
        # Calculate summatories
        for (k in seq(0, last.dicent.index - 4, 1)) {
          xf <- xf + mytable[row, grep("C", names(mytable), value = T)][k + 1] * k
          x2f <- x2f + mytable[row, grep("C", names(mytable), value = T)][k + 1] * k^2
        }
        # Calculate variance and mean
        var <- (x2f - (xf^2) / mytable[row, "N"]) / (mytable[row, "N"] - 1)
        mean <- xf / mytable[row, "N"]
        # Save values into data frame
        mytable[row, "X"] <- as.integer(xf)
        mytable[row, "DI"] <- var / mean
        mytable[row, "u"] <- (var / mean - 1) * sqrt((mytable[row, "N"] - 1) / (2 * (1 - 1 / mytable[row, "X"])))
      }

      return(mytable)
    }
  })

  # Output ----
  output$hotable <- renderRHandsontable({
    hot <- rhandsontable(
      changed.data()
    )
    hot$x$contextMenu = list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })
}

fittingAdvTable <- function(input, output, session, stringsAsFactors) {
  table <- reactive({
    input$button_fit

    isolate({
      table.df <- hot_to_r(input$hotable)

      dose <- table.df[["D"]]
      aberr <- table.df[["X"]]
      cell <- table.df[["N"]]
    })

    data.frame(
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


fittingAdvResults <- function(input, output, session, stringsAsFactors) {

  # Calculations ----
  data <- reactive({
    input$button_fit

    isolate({
      table.df <- hot_to_r(input$hotable)

      dose <- table.df[["D"]]
      aberr <- table.df[["X"]]
      cell <- table.df[["N"]]
      disp <- table.df[["DI"]]

      model.formula <- input$formula_select
      model.family <- input$family_select
      disp.select <- input$slider_disp_select
    })

    # Construct predictors and model data
    x0 <- cell
    x1 <- cell * dose
    x2 <- cell * dose * dose
    model.data <- list(x0, x1, x2, aberr)

    # Weight selection
    if (disp.select) {
      weights <- rep(1, length(dose))
    } else {
      weights <- 1 / disp
    }

    # Select model formula
    if (model.formula == "lin-quad") {
      fit.formula <- as.formula("aberr ~ -1 + x0 + x1 + x2")
      fit.link <- "identity"
    } else if (model.formula == "lin") {
      fit.formula <- as.formula("aberr ~ -1 + x0 + x1")
      fit.link <- "identity"
    }
    else if (model.formula == "lin-quad-no-int") {
      fit.formula <- as.formula("aberr ~ -1 + x1 + x2")
      fit.link <- "identity"
    }
    else if (model.formula == "lin-no-int") {
      fit.formula <- as.formula("aberr ~ -1 + x1")
      fit.link <- "identity"
    }
    # TODO: add automatic selection

    # Select model family
    if (model.family == 1) {
      family.select <- "poisson"
    } else if (model.family == 2) {
      family.select <- "quasipoisson"
    }
    # TODO: add automatic selection

    # Calculate fit
    result <- glm(
      formula = fit.formula,
      family = eval(parse(text = paste(family.select, "(link =", fit.link, ")"))),
      weights = weights,
      data = model.data
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

    return(res_list)
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

  output$vakoma <- renderPrint({
    # "variance-covariance matrix 'vakoma'"
    data()[[3]]
  })

  output$corma <- renderPrint({
    # "Correlation matrix 'corma'"
    data()[[4]]
  })

  # Export options ----
  output$save_count_data <- downloadHandler(
    filename = function() {
      paste("count-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(hot_to_r(input$hotable), file)
    }
  )

  output$save_fit_data <- downloadHandler(
    filename = function() {
      paste("fitting-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(hot_to_r(input$hotable), file)
    }
  )
}
