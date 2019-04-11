# Dose Estimateion Modules ------------------------------------------

dicCalcMethodUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    # Assessment selection
    div(
      class = "side-widget-tall",
      uiOutput(ns("assessment_dynamic"))
    ),
    div(class = "widget-sep", br()),

    # Curve method selection
    div(
      class = "side-widget-tall",
      uiOutput(ns("method_dynamic"))
    )
  )
}

dicCalcMethod <- function(input, output, session) {
  # Dynamic UI

  output$assessment_dynamic <- renderUI({
    selectInput(
      session$ns("assessment_select"),
      label = "Assessment",
      width = "175px",
      choices = list(
        "Whole body"    = "whole-body",
        "Partial"       = "partial",
        "Heterogeneous" = "hetero"
      ),
      selected = "whole-body"
    )
  })

  output$method_dynamic <- renderUI({
    selectInput(
      session$ns("curve_method_select"),
      label = "Error calculation",
      width = "250px",
      choices = list(
        "Merkle's method (83%-83%)" = "merkle-83",
        "Merkle's method (95%-95%)" = "merkle-95",
        "Simple method"             = "simple"
      ),
      selected = "merkle-83"
    )
  })
}


estimateUI <- function(id, label) { #, locale = i18n) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  bs4TabItem(
    tabName = label,
    h2("Dose estimation"),
    # h2(locale$t("Hello Shiny!")),

    fluidRow(
      # Card: Curve fitting options ----
      bs4MyCard(
        width = 5,
        title = "Curve fitting data options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
        fluidRow(
          column(
            width = 12,
            # Load data from file
            awesomeCheckbox(
              inputId = ns("load_fit_data_check"),
              label = "Load fit data from file",
              value = TRUE, status = "warning"
            ),
            # Manual input ----
            conditionalPanel(
              condition = "!input.load_fit_data_check",
              ns = ns,
              p("This part has not been implemented yet.")
              # TODO: add fit data input widgets
            ),
            # Load from file ----
            conditionalPanel(
              condition = "input.load_fit_data_check",
              ns = ns,
              fileInput(ns("load_fit_data"), label = "File input")
            ),
            # Buttons
            actionButton(ns("button_view_fit_data"), class = "options-button", "Preview data")
          ),
          # Tooltip
          bsTooltip(ns("button_upd_table"),
                    "Note that previously introduced data will be deleted.",
                    "bottom",
                    options = list(container = "body")
          )
        ),

        # Help button
        topButton =
          bsButton(
            ns("help_fit_data"),
            label = "",
            icon = icon("question"),
            style = "default", size = "default"
          ),

        # Help modal
        bs4MyModal(
          id = ns("help_fit_data_dialog"),
          title = "Help: Fitting data input",
          trigger = ns("help_fit_data"),
          size = "large",

          # Option selection
          radioGroupButtons(
            inputId = ns("help_fit_data_option"),
            label = NULL,
            choices = c(
              "Manual input" = "manual",
              "Load data"    = "load"
            )
          ),
          # Contents
          conditionalPanel(
            condition = "input.help_fit_data_option == 'manual'",
            ns = ns,
            withMathJax(includeMarkdown("help/help_fit_data_input.md"))
          ),
          conditionalPanel(
            condition = "input.help_fit_data_option == 'load'",
            ns = ns,
            withMathJax(includeMarkdown("help/help_fit_data_load.md"))
          )
        )
      ),
      # tabBox: Curve fitting overview ----
      # tabBox(
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
      )
    ),

    fluidRow(
      # Card: Data input options ----
      bs4MyCard(
        width = 5,
        title = "Data input options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
        fluidRow(
          column(
            width = 12,
            # Load data from file
            awesomeCheckbox(
              inputId = ns("load_cases_data_check"),
              label = "Load data from file",
              value = FALSE, status = "warning"
            ),
            # Inputs
            conditionalPanel(
              condition = "!input.load_cases_data_check",
              ns = ns,
              numericInput(ns("num_cases"), "Number of cases", value = 1),
              numericInput(ns("num_dicentrics"), "Maximum number of dicentrics per cell", value = 5)
            ),
            conditionalPanel(
              condition = "input.load_cases_data_check",
              ns = ns,
              fileInput(ns("load_cases_data"), label = "File input")
            ),
            # Case description
            textAreaInput(
              inputId = ns("case_description"),
              label = "Case description",
              placeholder = "Short summary of the case"),
            # Buttons
            actionButton(ns("button_upd_table"), class = "options-button", "Generate table")
          ),
          # Tooltip
          bsTooltip(ns("button_upd_table"),
                    "Note that previously introduced data will be deleted.",
                    "bottom",
                    options = list(container = "body")
          )
        ),

        # Help button
        topButton =
          bsButton(
            ns("help_cases_data"),
            label = "",
            icon = icon("question"),
            style = "default", size = "default"
          ),

        # Help modal
        bs4MyModal(
          id = ns("help_cases_data_dialog"),
          title = "Help: Cases data input",
          trigger = ns("help_cases_data"),
          size = "large",

          # Option selection
          radioGroupButtons(
            inputId = ns("help_cases_data_option"),
            label = NULL,
            choices = c(
              "Manual input" = "manual",
              "Load data"    = "load"
            )
          ),
          # Contents
          conditionalPanel(
            condition = "input.help_cases_data_option == 'manual'",
            ns = ns,
            withMathJax(includeMarkdown("help/help_cases_data_input.md"))
          ),
          conditionalPanel(
            condition = "input.help_cases_data_option == 'load'",
            ns = ns,
            withMathJax(includeMarkdown("help/help_cases_data_load.md"))
          )
        )
      ),
      # Card: hot Cases input ----
      column(
        width = 7,
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Data input",
          status = "inputs", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
          rHandsontableOutput(ns("hotable"))
        ),

        # Card: Estimation options ----
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Dose estimation options",
          status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

          # Type of exposure selection
          div(
            class = "side-widget-tall",
            selectInput(
              ns("exposure_select"),
              label = "Exposure",
              width = "175px",
              choices = list(
                "Acute"      = "acute",
                "Protracted" = "protracted"
              ),
              selected = "acute"
            )
          ),
          div(class = "widget-sep", br()),

          # Assessment selection
          div(
            class = "side-widget-tall",
            selectInput(
              ns("assessment_select"),
              label = "Assessment",
              width = "175px",
              choices = list(
                "Whole body"    = "whole-body",
                "Partial"       = "partial",
                "Heterogeneous" = "hetero"
              ),
              selected = "whole-body"
            )
          ),
          div(class = "widget-sep", br()),

          dicCalcMethodUI(ns("inner")),

          # Curve method selection
          # div(
          #   class = "side-widget-tall",
          #   # selectInput(
          #   #   ns("curve_method_select"),
          #   #   label = "Error calculation",
          #   #   width = "250px",
          #   #   choices = list(
          #   #     "Merkle's method (83%-83%)" = "merkle-83",
          #   #     "Merkle's method (95%-95%)" = "merkle-95",
          #   #     "Simple method"             = "simple"
          #   #   ),
          #   #   selected = "merkle-83"
          #   # )
          #   # uiOutput(ns("method_dynamic"))
          #   dicCalcMethodUI(ns("inner"))
          # ),

          # Protracted timing
          conditionalPanel(
            condition = "input.exposure_select == 'protracted'",
            ns = ns,

            br(),

            # Irradiation time
            div(
              class = "side-widget-tall",
              numericInput(
                ns("protracted_time"),
                label = "Irradiation time (h)",
                width = "175px",
                value = 0.5,
                step = 0.1,
                min = 0
              )
            ),
            div(class = "widget-sep", br()),

            # Rejoining time
            div(
              class = "side-widget-tall",
              numericInput(
                ns("protracted_life_time"),
                label = "Rejoining time (h)",
                width = "175px",
                value = 2,
                step = 0.1,
                min = 2,
                max = 5
              )
            )
          ),

          # Coefficient conditional input
          conditionalPanel(
            condition = "input.assessment_select != 'whole-body'",
            ns = ns,

            br(),

            # Coefficient input selection
            div(
              class = "side-widget-tall",
              selectInput(
                ns("fraction_coeff_select"),
                label = "Survival coefficient",
                width = "175px",
                choices = list(
                  "D0" = "d0",
                  "Gamma" = "gamma"
                ),
                selected = "d0"
              )
            ),

            div(class = "widget-sep", br()),

            div(
              class = "side-widget",
              # Input gamma
              conditionalPanel(
                condition = "input.fraction_coeff_select == 'gamma'",
                ns = ns,
                div(
                  class = "side-widget-tall",
                  numericInput(
                    width = "175px",
                    ns("gamma_coeff"), "Gamma",
                    value = 0.3706479, step = 0.01
                  )
                ),
                div(
                  class = "side-widget-tall",
                  numericInput(
                    width = "150px",
                    ns("gamma_error"), "Error of gamma",
                    value = 0.009164707, step = 0.0001
                  )
                )
              ),
              # Input D0
              conditionalPanel(
                condition = "input.fraction_coeff_select == 'd0'",
                ns = ns,
                div(
                  class = "side-widget-tall",
                  numericInput(
                    width = "150px",
                    ns("d0_coeff"), "D0",
                    value = 2.7, step = 0.01,
                    min = 2.7, max = 3.5
                  )
                )
              )
            )
          ),

          conditionalPanel(
            condition = "input.assessment_select == 'whole-body'",
            ns = ns,
            br()
          ),

          conditionalPanel(
            condition = "input.assessment_select != 'whole-body'",
            ns = ns,
            br()
          ),

          actionButton(ns("button_estimate"), class = "options-button", "Estimate dose"),

          # Help button
          topButton =
            bsButton(
              ns("help_estimate_options"),
              label = "",
              icon = icon("question"),
              style = "default", size = "default"
            ),

          # Help modal
          bs4MyModal(
            id = ns("help_estimate_options_dialog"),
            title = "Help: Dose estimation options",
            trigger = ns("help_estimate_options"),
            size = "large",

            # Option selection
            radioGroupButtons(
              inputId = ns("help_estimate_options_option"),
              label = NULL,
              choices = c(
                "Assessment"           = "assess",
                "Error calculation"    = "error",
                "Survival coefficient" = "surv_coeff"
              )

            ),
            # Contents
            conditionalPanel(
              condition = "input.help_estimate_options_option == 'assess'",
              ns = ns,
              withMathJax(includeMarkdown("help/help_dose_assessment.md"))
            ),
            conditionalPanel(
              condition = "input.help_estimate_options_option == 'error'",
              ns = ns,
              withMathJax(includeMarkdown("help/help_dose_curve_method.md"))
            ),
            conditionalPanel(
              condition = "input.help_estimate_options_option == 'surv_coeff'",
              ns = ns,
              withMathJax(includeMarkdown("help/help_fraction_coeff_select.md"))
            )
          )

        )
      )
    ),

    fluidRow(
      # Card: Estimation results ----
      column(
        width = 6,
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Results",
          status = "results", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

          h4("Whole-body estimated dose"),
          rHandsontableOutput(ns("est_doses_whole")),

          conditionalPanel(
            condition = "input.assessment_select == 'partial'",
            ns = ns,
            h4("Dose recieved by the irradiated fraction"),
            rHandsontableOutput(ns("est_doses_partial")),
            h4("Initial fraction of irradiated cells"),
            rHandsontableOutput(ns("est_frac_partial"))
          ),

          conditionalPanel(
            condition = "input.assessment_select == 'hetero'",
            ns = ns,
            h4("Observed fraction of irradiated cells and its yield"),
            rHandsontableOutput(ns("est_mixing_prop_hetero")),
            h4("Dose recieved by the irradiated fraction"),
            div(
              class = "side-widget",
              rHandsontableOutput(ns("est_doses_hetero"))
            ),
            bsButton(
              ns("help_dose_mixed_yields"),
              # class = "rightAlign",
              label = "",
              icon = icon("question"),
              style = "default", size = "default"
            ),
            bsModal(
              id = ns("help_dose_mixed_yields_dialog"),
              title = "Help: Partial and heterogeneous exposures",
              trigger = ns("help_dose_mixed_yields"),
              size = "large",
              withMathJax(includeMarkdown("help/help_dose_mixed_yields.md"))
            ),
            h4("Initial fraction of irradiated cells"),
            rHandsontableOutput(ns("est_frac_hetero"))
          )
        ),

        # Card: Export data and results ----
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Save results",
          status = "export", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
          # Case description
          textAreaInput(
            inputId = ns("results_comments"),
            label = "Comments",
            placeholder = "Comments to be included on report"),

          # Download fit data & report
          # downloadButton(ns("save_fit_data"), class = "side-widget", "Save fitting data"),
          # div(
          #   class = "side-widget-tall",
          #   selectInput(
          #     ns("save_fit_data_format"),
          #     label = NULL,
          #     width = "85px",
          #     choices = list(".rds"),
          #     selected = ".rds"
          #   )
          # ),
          # Download report
          # div(class = "widget-sep", br()),
          downloadButton(ns("save_report"), class = "export-button", "Download report"),

          # Help button
          topButton =
            bsButton(
              ns("help_fit_data_save"),
              label = "",
              icon = icon("question"),
              style = "default", size = "default"
            ),

          # Help Modal
          bs4MyModal(
            id = ns("help_fit_data_save_dialog"),
            title = "Help: Export results",
            trigger = ns("help_fit_data_save"),
            size = "large",

            # Option selection
            # radioGroupButtons(
            #   inputId = ns("help_fit_data_save_option"),
            #   label = NULL,
            #   choices = c(
            #     "Fitting results" = "data",
            #     "Report"          = "report"
            #   )
            # ),
            # Contents
            # conditionalPanel(
            #   condition = "input.help_fit_data_save_option == 'data'",
            #   ns = ns,
            #   withMathJax(includeMarkdown("help/help_fit_data_save.md"))
            # ),
            # conditionalPanel(
              # condition = "input.help_fit_data_save_option == 'report'",
              # ns = ns,
              withMathJax(includeMarkdown("help/help_fit_data_save_report.md"))
            # )
          )

        )
      ),
      # Card: Plot curves ----
      column(
        width = 6,
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Curve plot",
          status = "results", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
          # Plot
          plotOutput(ns("plot")),
          # Download plot
          downloadButton(ns("save_plot"), class = "results-button side-widget", "Save plot"),
          div(
            class = "side-widget-tall",
            selectInput(
              ns("save_plot_format"),
              label = NULL,
              width = "85px",
              choices = list(".png", ".pdf"),
              selected = ".png"
            )
          )
        )
      )
    )
  )
}


estimateHotTable <- function(input, output, session, stringsAsFactors) {

  # Reset table ----
  table_reset <- reactiveValues(value = 0)

  observeEvent(input$button_upd_table, {
    table_reset$value <- 1
  })

  # Initialize data frame ----
  previous <- reactive({

    # Create button dependency for updating dimensions
    input$button_upd_table

    isolate({
      load_cases_data <- input$load_cases_data_check
      cases_data <- input$load_cases_data
      num_cases <- as.numeric(input$num_cases)
      num_dicentrics <- as.numeric(input$num_dicentrics) + 1
    })

    if (!load_cases_data) {
      # Base data frame
      full_data <- data.frame(
        matrix(
          0,
          nrow = num_cases,
          ncol = num_dicentrics
        )
      )

      colnames(full_data) <- paste0("C", seq(0, num_dicentrics - 1, 1))
    } else {
      full_data <- read.csv(cases_data$datapath, header = TRUE) %>%
        mutate_at(vars(starts_with("C")), funs(as.integer(.)))
    }

    return(full_data)
  })

  # Reactive data frame ----
  changed_data <- reactive({
    # Create button dependency for updating dimensions
    input$button_upd_table

    if (is.null(input$hotable) || isolate(table_reset$value == 1)) {
      table_reset$value <- 0
      return(previous())
    } else if (!identical(previous(), input$hotable)) {
      mytable <- as.data.frame(hot_to_r(input$hotable))

      # Calculated columns
      mytable <- mytable %>%
        mutate(
          N = 0,
          X = 0,
          y = 0,
          y_err = 0,
          DI = 0,
          u = 0
        ) %>%
        # select(D, N, X, everything())
        select(N, X, everything())

      first_dicent_index <- 3
      last_dicent_index <- ncol(mytable) - 4
      num_rows <- nrow(mytable)


      mytable <- mytable %>%
        mutate(
          # D = as.numeric(D),
          X = as.integer(X),
          N = as.integer(rowSums(.[first_dicent_index:last_dicent_index]))
        )

      # Ugly method to calculate index of dispersion
      for (row in 1:num_rows) {
        xf <- 0
        x2f <- 0
        # Calculate summatories
        for (k in seq(0, last_dicent_index - first_dicent_index, 1)) {
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
        mytable[row, "y"] <- mytable[row, "X"] / mytable[row, "N"]
        mytable[row, "y_err"] <- sqrt(var / mytable[row, "N"])
      }

      return(mytable)
    }
  })

  # Output ----
  output$hotable <- renderRHandsontable({
    # Read number of columns
    num_cols <- ncol(changed_data())

    #
    hot <- changed_data() %>%
      rhandsontable() %>%
      hot_cols(colWidths = 50) %>%
      hot_col(c(1, 2, seq(num_cols - 3, num_cols, 1)), readOnly = TRUE) %>%
      hot_col(ncol(changed_data()), renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value > 1.96) {
              td.style.background = 'pink';
             }
           }")
    # hot_table(highlightCol = TRUE, highlightRow = TRUE)

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })
}


estimateFittingCurve <- function(input, output, session, stringsAsFactors) {

  # Calculations ----
  data <- reactive({
    input$button_view_fit_data

    isolate({
      load_fit_data <- input$load_fit_data_check
      fit_data <- input$load_fit_data
    })

    if (load_fit_data) {
      fit_results <- readRDS(fit_data$datapath)
    }

    # Summarise fit
    fit_summary <- summary(fit_results, correlation = TRUE)
    cor_mat <- fit_summary$correlation
    fit_coeffs <- fit_summary$coefficients
    var_cov_mat <- vcov(fit_results)

    # Generalized variance-covariance matrix
    general_fit_coeffs <- numeric(length = 3L)
    names(general_fit_coeffs) <- c("x0", "x1", "x2")

    for (var in rownames(fit_coeffs)) {
      general_fit_coeffs[var] <- fit_coeffs[var, "Estimate"]
    }

    # Generalized fit coefficients
    general_var_cov_mat <- matrix(0, nrow = 3, ncol = 3)
    rownames(general_var_cov_mat) <- c("x0", "x1", "x2")
    colnames(general_var_cov_mat) <- c("x0", "x1", "x2")

    for (x_var in rownames(var_cov_mat)) {
      for (y_var in colnames(var_cov_mat)) {
        general_var_cov_mat[x_var, y_var] <- var_cov_mat[x_var, y_var]
      }
    }

    # Make list of results to return
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
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_results"]]
  })

  output$fit_formula <- renderPrint({
    # Fitting formula
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_results"]]$formula
    # TODO: transform this into LaTeX output?
  })

  output$fit_statistics <- renderRHandsontable({
    # Model-level statistics using broom::glance
    if (input$button_view_fit_data <= 0) return(NULL)
    broom::glance(data()[["fit_results"]]) %>%
      select(logLik, df.null, df.residual, null.deviance, deviance, AIC, BIC) %>%
      mutate(null.deviance = as.character(null.deviance)) %>%
      rhandsontable()
  })

  output$fit_coeffs <- renderRHandsontable({
    # Coefficients 'fit_coeffs'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_coeffs"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 75) %>%
      hot_cols(format = "0.000")
  })

  output$var_cov_mat <- renderRHandsontable({
    # Variance-covariance matrix 'var_cov_mat'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["var_cov_mat"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.0000000")
  })

  output$cor_mat <- renderRHandsontable({
    # Correlation matrix 'cor_mat'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["cor_mat"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })
}


estimateResults <- function(input, output, session, stringsAsFactors) {
  callModule(dicCalcMethod, "inner")

  data <- reactive({
    # Calcs: get variables ----
    input$button_estimate

    isolate({
      # Fit data
      load_fit_data <- input$load_fit_data_check
      fit_data <- input$load_fit_data
      exposure <- input$exposure_select
      assessment <- input$assessment_select
      curve_method <- input$`inner-curve_method_select`

      # Cases data
      cases_data <- hot_to_r(input$hotable)

      aberr <- cases_data[["X"]]
      cell <- cases_data[["N"]]
      yield_obs <- cases_data[["y"]]

      counts <- cases_data[1, ] %>%
        select(contains("C")) %>%
        as.numeric()

      # Coefficient input selection
      fraction_coeff <- input$fraction_coeff_select
    })

    # Get fitting data ----
    if (load_fit_data) {
      fit_results <- readRDS(fit_data$datapath)
    }

    # Summarise fit
    fit_summary <- summary(fit_results, correlation = TRUE)
    # cor_mat <- fit_summary$correlation
    fit_coeffs <- fit_summary$coefficients
    var_cov_mat <- vcov(fit_results)

    # Generalized variance-covariance matrix
    general_fit_coeffs <- numeric(length = 3L)
    names(general_fit_coeffs) <- c("x0", "x1", "x2")

    for (var in rownames(fit_coeffs)) {
      general_fit_coeffs[var] <- fit_coeffs[var, "Estimate"]
    }

    # Generalized fit coefficients
    general_var_cov_mat <- matrix(0, nrow = 3, ncol = 3)
    rownames(general_var_cov_mat) <- c("x0", "x1", "x2")
    colnames(general_var_cov_mat) <- c("x0", "x1", "x2")

    for (x_var in rownames(var_cov_mat)) {
      for (y_var in colnames(var_cov_mat)) {
        general_var_cov_mat[x_var, y_var] <- var_cov_mat[x_var, y_var]
      }
    }

    # Protracted variables ----
    protracted_g_function <- function(time, time_0) {
      x <- time / time_0
      g <- (2 / x^2) * (x - 1 + exp(-x))
      return(g)
    }

    if (exposure == "protracted") {
      protracted_time <- input$protracted_time
      protracted_life_time <- input$protracted_life_time
      protracted_g_value <- protracted_g_function(protracted_time, protracted_life_time)
    } else {
      protracted_g_value <- 1
    }

    # Generalized curves ----
    yield_fun <- function(x) {
      general_fit_coeffs[[1]] +
        general_fit_coeffs[[2]] * x +
        general_fit_coeffs[[3]] * x * x * protracted_g_value
    }

    # R factor depeding on selected CI
    chisq_df <- nrow(fit_coeffs)
    R_factor <- function(conf_int = 0.95) {
      sqrt(qchisq(conf_int, df = chisq_df))
    }

    yield_error_fun <- function(x) {
      # TODO: How does the protracted function affect this function?
      sqrt(
        general_var_cov_mat[["x0", "x0"]] +
          general_var_cov_mat[["x1", "x1"]] * x * x +
          general_var_cov_mat[["x2", "x2"]] * x * x * x * x +
          2 * general_var_cov_mat[["x0", "x1"]] * x +
          2 * general_var_cov_mat[["x0", "x2"]] * x * x +
          2 * general_var_cov_mat[["x1", "x2"]] * x * x * x
      )
    }

    # Projection functions ----
    project_yield_base <- function(yield) {
      uniroot(function(dose) {
        yield_fun(dose) - yield
      }, c(0.1, 30))$root
    }

    project_yield_lower <- function(yield, conf_int) {
      uniroot(function(dose) {
        yield_fun(dose) + R_factor(conf_int) * yield_error_fun(dose) - yield
      }, c(0.1, 30))$root
    }

    project_yield_upper <- function(yield, conf_int) {
      uniroot(function(dose) {
        yield_fun(dose) - R_factor(conf_int) * yield_error_fun(dose) - yield
      }, c(0.1, 30))$root
    }

    # Select CI depending on selected method
    if (grepl("merkle", curve_method, fixed = TRUE)) {
      conf_int_curve <- paste0("0.", gsub("\\D", "", curve_method)) %>% as.numeric()
      conf_int_yield <- conf_int_curve
    } else if (curve_method == "simple") {
      conf_int_curve <- 0 # This makes R_factor = 0
      conf_int_yield <- 0.95
    }

    # Dose estimation functions ----

    # Calcs: whole-body estimation
    estimate_whole_body <- function(aberr, cell, y_obs, conf_int_yield, conf_int_curve) {

      # Calculate CI using Exact Poisson tests
      aberr_row <-  poisson.test(x = round(aberr, 0), conf.level = conf_int_yield)[["conf.int"]]

      aberr_l <- aberr_row[1]
      aberr_u <- aberr_row[2]

      yield_l <- aberr_l / cell
      yield_u <- aberr_u / cell

      # Calculate projections
      dose_b <- project_yield_base(yield_obs)
      dose_l <- project_yield_lower(yield_l, conf_int_curve)
      dose_u <- project_yield_upper(yield_u, conf_int_curve)

      # Whole-body estimation results
      est_doses <- data.frame(
        yield = c(yield_l, yield_obs, yield_u),
        dose  = c(dose_l, dose_b, dose_u)
      )

      row.names(est_doses) <- c("lower", "base", "upper")

      # Return
      return(est_doses)
    }

    # Calcs: partial dose estimation
    estimate_partial <- function(aberr, cell, cases_data, fraction_coeff) {

      cells_0 <- cases_data[["C0"]]

      # Yield calculation
      yield_b <- uniroot(function(yield) {
        yield / (1 - exp(-yield)) - aberr / (cell - cells_0)
      }, c(0.00001, 5))$root

      yield_b_var <-
        (yield_b * (1 - exp(-yield_b))^2) /
        ((cell - cells_0) * (1 - exp(-yield_b) - yield_b * exp(-yield_b)))

      yield_l <- yield_b - 1.96 * sqrt(yield_b_var)
      yield_u <- yield_b + 1.96 * sqrt(yield_b_var)

      # Dose estimation
      dose_b <- project_yield_base(yield_b)
      dose_l <- project_yield_lower(yield_l, 0.95)
      dose_u <- project_yield_upper(yield_u, 0.95)
      # TODO: This will become irrelevant once we use delta method

      # Partial estimation results
      est_doses <- data.frame(
        yield = c(yield_l, yield_b, yield_u),
        dose = c(dose_l, dose_b, dose_u)
      )

      # Input of the parameter gamma and its variance
      if (fraction_coeff == "gamma") {
        gamma <- input$gamma_coeff
      } else if (fraction_coeff == "d0"){
        gamma <- 1 / input$d0_coeff
      }

      # Irradiated fraction
      f <- aberr / (cell * yield_b)
      p <- exp(- dose_b * gamma)

      est_F <- (f / p) / (1 - f + f / p)

      est_frac <- data.frame(
        estimate = c(est_F)
      )

      # Return
      results_list <- list(
        est_doses = est_doses,
        est_frac  = est_frac
      )

      return(results_list)
    }

    # Calcs: heterogeneous dose estimation
    estimate_hetero <- function(counts, fit_coeffs, var_cov_mat, fraction_coeff, conf_int_yield, conf_int_curve) {
      # TODO: How does the protracted function affect this whole calculation?

      # Get cases data
      # Data test is stored in vector y
      y <- rep(seq(0, length(counts) - 1, 1), counts)
      x <- c(rep(1, length(y)))

      fit <- mixtools::poisregmixEM(y, x, addintercept = FALSE, k = 2)
      # TODO: review if k needs to be changed

      # Input of the parameters of the dose-effect linear-quadratic model
      beta0 <- fit_coeffs[1, "Estimate"]
      beta1 <- fit_coeffs[2, "Estimate"]
      beta2 <- fit_coeffs[3, "Estimate"]

      # Input of the Variance-covariance matrix of the parameters
      sigma <- numeric(49)
      dim(sigma) <- c(7, 7)
      sigma[1, 1] <- var_cov_mat[1, 1]
      sigma[2, 2] <- var_cov_mat[2, 2]
      sigma[3, 3] <- var_cov_mat[3, 3]
      sigma[1, 2] <- var_cov_mat[1, 2]
      sigma[1, 3] <- var_cov_mat[1, 3]
      sigma[2, 3] <- var_cov_mat[2, 3]
      sigma[2, 1] <- sigma[1, 2]
      sigma[3, 1] <- sigma[1, 3]
      sigma[3, 2] <- sigma[2, 3]

      # Input of the parameter gamma and its variance
      if (fraction_coeff == "gamma") {
        gamma <- input$gamma_coeff
        sigma[4, 4] <- input$gamma_error
      } else if (fraction_coeff == "d0"){
        gamma <- 1 / input$d0_coeff
        sigma[4, 4] <- 0
      }

      # First parameter is the mixing proportion
      # the second and third parameters are the yields
      # TODO: Generalize loglik and MLE to all possible fittings
      loglik <- function(b) {
        loglik <- sum(log(b[1] * dpois(y, b[2]) + (1 - b[1]) * dpois(y, b[3])))

        return(-loglik)
      }

      MLE <- optim(
        c(fit$lambda[1], exp(fit$beta)[1], exp(fit$beta)[2]),
        loglik,
        method = c("L-BFGS-B"),
        lower = c(0.01, 0.01, 0.01), upper = c(0.99, Inf, Inf), hessian = T
      )

      st <- solve(MLE$hessian)
      yield1 <- MLE$par[2]
      yield2 <- MLE$par[3]
      f1 <- MLE$par[1]

      if (yield1 < yield2) {
        yield1 <- MLE$par[3]
        yield2 <- MLE$par[2]
        f1 <- 1 - f1
        stm <- st
        stm[2, 2] <- st[3, 3]
        stm[3, 3] <- st[2, 2]
        stm[1, 2] <- st[1, 3]
        stm[1, 3] <- st[1, 2]
        stm[2, 1] <- stm[1, 2]
        stm[3, 1] <- stm[1, 3]
        st <- stm
      }

      # WIP: This is not requiered yet
      # sigma[5, 5] <- st[1, 1]
      # sigma[6, 6] <- st[2, 2]
      # sigma[7, 7] <- st[3, 3]
      # sigma[5, 6] <- st[1, 2]
      # sigma[5, 7] <- st[1, 3]
      # sigma[6, 7] <- st[2, 3]
      # sigma[6, 5] <- st[1, 2]
      # sigma[7, 5] <- st[1, 3]
      # sigma[7, 6] <- st[2, 3]

      # Estimated parameters and its standard errors
      # First parameter is the mixing proportion
      # the second and third parameters are the yields
      estim <- c(f1, yield1, yield2)
      std_estim <- sqrt(diag(st))

      yield1_l <- yield1 - std_estim[2]
      yield1_u <- yield1 + std_estim[2]
      yield2_l <- yield2 - std_estim[3]
      yield2_u <- yield2 + std_estim[3]

      # Fix negative yields
      if (yield2_l <= 0) {
        yield2_l <- 0
      }

      est_mixing_prop <- data.frame(
        y_estimate = c(estim[2], estim[3]),
        y_std_err = c(std_estim[2], std_estim[3]),
        f_estimate = c(estim[1], 1 - estim[1]),
        f_std_err = rep(std_estim[1], 2)
      )

      row.names(est_mixing_prop) <- c("x1", "x2")

      # Estimated received doses
      dose1 <- project_yield_base(yield1)
      dose1_l <- project_yield_lower(yield1_l, conf_int_curve)
      dose1_u <- project_yield_upper(yield1_u, conf_int_curve)

      if (yield2 <= 0.01) {
        dose2 <- 0
        dose2_l <- 0
        dose2_u <- project_yield_upper(yield2_u, conf_int_curve)
      } else {
        dose2 <- project_yield_base(yield2)
        if (yield2_l > 0) {
          dose2_l <- project_yield_lower(yield2_l, conf_int_curve)
        } else {
          dose2_l <- 0
        }
        dose2_u <- project_yield_upper(yield2_u, conf_int_curve)
      }

      est_yields <- data.frame(
        yield1 = c(yield1_l, yield1, yield1_u),
        yield2 = c(yield2_l, yield2, yield2_u)
      )

      row.names(est_yields) <- c("lower", "base", "upper")

      est_doses <- data.frame(
        dose1 = c(dose1_l, dose1, dose1_u),
        dose2 = c(dose2_l, dose2, dose2_u)
      )

      row.names(est_doses) <- c("lower", "base", "upper")

      frac <- function(g, f, mu1, mu2) {
        dose1 <- project_yield_base(mu1)

        if (mu2 <= 0.01) {
          dose2 <- 0
        } else {
          dose2 <- project_yield_base(mu2)
        }

        frac <- f / (f + (1 - f) * exp(g * (dose2 - dose1)))

        return(frac)
      }

      # Estimated fraction of irradiated blood for dose dose1
      est_F1 <- frac(gamma, f1, yield1, yield2)
      est_F2 <- 1 - est_F1

      # Approximated standard error
      std_err_F1 <- est_F1 * (1 - est_F1) * sqrt((dose2 - dose1)^2 * sigma[4, 4] + st[1, 1] / (f1^2 * (1 - f1)^2))

      est_frac <- data.frame(
        estimate = c(est_F1, est_F2),
        std_err = rep(std_err_F1, 2)
      )

      row.names(est_frac) <- c("x1", "x2")

      # WIP: This is not requiered yet
      # Gradient
      # h <- 0.000001
      # if (yield2 > 0.01) {
      #   c1 <- (frac(beta0 + h, beta1, beta2, gamma, f1, yield1, yield2) - F) / h
      #   c2 <- (frac(beta0, beta1 + h, beta2, gamma, f1, yield1, yield2) - F) / h
      #   c3 <- (frac(beta0, beta1, beta2 + h, gamma, f1, yield1, yield2) - F) / h
      #   c5 <- (frac(beta0, beta1, beta2, gam + h, f1, yield1, yield2) - F) / h
      #   c6 <- (frac(beta0, beta1, beta2, gamma, f1 + h, yield1, yield2) - F) / h
      #   c7 <- (frac(beta0, beta1, beta2, gamma, f1, yield1 + h, yield2) - F) / h
      #   c8 <- (frac(beta0, beta1, beta2, gamma, f1, yield1, yield2 + h) - F) / h
      #   grad <- c(c1, c2, c3, c5, c6, c7, c8)
      #   sqrt(t(grad) %*% sigma %*% grad)
      # }
      #
      # if (yield2 <= 0.01) {
      #   c1 <- (frac(beta0 + h, beta1, beta2, gamma, f1, yield1, yield2) - F) / h
      #   c2 <- (frac(beta0, beta1 + h, beta2, gamma, f1, yield1, yield2) - F) / h
      #   c3 <- (frac(beta0, beta1, beta2 + h, gamma, f1, yield1, yield2) - F) / h
      #   c5 <- (frac(beta0, beta1, beta2, gam + h, f1, yield1, yield2) - F) / h
      #   c6 <- (frac(beta0, beta1, beta2, gamma, f1 + h, yield1, yield2) - F) / h
      #   c7 <- (frac(beta0, beta1, beta2, gamma, f1, yield1 + h, yield2) - F) / h
      #   grad <- c(c1, c2, c3, c5, c6, c7)
      #   sigma2 <- sigma[1:6, 1:6]
      #   sqrt(t(grad) %*% sigma2 %*% grad)
      # }

      # Return
      results_list <- list(
        est_mixing_prop = est_mixing_prop,
        est_yields      = est_yields,
        est_doses       = est_doses,
        est_frac        = est_frac
      )

      return(results_list)
    }

    # Calculations ----

    # Calculate whole-body results
    est_doses_whole <- estimate_whole_body(aberr, cell, y_obs, conf_int_yield, conf_int_curve)
    if (assessment == "partial") {
      # Calculate partial results
      results_partial <- estimate_partial(aberr, cell, cases_data, fraction_coeff)
      # Parse results
      est_doses_partial <- results_partial[["est_doses"]]
      est_frac_partial <- results_partial[["est_frac"]]
    } else if (assessment == "hetero") {
      # Calculate heterogeneous result
      results_hetero <- estimate_hetero(counts, fit_coeffs, var_cov_mat, fraction_coeff, conf_int_yield, conf_int_curve)
      # Parse results
      est_mixing_prop_hetero <- results_hetero[["est_mixing_prop"]]
      est_yields_hetero <- results_hetero[["est_yields"]]
      est_doses_hetero <- results_hetero[["est_doses"]]
      est_frac_hetero <- results_hetero[["est_frac"]]
    }

    # Update plot ----

    # Data set for dose plotting

    if (assessment == "whole-body") {
      est_full_doses <- data.frame(
        dose =  c(est_doses_whole[["dose"]]),
        yield = c(est_doses_whole[["yield"]]),
        type =  c(rep("whole-body", 3)),
        level = rep(c("lower", "base", "upper"), 1)
      )
    } else if (assessment == "partial") {
      est_full_doses <- data.frame(
        dose =  c(est_doses_whole[["dose"]],  est_doses_partial[["dose"]]),
        yield = c(est_doses_whole[["yield"]], est_doses_partial[["yield"]]),
        type =  c(rep("whole-body", 3), rep("partial", 3)),
        level = rep(c("lower", "base", "upper"), 2)
      )
    } else if (assessment == "hetero") {
      est_full_doses <- data.frame(
        dose =  c(est_doses_whole[["dose"]],  est_doses_hetero[["dose1"]],  est_doses_hetero[["dose2"]]),
        yield = c(est_doses_whole[["yield"]], est_yields_hetero[["yield1"]], est_yields_hetero[["yield2"]]),
        type =  c(rep("whole-body", 3), rep("heterogeneous X1", 3), rep("heterogeneous X2", 3)),
        level = rep(c("lower", "base", "upper"), 3)
      )
    }

    max_dose <- 1.05 * max(est_full_doses[["dose"]])

    # Data set from fit
    plot_data <- broom::augment(fit_results)
    x0 <- fit_results$data[[1]]
    x1 <- fit_results$data[[2]]

    curves_data <- data.frame(dose = seq(0, max_dose, length.out = 100)) %>%
      mutate(
        yield = yield_fun(dose),
        yield_low = yield_fun(dose) - R_factor(0.83) * yield_error_fun(dose),
        yield_upp = yield_fun(dose) + R_factor(0.83) * yield_error_fun(dose)
      )
    # TODO: This should depend on the selected method

    # Make base plot
    gg_curve <- ggplot(plot_data / x0) +
      # Fitted curve
      stat_function(
        data = data.frame(x = c(0, max_dose)),
        mapping = aes(x),
        fun = function(x) yield_fun(x),
        linetype = "dashed"
      ) +
      # Confidence bands (Merkle, 1983)
      geom_ribbon(data = curves_data, aes(x = dose, ymin = yield_low, ymax = yield_upp), alpha = 0.25) +
      labs(x = "Dose (Gy)", y = "Aberrations / Cells") +
      theme_bw()

    # Add doses to plot
    gg_curve <- gg_curve +
      # Estimated whole-body doses
      geom_point(data = est_full_doses,
                 aes(x = dose, y = yield, colour = type, shape = level),
                 size = 2) +
      labs(colour = "Assessment", shape = "Confidence interval")

    # Return list ----
    # Make list of results to return
    if (assessment == "whole-body") {
      results_list <- list(
        assessment = assessment,
        est_doses_whole = est_doses_whole,
        gg_curve = gg_curve
      )
    } else if (assessment == "partial") {
      results_list <- list(
        assessment = assessment,
        est_doses_whole = est_doses_whole,
        est_doses_partial = est_doses_partial,
        est_frac_partial = est_frac_partial,
        gg_curve = gg_curve
      )
    } else if (assessment == "hetero"){
      results_list <- list(
        assessment = assessment,
        est_doses_whole = est_doses_whole,
        est_mixing_prop_hetero = est_mixing_prop_hetero,
        est_doses_hetero = est_doses_hetero,
        est_frac_hetero = est_frac_hetero,
        gg_curve = gg_curve
      )
    }

    return(results_list)
  })

  # Results outputs ----
  output$est_doses_whole <- renderRHandsontable({
    # Estimated recieved doses (whole-body)
    if (input$button_estimate <= 0) return(NULL)
    data()[["est_doses_whole"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_doses_partial <- renderRHandsontable({
    # Estimated recieved doses (partial)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "partial") return(NULL)
    data()[["est_doses_partial"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_frac_partial <- renderRHandsontable({
    # Estimated fraction of irradiated blood for dose dose1 (partial)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "partial") return(NULL)
    data()[["est_frac_partial"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_mixing_prop_hetero <- renderRHandsontable({
    # Estimated yields (heterogeneous)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "hetero") return(NULL)
    data()[["est_mixing_prop_hetero"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_doses_hetero <- renderRHandsontable({
    # Estimated recieved doses (heterogeneous)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "hetero") return(NULL)
    data()[["est_doses_hetero"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_frac_hetero <- renderRHandsontable({
    # Estimated fraction of irradiated blood for dose dose1 (heterogeneous)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "hetero") return(NULL)
    data()[["est_frac_hetero"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$plot <- renderPlot(
    # Plot of the data and fitted curve
    res = 120, {
      if (input$button_estimate <= 0) return(NULL)
      data()[["gg_curve"]]
    }
  )

  # Export plot ----
  output$save_plot <- downloadHandler(
    filename = function() {
      paste("fitting-curve-", Sys.Date(), input$save_plot_format, sep = "")
    },
    content = function(file) {
      ggsave(
        plot = data()[["gg_curve"]], filename = file,
        width = 6, height = 4.5, dpi = 96,
        device = gsub("\\.", "", input$save_plot_format)
      )
    }
  )

  # Export report ----
  # output$save_report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = function() {
  #     paste("report-", Sys.Date(), ".html", sep = "")
  #   },
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     normReport <- file.path("report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #
  #     # Set up parameters to pass to Rmd document
  #     params <- list(
  #       fit_result = data()[["fit_results"]],
  #       fit_coeffs = data()[["fit_coeffs"]],
  #       var_cov_mat = data()[["var_cov_mat"]],
  #       cor_mat = data()[["cor_mat"]],
  #       gg_curve = data()[["gg_curve"]]
  #     )
  #
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport,
  #                       output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
}
