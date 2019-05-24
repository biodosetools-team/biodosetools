# Dose Estimation Modules -------------------------------------------

dicentEstimateUI <- function(id, label) { #, locale = i18n) {
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
              div(
                class = "side-widget-tall",
                selectInput(
                  ns("formula_select"),
                  width = 165,
                  label = "Fitting formula",
                  choices = list(
                    "Linear quadratic" = c(
                      "Y = C + αD + βD²" = "lin-quad",
                      "Y = αD + βD²" = "lin-quad-no-int"
                    ),
                    "Linear" = c(
                      "Y = C + αD" = "lin",
                      "Y = αD" = "lin-no-int"
                    )
                  ),
                  selected = "lin-quad"
                )
              ),
              div(class = "widget-sep", br()),
              actionButton(ns("button_gen_table"), class = "options-button", style = "margin-left: -10px; margin-bottom: 2px;", "Generate tables"),

              br(),
              h6("Coefficients"),
              rHandsontableOutput(ns("fit_coeffs_hot")),

              br(),
              h6("Variance-covariance matrix"),
              rHandsontableOutput(ns("fit_var_cov_mat_hot")),

              br()
            ),
            # Load from file ----
            conditionalPanel(
              condition = "input.load_fit_data_check",
              ns = ns,
              fileInput(ns("load_fit_data"), label = "File input", accept = c(".rds"))
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

      bs4TabCard(
        id = ns("fit_results_tabs"),
        width = 7,
        side = "left",
        solidHeader = TRUE,
        closable = FALSE,

        bs4TabPanel(
          tabName = "Result of curve fit",
          active = TRUE,
          h6("Fit formula"),
          uiOutput(ns("fit_formula_tex")),

          h6("Coefficients"),
          rHandsontableOutput(ns("fit_coeffs"))
        ),
        bs4TabPanel(
          tabName = "Summary statistics",
          conditionalPanel(
            condition = "input.load_fit_data_check",
            ns = ns,
            h6("Model-level statistics"),
            rHandsontableOutput(ns("fit_model_statistics")),
            br()
          ),

          h6("Correlation matrix"),
          rHandsontableOutput(ns("fit_cor_mat")),

          br(),
          h6("Variance-covariance matrix"),
          rHandsontableOutput(ns("fit_var_cov_mat"))
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
              inputId = ns("load_case_data_check"),
              label = "Load data from file",
              value = FALSE, status = "warning"
            ),
            # Inputs
            conditionalPanel(
              condition = "!input.load_case_data_check",
              ns = ns,
              numericInput(ns("num_cases"), "Number of cases", value = 1),
              numericInput(ns("num_dicentrics"), "Maximum number of dicentrics per cell", value = 5)
            ),
            conditionalPanel(
              condition = "input.load_case_data_check",
              ns = ns,
              fileInput(ns("load_case_data"), label = "File input", accept = c("txt/csv", "text/comma-separated-values", "text/plain", ".csv", ".txt", ".dat"))
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
          rHandsontableOutput(ns("case_data_hot"))
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

          # Curve method selection
          div(
            style = "display: inline-block;",
            conditionalPanel(
              condition = "input.assessment_select != 'partial'",
              ns = ns,

              div(
                class = "side-widget-tall",
                selectInput(
                  ns("curve_method_select"),
                  label = "Error calculation",
                  width = "250px",
                  choices = list(
                    "Merkle's method (83%-83%)" = "merkle-83",
                    "Merkle's method (95%-95%)" = "merkle-95"
                    # "Simple method"             = "simple"
                  ),
                  selected = "merkle-83"
                )
              )
            ),

            # Partial method selection
            conditionalPanel(
              condition = "input.assessment_select == 'partial'",
              ns = ns,

              div(
                class = "side-widget-tall",
                selectInput(
                  ns("partial_method_select"),
                  label = "Calculation method",
                  width = "250px",
                  choices = list(
                    "Dolphin" = "dolphin"
                  ),
                  selected = "dolphin"
                )
              )
            )
          ),

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

          h6("Whole-body estimated dose"),
          rHandsontableOutput(ns("est_yields_whole")),
          br(),
          rHandsontableOutput(ns("est_doses_whole")),

          conditionalPanel(
            condition = "input.assessment_select == 'partial'",
            ns = ns,

            br(),
            h6("Dose recieved by the irradiated fraction"),
            rHandsontableOutput(ns("est_yields_partial")),
            br(),
            rHandsontableOutput(ns("est_doses_partial")),

            br(),
            h6("Initial fraction of irradiated cells"),
            rHandsontableOutput(ns("est_frac_partial"))
          ),

          conditionalPanel(
            condition = "input.assessment_select == 'hetero'",
            ns = ns,

            br(),
            h6("Observed fraction of irradiated cells and its yield"),
            rHandsontableOutput(ns("est_mixing_prop_hetero")),

            br(),
            h6("Dose recieved by the irradiated fraction"),
            div(
              class = "side-widget",
              rHandsontableOutput(ns("est_yields_hetero")),
              br(),
              rHandsontableOutput(ns("est_doses_hetero")),
              br()
            ),

            h6("Initial fraction of irradiated cells"),
            rHandsontableOutput(ns("est_frac_hetero"))
          ),

          # Help button
          topButton =
            bsButton(
              ns("help_dose_mixed_yields"),
              label = "",
              icon = icon("question"),
              style = "default", size = "default"
            ),
          # Help modal
          bs4MyModal(
            id = ns("help_dose_mixed_yields_dialog"),
            title = "Help: Partial and heterogeneous exposures",
            trigger = ns("help_dose_mixed_yields"),
            size = "large",
            withMathJax(includeMarkdown("help/help_dose_mixed_yields.md"))
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


dicentEstimateHotTable <- function(input, output, session, stringsAsFactors) {

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
      load_case_data <- input$load_case_data_check
      case_data <- input$load_case_data
      num_cases <- as.numeric(input$num_cases)
      num_dicentrics <- as.numeric(input$num_dicentrics) + 1
    })

    if (!load_case_data) {
      # Base data frame
      full_data <- data.frame(
        matrix(
          0,
          nrow = num_cases,
          ncol = num_dicentrics
        )
      ) %>%
        `colnames<-`(paste0("C", seq(0, num_dicentrics - 1, 1)))

    } else {
      full_data <- read.csv(case_data$datapath, header = TRUE) %>%
        dplyr::mutate_at(vars(starts_with("C")), as.integer)
    }

    return(full_data)
  })

  # Reactive data frame ----
  changed_data <- reactive({
    # Create button dependency for updating dimensions
    input$button_upd_table

    if (is.null(input$case_data_hot) || isolate(table_reset$value == 1)) {
      table_reset$value <- 0
      return(previous())
    } else if (!identical(previous(), input$case_data_hot)) {
      mytable <- as.data.frame(hot_to_r(input$case_data_hot))

      # Calculated columns
      mytable <- mytable %>%
        dplyr::mutate(
          N = 0,
          X = 0,
          y = 0,
          y_err = 0,
          DI = 0,
          u = 0
        ) %>%
        # select(D, N, X, everything())
        dplyr::select(N, X, everything())

      first_dicent_index <- 3
      last_dicent_index <- ncol(mytable) - 4
      num_rows <- nrow(mytable)


      mytable <- mytable %>%
        dplyr::mutate(
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
  output$case_data_hot <- renderRHandsontable({
    # Read number of columns
    num_cols <- ncol(changed_data())

    # Convert to hot and format table
    hot <- changed_data() %>%
      rhandsontable(width = "100%", height = "100%") %>%
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

dicentEstimateFittingCurveHotTable <- function(input, output, session, stringsAsFactors) {

  # Reset tables ----
  table_reset <- reactiveValues(value = 0)
  table_var_reset <- reactiveValues(value = 0)

  observeEvent(input$button_gen_table, {
    table_reset$value <- 1
    table_var_reset$value <- 1
  })

  # Initialize data frames ----
  previous_coeffs <- reactive({

    # Create button dependency for updating dimensions
    input$button_gen_table

    isolate({
      formula_select <- input$formula_select
    })

    if (formula_select == "lin-quad") {
      fit_coeffs_names <- c("C", "α", "β")
    } else if (formula_select == "lin-quad-no-int") {
      fit_coeffs_names <- c("α", "β")
    } else if (formula_select == "lin") {
      fit_coeffs_names <- c("C", "α")
    } else if (formula_select == "lin-no-int") {
      fit_coeffs_names <- c("α")
    }

    full_data <- data.frame(
      matrix(
        0.0,
        nrow = length(fit_coeffs_names),
        ncol = 2
      )
    ) %>%
      `row.names<-`(fit_coeffs_names) %>%
      `colnames<-`(c("estimate", "std.error"))

    return(full_data)
  })

  previous_var <- reactive({

    # Create button dependency for updating dimensions
    input$button_gen_table

    isolate({
      model_formula <- input$formula_select
    })

    if (model_formula == "lin-quad") {
      fit_coeffs_names <- c("C", "α", "β")
    } else if (model_formula == "lin-quad-no-int") {
      fit_coeffs_names <- c("α", "β")
    } else if (model_formula == "lin") {
      fit_coeffs_names <- c("C", "α")
    } else if (model_formula == "lin-no-int") {
      fit_coeffs_names <- c("α")
    }

    full_data <- matrix(
      0.0,
      nrow = length(fit_coeffs_names),
      ncol = length(fit_coeffs_names)
    ) %>%
      `row.names<-`(fit_coeffs_names) %>%
      `colnames<-`(fit_coeffs_names) %>%
      as.data.frame()

    return(full_data)
  })

  # Reactive data frames ----
  changed_coeffs_data <- reactive({
    # Create button dependency for updating dimensions
    input$button_gen_table

    if (is.null(input$fit_coeffs_hot) || isolate(table_reset$value == 1)) {
      table_reset$value <- 0
      return(previous_coeffs())
    } else if (!identical(previous_coeffs(), input$fit_coeffs_hot)) {
      fit_coeffs_names <- row.names(hot_to_r(input$fit_coeffs_hot))

      mytable <- as.data.frame(hot_to_r(input$fit_coeffs_hot)) %>%
        dplyr::mutate_all(as.numeric) %>%
        `row.names<-`(fit_coeffs_names)

      return(mytable)
    }
  })

  changed_var_data <- reactive({
    # Create button dependency for updating dimensions
    input$button_gen_table

    if (is.null(input$fit_var_cov_mat_hot) || isolate(table_var_reset$value == 1)) {
      table_var_reset$value <- 0
      return(previous_var())
    } else if (!identical(previous_var(), input$fit_var_cov_mat_hot)) {
      fit_coeffs_names <- row.names(hot_to_r(input$fit_var_cov_mat_hot))

      mytable <- as.data.frame(hot_to_r(input$fit_var_cov_mat_hot)) %>%
        dplyr::mutate_all(as.numeric) %>%
        `row.names<-`(fit_coeffs_names)

      return(mytable)
    }
  })

  # Output ----
  output$fit_coeffs_hot <- renderRHandsontable({
    # Read number of columns
    num_cols <- ncol(changed_coeffs_data())

    # Convert to hot and format table
    hot <- changed_coeffs_data() %>%
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.000000")

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })

  output$fit_var_cov_mat_hot <- renderRHandsontable({
    # Read number of columns
    num_cols <- ncol(changed_var_data())

    # Convert to hot and format table
    hot <- changed_var_data() %>%
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.00000000")

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })
}


dicentEstimateFittingCurve <- function(input, output, session, stringsAsFactors) {

  # Calculations ----
  data <- reactive({
    input$button_view_fit_data

    isolate({
      load_fit_data <- input$load_fit_data_check
      fit_data <- input$load_fit_data
    })

    if (load_fit_data) {
      fit_results_list <- readRDS(fit_data$datapath)
    } else {
      model_formula <- input$formula_select
      # Parse formula
      if (model_formula == "lin-quad") {
        fit_formula_tex <- "Y = C + \\alpha D + \\beta D^{2}"
      } else if (model_formula == "lin") {
        fit_formula_tex <- "Y = C + \\alpha D"
      }
      else if (model_formula == "lin-quad-no-int") {
        fit_formula_tex <- "Y = \\alpha D + \\beta D^{2}"
      }
      else if (model_formula == "lin-no-int") {
        fit_formula_tex <- "Y = \\alpha D"
      }

      fit_coeffs_raw <- hot_to_r(input$fit_coeffs_hot)
      fit_coeffs <- fit_coeffs_raw %>%
        cbind(statistic = c(rep(0, nrow(fit_coeffs_raw)))) %>%
        as.matrix()

      fit_var_cov_mat <- hot_to_r(input$fit_var_cov_mat_hot) %>%
        as.matrix()

      fit_cor_mat <- fit_var_cov_mat
      for (x_var in rownames(fit_var_cov_mat)) {
        for (y_var in colnames(fit_var_cov_mat)) {
          fit_cor_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var] / (fit_coeffs[x_var, "std.error"] * fit_coeffs[y_var, "std.error"])
        }
      }

      fit_results_list <- list(
        fit_formula_tex = fit_formula_tex,
        fit_coeffs = fit_coeffs,
        fit_var_cov_mat = fit_var_cov_mat,
        fit_cor_mat = fit_cor_mat
      )
    }

    return(fit_results_list)
  })

  # Results outputs ----
  output$fit_formula_tex <- renderUI({
    # Fitting formula
    if (input$button_view_fit_data <= 0) return(NULL)
    withMathJax(paste0("$$", data()[["fit_formula_tex"]], "$$"))
  })

  output$fit_model_statistics <- renderRHandsontable({
    # Model-level statistics
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_model_statistics"]] %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 70)
  })

  output$fit_coeffs <- renderRHandsontable({
    # Coefficients 'fit_coeffs'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_coeffs"]] %>%
      formatC(format = "e", digits = 3) %>%
      as.data.frame() %>%
      dplyr::select(-statistic) %>%
      as.matrix() %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(halign = "htRight")
  })

  output$fit_var_cov_mat <- renderRHandsontable({
    # Variance-covariance matrix 'var_cov_mat'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_var_cov_mat"]] %>%
      formatC(format = "e", digits = 3) %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(halign = "htRight")
  })

  output$fit_cor_mat <- renderRHandsontable({
    # Correlation matrix 'cor_mat'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_cor_mat"]] %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.000")
  })
}


dicentEstimateResults <- function(input, output, session, stringsAsFactors) {

  data <- reactive({
    # Calcs: get variables ----
    input$button_estimate

    isolate({
      # Fit data
      load_fit_data <- input$load_fit_data_check
      fit_data <- input$load_fit_data
      exposure <- input$exposure_select
      assessment <- input$assessment_select
      # curve_method <- input$curve_method_select

      # Cases data
      case_data <- hot_to_r(input$case_data_hot)

      # Coefficient input selection
      fraction_coeff <- input$fraction_coeff_select
    })

    # Get error/calculation method
    if (assessment != "partial") {
      curve_method <- input$curve_method_select
    } else {
      curve_method <- input$partial_method_select
    }

    # Get fitting data ----
    if (load_fit_data) {
      fit_results_list <- readRDS(fit_data$datapath)

      # Summarise fit
      fit_coeffs <- fit_results_list[["fit_coeffs"]]
      fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]
      fit_formula_tex <- fit_results_list[["fit_formula_tex"]]
    } else {
      fit_coeffs <- hot_to_r(input$fit_coeffs_hot)
      fit_var_cov_mat <- hot_to_r(input$fit_var_cov_mat_hot)

      model_formula <- input$formula_select
      # Parse formula
      if (model_formula == "lin-quad") {
        fit_formula_tex <- "Y = C + \\alpha D + \\beta D^{2}"
      } else if (model_formula == "lin") {
        fit_formula_tex <- "Y = C + \\alpha D"
      }
      else if (model_formula == "lin-quad-no-int") {
        fit_formula_tex <- "Y = \\alpha D + \\beta D^{2}"
      }
      else if (model_formula == "lin-no-int") {
        fit_formula_tex <- "Y = \\alpha D"
      }
    }

    # Generalized variance-covariance matrix
    general_fit_coeffs <- numeric(length = 3L) %>%
      `names<-`(c("C", "α", "β"))

    for (var in rownames(fit_coeffs)) {
      general_fit_coeffs[var] <- fit_coeffs[var, "estimate"]
    }

    # Generalized fit coefficients
    general_var_cov_mat <- matrix(0, nrow = 3, ncol = 3) %>%
      `row.names<-`(c("C", "α", "β")) %>%
      `colnames<-`(c("C", "α", "β"))

    for (x_var in rownames(fit_var_cov_mat)) {
      for (y_var in colnames(fit_var_cov_mat)) {
        general_var_cov_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var]
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
      # Used in report (dummy values)
      protracted_time <- NA
      protracted_life_time <- NA
    }

    # Generalized curves ----
    yield_fun <- function(d, G) {
      general_fit_coeffs[[1]] +
        general_fit_coeffs[[2]] * d +
        general_fit_coeffs[[3]] * d^2 * G
    }

    # R factor depeding on selected CI
    chisq_df <- nrow(fit_coeffs)
    R_factor <- function(conf_int = 0.95) {
      sqrt(qchisq(conf_int, df = chisq_df))
    }

    yield_error_fun <- function(d, G) {
      res <- general_var_cov_mat[["C", "C"]] +
        general_var_cov_mat[["α", "α"]] * d^2 +
        general_var_cov_mat[["β", "β"]] * d^4 * G^2 +
        2 * general_var_cov_mat[["C", "α"]] * d +
        2 * general_var_cov_mat[["C", "β"]] * d^2 * G +
        2 * general_var_cov_mat[["α", "β"]] * d^3 * G
      if (sum(res < 0) > 0) {
        rep(0, length(res))
      } else {
        return(sqrt(res))
      }
    }

    # Select CI depending on selected method
    if (grepl("merkle", curve_method, fixed = TRUE)) {
      conf_int_curve <- paste0("0.", gsub("\\D", "", curve_method)) %>% as.numeric()
      conf_int_yield <- conf_int_curve
    # } else if (curve_method == "simple") {
    #   conf_int_curve <- 0 # This makes R_factor = 0
    #   conf_int_yield <- 0.95
    } else { # For partial body
      conf_int_curve <- 0.83
      conf_int_yield <- conf_int_curve
      conf_int_dolphin <- 0.95
    }

    # Correct conf_int_yield if simple method is required
    correct_conf_int <- function(conf_int, d, G, type) {
      res <- general_var_cov_mat[["C", "C"]] +
        general_var_cov_mat[["α", "α"]] * d^2 +
        general_var_cov_mat[["β", "β"]] * d^4 * G^2 +
        2 * general_var_cov_mat[["C", "α"]] * d +
        2 * general_var_cov_mat[["C", "β"]] * d^2 * G +
        2 * general_var_cov_mat[["α", "β"]] * d^3 * G
      if (sum(res <= 0) > 1) {
        if (type == "curve") {
          conf_int <- 0
        } else if (type == "yield") {
          conf_int <- 0.95
        }
      }

      return(conf_int)
    }

    conf_int_curve <- conf_int_curve %>% correct_conf_int(seq(0, 10, 0.2), protracted_g_value, type = "curve")
    conf_int_yield <- conf_int_yield %>% correct_conf_int(seq(0, 10, 0.2), protracted_g_value, type = "yield")

    # Calculate infimums of the different curves
    yield_low_inf <- yield_fun(0, 1) - R_factor(conf_int_curve) * yield_error_fun(0, 1)
    yield_est_inf <- yield_fun(0, 1)
    yield_upp_inf <- yield_fun(0, 1) + R_factor(conf_int_curve) * yield_error_fun(0, 1)

    # Projection functions ----

    project_yield_estimate <- function(yield) {
      if (yield >= yield_est_inf) {
        uniroot(function(dose) {
          yield_fun(dose, protracted_g_value) - yield
        }, c(1e-16, 100))$root
      } else {
        0
      }
    }

    project_yield_lower <- function(yield, conf_int) {
      if (yield >= yield_low_inf) {
        uniroot(function(dose) {
          yield_fun(dose, protracted_g_value) + R_factor(conf_int) * yield_error_fun(dose, protracted_g_value) - yield
        }, c(1e-16, 100))$root
      } else {
        0
      }
    }

    project_yield_upper <- function(yield, conf_int) {
      if (yield >= yield_upp_inf) {
        uniroot(function(dose) {
          yield_fun(dose, protracted_g_value) - R_factor(conf_int) * yield_error_fun(dose, protracted_g_value) - yield
        }, c(1e-16, 100))$root
      } else {
        0
      }
    }


    # Correction functions -------------------------------------

    # Correct negative values
    correct_negative_vals <- function(x) {
      ifelse(x < 0, 0, x)
    }

    # Correct yields if they are below the curve
    correct_yield <- function(yield) {
      yield_name <- deparse(substitute(yield))
      suffix <- stringr::str_extract(yield_name, "est|low|upp")
      yield_inf <- get(paste("yield", suffix, "inf", sep = "_"))

      if (yield < yield_inf) {
        yield <- 0
      }
      yield <- correct_negative_vals(yield)

      return(yield)
    }

    # Function to make sure F is bounded by 0 and 1
    correct_boundary <- function(x) {
      if (x > 1) {
        return(1)
      } else if (x < 0) {
        return(0)
      } else {
        return(x)
      }
    }


    # Dose estimation functions ----

    # Calcs: whole-body estimation
    estimate_whole_body <- function(case_data, conf_int_yield, conf_int_curve, protracted_g_value) {

      aberr <- case_data[["X"]]
      cells <- case_data[["N"]]
      yield_est <- case_data[["y"]]

      # Calculate CI using Exact Poisson tests
      aberr_row <-  poisson.test(x = round(aberr, 0), conf.level = conf_int_yield)[["conf.int"]]

      aberr_low <- aberr_row[1]
      aberr_upp <- aberr_row[2]

      yield_low <- aberr_low / cells
      yield_upp <- aberr_upp / cells
      # TODO: possible modification IAEA§9.7.3

      # Correct "unrootable" yields
      yield_est <- correct_yield(yield_est)
      yield_low <- correct_yield(yield_low)
      yield_upp <- correct_yield(yield_upp)

      # Calculate projections
      dose_est <- project_yield_estimate(yield_est)
      dose_low <- project_yield_lower(yield_low, conf_int_curve)
      dose_upp <- project_yield_upper(yield_upp, conf_int_curve)

      # Whole-body estimation results
      est_doses <- data.frame(
        yield = c(yield_low, yield_est, yield_upp),
        dose  = c(dose_low, dose_est, dose_upp)
      ) %>%
        `row.names<-`(c("lower", "estimate", "upper"))

      # Return object
      return(est_doses)
    }

    # Calcs: partial dose estimation
    estimate_partial_dolphin <- function(case_data, general_fit_coeffs, general_var_cov_mat, fraction_coeff,
                                         conf_int, protracted_g_value, cov = TRUE) {
      # cov: TRUE if the covariances of the regression coefficients should be considered,
      #      otherwise only the diagonal of the covariance matrix is used

      # Function to get the fisher information matrix
      get_cov_ZIP_ML <- function(lambda, pi, cells) {
        # For the parameters of a ZIP distribution (lambda and pi) where 1-p is the fraction of extra zeros
        info_mat <- matrix(NA, nrow = 2, ncol = 2)
        info_mat[1, 1] <- cells * pi * ((pi - 1) * exp(-lambda) / (1 - pi + pi * exp(-lambda)) + 1 / lambda)
        info_mat[1, 2] <- cells * exp(-lambda) / (1 - pi + pi * exp(-lambda))
        info_mat[2, 1] <- info_mat[1, 2]
        info_mat[2, 2] <- cells * (1 - exp(-lambda)) / (pi * (1 - pi + pi * exp(-lambda)))

        # Solve system
        cov_est <- base::solve(info_mat)

        return(cov_est)
      }

      # Input of the parameter gamma and its variance
      if (fraction_coeff == "gamma") {
        d0 <- 1 / input$gamma_coeff
      } else if (fraction_coeff == "d0") {
        d0 <- input$d0_coeff
      }

      # Get fitting model variables
      aberr <- case_data[["X"]]
      cells <- case_data[["N"]]
      cells_0 <- case_data[["C0"]]
      cells_1 <- case_data[["C1"]]

      C <- general_fit_coeffs[[1]]
      α <- general_fit_coeffs[[2]]
      β <- general_fit_coeffs[[3]]
      var_cov_mat <- general_var_cov_mat

      # Detect fitting model
      fit_is_lq <- isFALSE(β == 0)

      # If there are no cells with > 1 dic, the results include only NAs
      # This should be handled somewhere downstream
      if (cells - (cells_0 + cells_1) == 0) {
        # Partial estimation results
        est_doses <- data.frame(
          yield = rep(NA, 3),
          dose = rep(NA, 3)
        )

        # Estimated fraction
        est_frac <- data.frame(
          fraction = rep(NA, 3)
        )
      } else {
        # Get estimates for pi and lambda
        lambda_est <- uniroot(function(yield) {
          yield / (1 - exp(-yield)) - aberr / (cells - cells_0)
        }, c(1e-16, 100))$root

        pi_est <- aberr / (lambda_est * cells)

        # Get the covariance matrix for the parameters of the ZIP distribution
        cov_est <- get_cov_ZIP_ML(lambda_est, pi_est, cells)

        # Calculate dose and derivatives dependig on linear/linear-quadratic fitting model
        if (fit_is_lq) {
          # Update β to correct for protracted exposures
          β <- β * protracted_g_value

          # Auxiliary variable
          z <- α^2 + 4 * β * (lambda_est - C)

          # Get estimate for dose
          dose_est <- (-α + sqrt(z)) / (2 * β)

          # Derivatives of regression curve coefs
          deriv_lambda <- (z)^(-0.5)
          deriv_C <- -(z)^(-0.5)
          deriv_α <- (1 / (2 * β)) * (-1 + α * z^(-0.5))
          deriv_β <- protracted_g_value * (4 * β * (lambda_est - C) * (z^(-0.5)) + 2 * α - 2 * z^(0.5)) / (4 * β^2)
        } else {
          # Get estimate for dose
          dose_est <- (lambda_est - C) / α

          # Derivatives of regression curve coefs
          deriv_lambda <- 1 / α
          deriv_C <- - (1 / α)
          deriv_α <- (C - lambda_est) / α^2
          deriv_β <- 0
        }

        # Get the covariance matrix for the parameters of the ZIP distribution
        cov_est <- get_cov_ZIP_ML(lambda_est, pi_est, cells)

        if (fit_is_lq) {
          cov_extended <- matrix(0, nrow = 5, ncol = 5)
          cov_extended[1:3, 1:3] <- var_cov_mat
          cov_extended[4:5, 4:5] <- cov_est
        } else {
          cov_extended <- matrix(0, nrow = 4, ncol = 4)
          cov_extended[1:3, 1:3] <- var_cov_mat
          cov_extended[3:4, 3:4] <- cov_est
        }

        # Get variance of lambda based on delta methods (see Savage et al.)
        lambda_est_sd <- sqrt(cov_est[1, 1])

        # Get confidence interval of lambda estimates
        lambda_low <- lambda_est - qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd
        lambda_upp <- lambda_est + qnorm(conf_int + (1 - conf_int) / 2) * lambda_est_sd

        if (cov) {
          dose_est_var <-
            (deriv_C^2) * var_cov_mat[1, 1] +
            (deriv_α^2) * var_cov_mat[2, 2] +
            (deriv_β^2) * var_cov_mat[3, 3] +
            (deriv_lambda^2) * (lambda_est_sd^2) +
            2 * (deriv_C * deriv_α) * var_cov_mat[1, 2] +
            2 * (deriv_C * deriv_β) * var_cov_mat[1, 3] +
            2 * (deriv_α * deriv_β) * var_cov_mat[2, 3]
        } else {
          dose_est_var <-
            (deriv_C^2) * var_cov_mat[1, 1] +
            (deriv_α^2) * var_cov_mat[2, 2] +
            (deriv_β^2) * var_cov_mat[3, 3] +
            (deriv_lambda^2) * (lambda_est_sd^2)
          # TODO: These values shouldn't even exist if cov == FALSE
        }

        # Get confidence interval of dose estimates
        dose_low <- dose_est - qnorm(conf_int + (1 - conf_int) / 2) * sqrt(dose_est_var)
        dose_upp <- dose_est + qnorm(conf_int + (1 - conf_int) / 2) * sqrt(dose_est_var)

        # Correct negative values
        lambda_low <- correct_negative_vals(lambda_low)
        lambda_upp <- correct_negative_vals(lambda_upp)
        dose_low <- correct_negative_vals(dose_low)
        dose_est <- correct_negative_vals(dose_est)
        dose_upp <- correct_negative_vals(dose_upp)

        # Partial estimation results
        est_doses <- data.frame(
          yield = c(lambda_low, lambda_est, lambda_upp),
          dose = c(dose_low, dose_est, dose_upp)
        ) %>%
          `row.names<-`(c("lower", "estimate", "upper"))

        # Get estimate for fraction irradiated
        F_est <- pi_est * exp(dose_est / d0) / (1 - pi_est + pi_est * exp(dose_est / d0))

        # Get standard error of fraction irradiated by deltamethod
        if (fit_is_lq) {
          # x5: pi_est, x4: lambda_est, x1: C, x2: alpha, x3: beta
          formula <- paste(
            "~ x5 * exp((-x2 + sqrt(x2^2 + 4 * x3 * (x4 - x1))) / (2 * x3 *", d0,")) /
            (1 - x5 + x5 * exp((-x2 + sqrt(x2^2 + 4 * x3 * (x4 - x1))) / (2 * x3 *", d0,")))",
            sep = ""
          )
          F_est_sd <- msm::deltamethod(as.formula(formula), mean = c(C, α, β, lambda_est, pi_est), cov = cov_extended)
        } else {
          # x4: pi_est, x3: lambda_est, x1: C, x2: alpha
          formula <- paste(
            "~ x4 * exp((x3 - x1) / (x2 *", d0,")) /
            (1 - x4 + x4 * exp((x3 - x1) / (x2 *", d0,")))",
            sep = ""
          )
          F_est_sd <- msm::deltamethod(as.formula(formula), mean = c(C, α, lambda_est, pi_est), cov = cov_extended)
        }

        # Get confidence interval of fraction irradiated
        F_upp <- F_est + qnorm(conf_int + (1 - conf_int) / 2) * F_est_sd
        F_low <- F_est - qnorm(conf_int + (1 - conf_int) / 2) * F_est_sd

        # Set to zero if F < 0 and to 1 if F > 1
        F_low <- correct_boundary(F_low)
        F_est <- correct_boundary(F_est)
        F_upp <- correct_boundary(F_upp)

        # Estimated fraction
        est_frac <- data.frame(
          fraction = c(F_low, F_est, F_upp)
        ) %>%
          `row.names<-`(c("lower", "estimate", "upper"))
      }

      # Return objects
      results_list <- list(
        est_doses = est_doses,
        est_frac = est_frac
      )

      return(results_list)
    }

    # Calcs: heterogeneous dose estimation
    estimate_hetero <- function(case_data, general_fit_coeffs, general_var_cov_mat, fraction_coeff,
                                conf_int_yield, conf_int_curve, protracted_g_value) {

      # Select dicentric counts
      counts <- case_data[1, ] %>%
        select(contains("C")) %>%
        as.numeric()

      # Get fitting model variables
      cells <- case_data[["N"]]
      cells_0 <- case_data[["C0"]]
      cells_1 <- case_data[["C1"]]

      # Likelihood function
      loglik <- function(coeffs) {
        loglik <- sum(log(coeffs[1] * dpois(y, coeffs[2]) + (1 - coeffs[1]) * dpois(y, coeffs[3])))

        return(-loglik)
      }

      # Function to calculate fractions of irradiated blood
      get_fraction <- function(g, f, mu1, mu2) {
        dose1_est <- project_yield_estimate(mu1)

        if (mu2 <= 0.01) {
          dose2_est <- 0
        } else {
          dose2_est <- project_yield_estimate(mu2)
        }

        frac <- f / (f + (1 - f) * exp(g * (dose2_est - dose1_est)))

        return(frac)
      }

      # If there are no cells with > 1 dic, the results include only NAs
      # This should be handled somewhere downstream
      if (cells - (cells_0 + cells_1) == 0) {
        # Estimated yields
        est_yields <- data.frame(
          yield1 = rep(NA, 3),
          yield2 = rep(NA, 3)
        )

        # Estimated mixing proportion
        est_mixing_prop <- data.frame(
          y_estimate = rep(NA, 2),
          y_std_err =  rep(NA, 2),
          f_estimate = rep(NA, 2),
          f_std_err =  rep(NA, 2)
        )

        # Estimated doses
        est_doses <- data.frame(
          dose1 = rep(NA, 3),
          dose2 = rep(NA, 3)
        )

        # Estimated fraction
        est_frac <- data.frame(
          estimate = rep(NA, 2),
          std_err = rep(NA, 2)
        )
      } else {
        # Get cases data and store in vector y
        y <- rep(seq(0, length(counts) - 1, 1), counts)
        x <- c(rep(1, length(y)))

        fit <- mixtools::poisregmixEM(y, x, addintercept = FALSE, k = 2)

        # Get fitting model variables
        C <- general_fit_coeffs[[1]]
        α <- general_fit_coeffs[[2]]
        β <- general_fit_coeffs[[3]]
        var_cov_mat <- general_var_cov_mat

        # Input of the variance-covariance matrix of the parameters
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

        # Calculate Maximum Likielihood Estimation
        MLE <- optim(
          par = c(fit$lambda[1], exp(fit$beta)[1], exp(fit$beta)[2]),
          fn = loglik,
          method = c("L-BFGS-B"),
          lower = c(0.01, 0.01, 0.01),
          upper = c(0.99, Inf, Inf),
          hessian = TRUE
        )

        st <- solve(MLE$hessian)
        yield1_est <- MLE$par[2]
        yield2_est <- MLE$par[3]
        frac1 <- MLE$par[1]

        if (yield1_est < yield2_est) {
          yield1_est <- MLE$par[3]
          yield2_est <- MLE$par[2]
          frac1 <- 1 - frac1
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
        estim <- c(frac1, yield1_est, yield2_est)
        std_estim <- sqrt(diag(st))

        yield1_low <- yield1_est - std_estim[2]
        yield1_upp <- yield1_est + std_estim[2]

        yield2_low <- yield2_est - std_estim[3]
        yield2_upp <- yield2_est + std_estim[3]

        # Correct "unrootable" yields
        yield1_est <- correct_yield(yield1_est)
        yield1_low <- correct_yield(yield1_low)
        yield1_upp <- correct_yield(yield1_upp)

        yield2_est <- correct_yield(yield2_est)
        yield2_low <- correct_yield(yield2_low)
        yield2_upp <- correct_yield(yield2_upp)

        est_yields <- data.frame(
          yield1 = c(yield1_low, yield1_est, yield1_upp),
          yield2 = c(yield2_low, yield2_est, yield2_upp)
        ) %>%
          `row.names<-`(c("lower", "estimate", "upper"))

        # Estimated mixing proportion
        est_mixing_prop <- data.frame(
          y_estimate = c(estim[2], estim[3]),
          y_std_err =  c(std_estim[2], std_estim[3]),
          f_estimate = c(estim[1], 1 - estim[1]),
          f_std_err =  rep(std_estim[1], 2)
        ) %>%
          `row.names<-`(c("dose1", "dose2"))

        # Estimated received doses
        dose1_est <- project_yield_estimate(yield1_est)
        dose1_low <- project_yield_lower(yield1_low, conf_int_curve)
        dose1_upp <- project_yield_upper(yield1_upp, conf_int_curve)

        dose2_est <- project_yield_estimate(yield2_est)
        dose2_low <- project_yield_lower(yield2_low, conf_int_curve)
        dose2_upp <- project_yield_upper(yield2_upp, conf_int_curve)

        est_doses <- data.frame(
          dose1 = c(dose1_low, dose1_est, dose1_upp),
          dose2 = c(dose2_low, dose2_est, dose2_upp)
        ) %>%
          `row.names<-`(c("lower", "estimate", "upper"))

        # Estimated fraction of irradiated blood for dose dose1
        F1_est <- get_fraction(gamma, frac1, yield1_est, yield2_est)
        F1_est <- correct_boundary(F1_est)
        F2_est <- 1 - F1_est

        # Approximated standard error
        F1_est_sd <- F1_est * (1 - F1_est) * sqrt((dose2_est - dose1_est)^2 * sigma[4, 4] + st[1, 1] / (frac1^2 * (1 - frac1)^2))

        est_frac <- data.frame(
          estimate = c(F1_est, F2_est),
          std_err = rep(F1_est_sd, 2)
        ) %>%
          `row.names<-`(c("dose1", "dose2"))

        # WIP: This is not requiered yet
        # Gradient
        # h <- 0.000001
        # if (yield2_est > 0.01) {
        #   c1 <- (get_fraction(C + h, α, β, gamma, frac1, yield1_est, yield2_est) - F) / h
        #   c2 <- (get_fraction(C, α + h, β, gamma, frac1, yield1_est, yield2_est) - F) / h
        #   c3 <- (get_fraction(C, α, β + h, gamma, frac1, yield1_est, yield2_est) - F) / h
        #   c5 <- (get_fraction(C, α, β, gam + h, frac1, yield1_est, yield2_est) - F) / h
        #   c6 <- (get_fraction(C, α, β, gamma, frac1 + h, yield1_est, yield2_est) - F) / h
        #   c7 <- (get_fraction(C, α, β, gamma, frac1, yield1_est + h, yield2_est) - F) / h
        #   c8 <- (get_fraction(C, α, β, gamma, frac1, yield1_est, yield2_est + h) - F) / h
        #   grad <- c(c1, c2, c3, c5, c6, c7, c8)
        #   sqrt(t(grad) %*% sigma %*% grad)
        # }
        # if (yield2_est <= 0.01) {
        #   c1 <- (get_fraction(C + h, α, β, gamma, frac1, yield1_est, yield2_est) - F) / h
        #   c2 <- (get_fraction(C, α + h, β, gamma, frac1, yield1_est, yield2_est) - F) / h
        #   c3 <- (get_fraction(C, α, β + h, gamma, frac1, yield1_est, yield2_est) - F) / h
        #   c5 <- (get_fraction(C, α, β, gam + h, frac1, yield1_est, yield2_est) - F) / h
        #   c6 <- (get_fraction(C, α, β, gamma, frac1 + h, yield1_est, yield2_est) - F) / h
        #   c7 <- (get_fraction(C, α, β, gamma, frac1, yield1_est + h, yield2_est) - F) / h
        #   grad <- c(c1, c2, c3, c5, c6, c7)
        #   sigma2 <- sigma[1:6, 1:6]
        #   sqrt(t(grad) %*% sigma2 %*% grad)
        # }
      }

      # Return objects
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
    est_doses_whole <- estimate_whole_body(
      case_data, conf_int_yield,
      conf_int_curve, protracted_g_value
    )
    if (assessment == "partial") {
      # Calculate partial results
      results_partial <- estimate_partial_dolphin(
        case_data, general_fit_coeffs, general_var_cov_mat, fraction_coeff,
        conf_int_dolphin, protracted_g_value
      )
      # Parse results
      est_doses_partial <- results_partial[["est_doses"]]
      est_frac_partial <- results_partial[["est_frac"]]
    } else if (assessment == "hetero") {
      # Calculate heterogeneous result
      results_hetero <- estimate_hetero(
        case_data, general_fit_coeffs, general_var_cov_mat, fraction_coeff,
        conf_int_yield, conf_int_curve, protracted_g_value
      )
      # Parse results
      est_mixing_prop_hetero <- results_hetero[["est_mixing_prop"]]
      est_yields_hetero <- results_hetero[["est_yields"]]
      est_doses_hetero <- results_hetero[["est_doses"]]
      est_frac_hetero <- results_hetero[["est_frac"]]
    }

    # Make plot ----

    # Data set for dose plotting
    if (assessment == "whole-body") {
      est_full_doses <- data.frame(
        dose =  c(est_doses_whole[["dose"]]),
        yield = c(est_doses_whole[["yield"]]),
        type =  c(rep("Whole-body", 3)),
        level = rep(c("Lower", "Estimate", "Upper"), 1)
      )
    } else if (assessment == "partial") {
      est_full_doses <- data.frame(
        dose =  c(est_doses_whole[["dose"]],  est_doses_partial[["dose"]]),
        yield = c(est_doses_whole[["yield"]], est_doses_partial[["yield"]]),
        type =  c(rep("Whole-body", 3), rep("Partial", 3)),
        level = rep(c("Lower", "Estimate", "Upper"), 2)
      )
    } else if (assessment == "hetero") {
      est_full_doses <- data.frame(
        dose =  c(est_doses_whole[["dose"]],  est_doses_hetero[["dose1"]],  est_doses_hetero[["dose2"]]),
        yield = c(est_doses_whole[["yield"]], est_yields_hetero[["yield1"]], est_yields_hetero[["yield2"]]),
        type =  c(rep("Whole-body", 3), rep("Heterogeneous X1", 3), rep("Heterogeneous X2", 3)),
        level = rep(c("Lower", "Estimate", "Upper"), 3)
      )
    }

    # Rightmost limit of the plot
    max_dose <- 1.05 * est_full_doses[["dose"]] %>%
      ifelse(is.na(.), 0, .) %>%
      max()

    # Plot data from curves
    curves_data <- data.frame(dose = seq(0, max_dose, length.out = 100)) %>%
      dplyr::mutate(
        yield = yield_fun(dose, protracted_g_value),
        yield_low = yield_fun(dose, protracted_g_value) - R_factor(conf_int_curve) * yield_error_fun(dose, protracted_g_value),
        yield_upp = yield_fun(dose, protracted_g_value) + R_factor(conf_int_curve) * yield_error_fun(dose, protracted_g_value)
      )

    # Make base plot
    gg_curve <- ggplot(curves_data) +
      # Fitted curve
      stat_function(
        data = data.frame(x = c(0, max_dose)),
        mapping = aes(x),
        fun = function(x) yield_fun(x, protracted_g_value),
        linetype = "dashed"
      ) +
      # Confidence bands (Merkle, 1983)
      geom_ribbon(data = curves_data, aes(x = dose, ymin = yield_low, ymax = yield_upp), alpha = 0.25) +
      labs(x = "Dose (Gy)", y = "Dicentrics/cells") +
      theme_bw()

    # Add doses to plot
    gg_curve <- gg_curve +
      # Estimated whole-body doses
      geom_point(data = est_full_doses,
                 aes(x = dose, y = yield, color = type, shape = level),
                 size = 2, na.rm = TRUE) +
      # Confidence intervals
      geom_point(data = data.frame(x = c(0,0), y = c(0,0), z = c("curve", "yield")),
                 aes(x = x, y = y, fill = z),
                 alpha = 0) +
      scale_fill_manual("Confidence interval",
                        values = c("curve" = "red", "yield" = "green"),
                        labels = c(
                          paste0("Curve: ", round(100 * conf_int_curve, 0), "%"),
                          paste0("Yield: ", round(100 * conf_int_yield, 0), "%")
                        )) +
      guides(color = guide_legend(order = 1),
             shape = guide_legend(order = 2),
             fill = guide_legend(order = 3)) +
      labs(color = "Assessment", shape = "Estimation", fill = "Confidence interval")

    # Return list ----

    # Make basic list of results to return
    est_results_list <- list(
      # Used in app
      assessment = assessment,
      # Whole-body
      est_doses_whole = est_doses_whole,
      # Partial
      est_doses_partial = NA,
      est_frac_partial = NA,
      # Heterogeneous
      est_mixing_prop_hetero = NA,
      est_yields_hetero = NA,
      est_doses_hetero = NA,
      est_frac_hetero = NA,
      # Plot
      gg_curve = gg_curve,
      # Required for report
      fit_coeffs = fit_coeffs,
      protraction = c(0, 0, 0),
      fit_formula_tex = fit_formula_tex,
      case_data = case_data,
      case_description = input$case_description,
      results_comments = input$results_comments
    )
    if (assessment == "partial") {
      # Partial
      est_results_list[["est_doses_partial"]] <- est_doses_partial
      est_results_list[["est_frac_partial"]] <- est_frac_partial
      # Reset Heterogeneous
      est_results_list[["est_mixing_prop_hetero"]] <- NA
      est_results_list[["est_yields_hetero"]] <- NA
      est_results_list[["est_doses_hetero"]] <- NA
      est_results_list[["est_frac_hetero"]] <- NA

    } else if (assessment == "hetero"){
      # Heterogeneous
      est_results_list[["est_mixing_prop_hetero"]] <- est_mixing_prop_hetero
      est_results_list[["est_yields_hetero"]] <- est_yields_hetero
      est_results_list[["est_doses_hetero"]] <- est_doses_hetero
      est_results_list[["est_frac_hetero"]] <- est_frac_hetero
      # Reset Partial
      est_results_list[["est_doses_partial"]] <- NA
      est_results_list[["est_frac_partial"]] <- NA
    }
    # Check if protracted correction was applied
    if (exposure == "protracted" & stringr::str_detect(fit_formula_tex, "beta")) {
      est_results_list[["protraction"]] <- c(1, protracted_time, protracted_life_time)
    }

    return(est_results_list)
  })

  # Results outputs ----
  output$est_yields_whole <- renderRHandsontable({
    # Estimated recieved doses (whole-body)
    if (input$button_estimate <= 0) return(NULL)
    data()[["est_doses_whole"]] %>%
      dplyr::select(yield) %>%
      t() %>%
      as.data.frame() %>%
      # Convert to hot and format table
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_doses_whole <- renderRHandsontable({
    # Estimated recieved doses (whole-body)
    if (input$button_estimate <= 0) return(NULL)
    data()[["est_doses_whole"]] %>%
      dplyr::select(dose) %>%
      t() %>%
      as.data.frame() %>%
      # Convert to hot and format table
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_yields_partial <- renderRHandsontable({
    # Estimated recieved doses (partial)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "partial") return(NULL)
    data()[["est_doses_partial"]] %>%
      dplyr::select(yield) %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`("yield") %>%
      # Convert to hot and format table
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_doses_partial <- renderRHandsontable({
    # Estimated recieved doses (partial)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "partial") return(NULL)
    data()[["est_doses_partial"]] %>%
      dplyr::select(dose) %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`("dose") %>%
      # Convert to hot and format table
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_frac_partial <- renderRHandsontable({
    # Estimated fraction of irradiated blood for dose dose1 (partial)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "partial") return(NULL)
    data()[["est_frac_partial"]] %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`("frac") %>%
      # Convert to hot and format table
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_mixing_prop_hetero <- renderRHandsontable({
    # Estimated yields (heterogeneous)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "hetero") return(NULL)
    data()[["est_mixing_prop_hetero"]] %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("y_estimate", "y_std_err", "f_estimate", "f_std_err")) %>%
      `row.names<-`(c("dose1", "dose2")) %>%
      # Convert to hot and format table
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_yields_hetero <- renderRHandsontable({
    # Estimated recieved doses (heterogeneous)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "hetero") return(NULL)
    data()[["est_yields_hetero"]] %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`(c("yield1", "yield2")) %>%
      # Convert to hot and format table
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_doses_hetero <- renderRHandsontable({
    # Estimated recieved doses (heterogeneous)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "hetero") return(NULL)
    data()[["est_doses_hetero"]] %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`(c("dose1", "dose2")) %>%
      # Convert to hot and format table
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_frac_hetero <- renderRHandsontable({
    # Estimated fraction of irradiated blood for dose dose1 (heterogeneous)
    if (input$button_estimate <= 0 || data()[["assessment"]] != "hetero") return(NULL)
    data()[["est_frac_hetero"]] %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("estimate", "std_err")) %>%
      `row.names<-`(c("dose1", "dose2")) %>%
      # Convert to hot and format table
      rhandsontable(width = "100%", height = "100%") %>%
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
      paste("estimate-curve-", Sys.Date(), input$save_plot_format, sep = "")
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
  output$save_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste("estimate-report-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      # normReport <- file.path("report.Rmd")
      file.copy("reports/dicentrics-estimate-report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        est_results_list = data()
      )

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}
