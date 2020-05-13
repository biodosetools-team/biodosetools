# Fitting Modules ---------------------------------

dicentFittingUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  bs4Dash::bs4TabItem(
    tabName = label,
    h2("Dicentrics: Dose-effect fitting"),

    fluidRow(
      # Card: Data input options ----
      bs4MyCard(
        width = 6,
        title = "Data input options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

        topButton = div(
          # Help button
          shinyBS::bsButton(
            ns("help_count_data"),
            label = "",
            icon = icon("question"),
            style = "default", size = "default"
          ),

          # Help modal
          bs4MyModal(
            id = ns("help_count_data_dialog"),
            title = "Help: Count data input",
            trigger = ns("help_count_data"),
            size = "large",

            # Option selection
            shinyWidgets::radioGroupButtons(
              inputId = ns("help_count_data_option"),
              label = NULL,
              choices = c(
                "Manual input"    = "manual",
                "Load data"       = "load",
                "Aggregated data" = "aggr"
              )
            ),
            # Contents
            conditionalPanel(
              condition = "input.help_count_data_option == 'manual'",
              ns = ns,
              withMathJax(includeMarkdown("help/fitting/count_data_input.md"))
            ),
            conditionalPanel(
              condition = "input.help_count_data_option == 'load'",
              ns = ns,
              withMathJax(includeMarkdown("help/fitting/count_data_load.md"))
            ),
            conditionalPanel(
              condition = "input.help_count_data_option == 'aggr'",
              ns = ns,
              withMathJax(includeMarkdown("help/fitting/count_data_aggregated.md"))
            )
          )
        ),

        fluidRow(
          column(
            width = 12,
            # Load file checkbox
            mySwitchInput(
              inputId = ns("load_count_data_check"),
              size = "mini",
              onStatus = "options",
              sideLabel = "Load data from file",
              value = FALSE
            ),

            # Full/aggregated data checkbox
            mySwitchInput(
              inputId = ns("use_aggr_count_data_check"),
              size = "mini",
              onStatus = "options",
              width = "100%",
              sideLabel = "Only provide total number of dicentrics",
              value = FALSE
            ),

            # Manual input ----
            conditionalPanel(
              condition = "!input.load_count_data_check",
              ns = ns,
              numericInput(ns("num_doses"), "Number of doses", value = 11)
            ),
            conditionalPanel(
              condition = "!input.load_count_data_check & !input.use_aggr_count_data_check",
              ns = ns,
              numericInput(ns("num_aberrs"), "Maximum number of dicentrics per cell", value = 5)
            ),
            # Load from file ----
            conditionalPanel(
              condition = "input.load_count_data_check",
              ns = ns,
              fileInput(
                ns("load_count_data"),
                label = "File input",
                accept = c("txt/csv", "text/comma-separated-values", "text/plain", ".csv", ".txt", ".dat")
              )
            ),
            # Buttons
            actionButton(ns("button_upd_table"), class = "options-button", "Generate table")
          )
        )
      ),

      # Card: Fitting options ----
      bs4MyCard(
        width = 6,
        title = "Fitting options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

        topButton = div(
          # Help button
          shinyBS::bsButton(
            ns("help_fitting_options"),
            label = "",
            icon = icon("question"),
            style = "default", size = "default"
          ),

          # Help Modal
          bs4MyModal(
            id = ns("help_fitting_options_dialog"),
            title = "Help: Fitting options",
            trigger = ns("help_fitting_options"),
            size = "large",

            # Option selection
            shinyWidgets::radioGroupButtons(
              inputId = ns("help_fitting_options_option"),
              label = NULL,
              choices = c(
                "Fitting formula"  = "formula",
                "Fitting model"    = "model"#,
                # "Decision thresholds" = "decision_thresholds"
              )
            ),
            # Contents
            conditionalPanel(
              condition = "input.help_fitting_options_option == 'formula'",
              ns = ns,
              withMathJax(includeMarkdown("help/fitting/fitting_options_formula.md"))
            ),
            conditionalPanel(
              condition = "input.help_fitting_options_option == 'model'",
              ns = ns,
              withMathJax(includeMarkdown("help/fitting/fitting_options_model.md"))
            )#,
            # conditionalPanel(
            #   condition = "input.help_fitting_options_option == 'decision_thresholds'",
            #   ns = ns,
            #   withMathJax(includeMarkdown("help/fitting/fitting_options_decision_thresholds.md"))
            # )
          )
        ),

        fluidRow(
          column(
            width = 12,
            # Fitting formula
            selectInput(
              ns("formula_select"),
              label = "Fitting formula",
              choices = global_fitting_formulas,
              selected = "lin-quad"
            ),
            # Fitting model
            selectInput(
              ns("family_select"),
              label = "Fitting model",
              choices = list(
                "Automatic" = "automatic",
                "Poisson" = "poisson",
                "Quasipoisson" = "quasipoisson"
              ),
              selected = "automatic"
            )#,
            # Decision thresholds
            # textInput(ns("decision_thresh_cells"), "Cells for decision thresholds", value = "150 500 1000")
          )
        )
      )
    ),

    # Card: hot Count data input ----
    fluidRow(
      bs4MyCard(
        width = 12,
        title = "Data input",
        status = "inputs", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
        div(
          class = "hot-improved",
          rhandsontable::rHandsontableOutput(ns("count_data_hot"))
        ),
        # Buttons
        br(),
        div(
          style = "display: inline-block;",
          conditionalPanel(
            condition = "!input.use_aggr_count_data_check",
            ns = ns,
            actionButton(ns("button_upd_params"), class = "inputs-button", "Calculate parameters"),
            div(class = "widget-sep", br())
          )
        ),
        downloadButton(ns("save_count_data"), class = "side-widget", "Save count data"),
        div(
          class = "side-widget-tall",
          selectInput(
            ns("save_count_data_format"),
            label = NULL,
            width = "85px",
            choices = list(".csv", ".tex"),
            selected = ".csv"
          )
        ),
        div(class = "widget-sep", br()),
        actionButton(ns("button_fit"), class = "inputs-button", "Calculate fitting")
      )
    ),


    fluidRow(
      column(
        width = 6,
        # tabCard: Fit results ----
        div(
          # Ugly fix for inner fluidRow() padding
          style = "margin-left: -7.5px; margin-right: -7.5px",
          bs4Dash::bs4TabCard(
            id = ns("fit_results_tabs"),
            width = 12,
            side = "left",
            solidHeader = TRUE,
            closable = FALSE,

            bs4MyTabPanel(
              tabName = "Result of curve fit",
              active = TRUE,
              h6("Fit formula"),
              uiOutput(ns("fit_formula_tex")),

              h6("Model"),
              uiOutput(ns("fit_model_summary")),

              br(),
              h6("Coefficients"),
              div(
                class = "hot-improved",
                rhandsontable::rHandsontableOutput(ns("fit_coeffs"))
              )
            ),
            bs4MyTabPanel(
              tabName = "Summary statistics",
              h6("Model-level statistics"),
              div(
                class = "hot-improved",
                rhandsontable::rHandsontableOutput(ns("fit_model_statistics"))
              ),

              br(),
              h6("Correlation matrix"),
              div(
                class = "hot-improved",
                rhandsontable::rHandsontableOutput(ns("fit_cor_mat"))
              ),

              br(),
              h6("Variance-covariance matrix"),
              div(
                class = "hot-improved",
                rhandsontable::rHandsontableOutput(ns("fit_var_cov_mat"))
              )
            )#,
            # bs4MyTabPanel(
            #   tabName = "Decision thresholds",
            #   h6("Decision thresholds"),
            #   div(
            #     class = "hot-improved",
            #     rhandsontable::rHandsontableOutput(ns("fit_decision_thresh"))
            #   )
            # )
          )
        ),

        # Card: Export data and results ----
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Export results",
          status = "export", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

          topButton = div(
            # Help button
            shinyBS::bsButton(
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
              shinyWidgets::radioGroupButtons(
                inputId = ns("help_fit_data_save_option"),
                label = NULL,
                choices = c(
                  "Fitting data" = "data",
                  "Report"       = "report"
                )
              ),
              # Contents
              conditionalPanel(
                condition = "input.help_fit_data_save_option == 'data'",
                ns = ns,
                withMathJax(includeMarkdown("help/save/fit_data_save.md"))
              ),
              conditionalPanel(
                condition = "input.help_fit_data_save_option == 'report'",
                ns = ns,
                withMathJax(includeMarkdown("help/save/fit_data_save_report.md"))
              )
            )
          ),

          # Download fit data & report
          downloadButton(ns("save_fit_data"), class = "side-widget", "Save fitting data"),
          div(
            class = "side-widget-tall",
            selectInput(
              ns("save_fit_data_format"),
              label = NULL,
              width = "85px",
              choices = list(".rds"),
              selected = ".rds"
            )
          ),
          # Download report
          div(class = "widget-sep", br()),
          downloadButton(ns("save_report"), class = "export-button", "Download report"),
          div(
            class = "side-widget-tall",
            selectInput(
              ns("save_report_format"),
              label = NULL,
              width = "85px",
              choices = list(".html", ".docx"),
              selected = ".html"
            )
          )

        )
      ),
      column(
        width = 6,
        # Card: Plot box ----
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
