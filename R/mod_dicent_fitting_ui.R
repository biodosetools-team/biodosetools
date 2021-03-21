#' Dicentrics Fitting UI Module
#'
#' @param id Namespace for the {shiny} module.
#' @param label Internal label of the {shiny} module.
#'
#' @import shiny shinydashboard shinyWidgets rhandsontable
#' @noRd
mod_dicent_fitting_ui <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabItem(
    tabName = label,
    h2("Dicentrics: Dose-effect fitting"),

    fluidRow(
      # Box: Data input options ----
      box(
        width = 6,
        title = span(
          "Data input options",
          help_modal_button(
            ns("help_count_data"),
            ns("help_count_data_modal")
          )
        ),
        status = "info",
        collapsible = TRUE,

        # Help modal
        bsplus::bs_modal(
          id = ns("help_count_data_modal"),
          title = "Help: Count data input",
          size = "large",

          body = tagList(
            # Option selection
            radioGroupButtons(
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
              include_help("fitting/count_data_input.md")
            ),
            conditionalPanel(
              condition = "input.help_count_data_option == 'load'",
              ns = ns,
              include_help("fitting/count_data_load.md")
            ),
            conditionalPanel(
              condition = "input.help_count_data_option == 'aggr'",
              ns = ns,
              include_help("fitting/count_data_aggregated.md")
            )
          )
        ),

        fluidRow(
          col_12(
            # Load file checkbox
            awesomeCheckbox(
              inputId = ns("load_count_data_check"),
              status = "info",
              label = "Load data from file",
              value = FALSE
            ),

            # Full/aggregated data checkbox
            awesomeCheckbox(
              inputId = ns("use_aggr_count_data_check"),
              status = "info",
              width = "100%",
              label = "Only provide total number of dicentrics",
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

      # Box: Fitting options ----
      box(
        width = 6,
        title = span(
          "Fitting options",
          help_modal_button(
            ns("help_fitting_options"),
            ns("help_fitting_options_modal")
          )
        ),
        status = "info",
        collapsible = TRUE,

        # Help modal
        bsplus::bs_modal(
          id = ns("help_fitting_options_modal"),
          title = "Help: Fitting options",
          size = "large",

          body = tagList(
            # Option selection
            radioGroupButtons(
              inputId = ns("help_fitting_options_option"),
              label = NULL,
              choices = c(
                "Fitting formula"  = "formula",
                "Fitting model"    = "model" # ,
                # "Decision thresholds" = "decision_thresholds"
              )
            ),
            # Contents
            conditionalPanel(
              condition = "input.help_fitting_options_option == 'formula'",
              ns = ns,
              include_help("fitting/fitting_options_formula.md")
            ),
            conditionalPanel(
              condition = "input.help_fitting_options_option == 'model'",
              ns = ns,
              include_help("fitting/fitting_options_model.md")
            ) # ,
            # conditionalPanel(
            #   condition = "input.help_fitting_options_option == 'decision_thresholds'",
            #   ns = ns,
            #   include_help("fitting/fitting_options_decision_thresholds.md")
            # )
          )
        ),

        fluidRow(
          col_12(
            # Fitting formula
            selectInput(
              ns("formula_select"),
              label = "Fitting formula",
              choices = list_fitting_formulas(),
              selected = "lin-quad"
            ),
            # Fitting model
            selectInput(
              ns("family_select"),
              label = "Fitting model",
              choices = list(
                "Automatic" = "automatic",
                "Poisson" = "poisson",
                "Quasi-Poisson" = "quasipoisson"
              ),
              selected = "automatic"
            ) # ,
            # Decision thresholds
            # textInput(ns("decision_threshold_cells"), "Cells for decision thresholds", value = "150 500 1000")
          )
        )
      )
    ),

    # Box: hot Count data input ----
    fluidRow(
      box(
        width = 12,
        title = "Data input",
        status = "primary",
        collapsible = TRUE,

        div(
          class = "hot-improved",
          rHandsontableOutput(ns("count_data_hot"))
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
      col_6(
        # tabBox: Fit results ----
        tabBox(
          id = ns("fit_results_tabs"),
          width = 12,
          side = "left",

          tabPanel(
            title = "Result of curve fit",
            h5("Fit formula"),
            uiOutput(ns("fit_formula_tex")),

            h5("Model"),
            uiOutput(ns("fit_model_summary")),

            br(),
            h5("Coefficients"),
            div(
              class = "hot-improved",
              rHandsontableOutput(ns("fit_coeffs"))
            )
          ),

          tabPanel(
            title = "Summary statistics",
            h5("Model-level statistics"),
            div(
              class = "hot-improved",
              rHandsontableOutput(ns("fit_model_statistics"))
            ),

            br(),
            h5("Correlation matrix"),
            div(
              class = "hot-improved",
              rHandsontableOutput(ns("fit_cor_mat"))
            ),

            br(),
            h5("Variance-covariance matrix"),
            div(
              class = "hot-improved",
              rHandsontableOutput(ns("fit_var_cov_mat"))
            )
          ) # ,

          # tabPanel(
          #   title = "Decision thresholds",
          #   h5("Decision thresholds"),
          #   div(
          #     class = "hot-improved",
          #     rHandsontableOutput(ns("fit_decision_thresh"))
          #   )
          # )
        ),

        # Box: Export data and results ----
        box(
          width = 12,
          title = span(
            "Export results",
            help_modal_button(
              ns("help_fit_data_save"),
              ns("help_fit_data_save_modal")
            )
          ),
          status = "warning",
          collapsible = TRUE,

          # Help modal
          bsplus::bs_modal(
            id = ns("help_fit_data_save_modal"),
            title = "Help: Export results",
            size = "large",

            body = tagList(
              # Option selection
              radioGroupButtons(
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
                include_help("save/fit_data_save.md")
              ),
              conditionalPanel(
                condition = "input.help_fit_data_save_option == 'report'",
                ns = ns,
                include_help("save/fit_data_save_report.md")
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
              choices = list(".pdf", ".html", ".docx"),
              selected = ".pdf"
            )
          )
        )
      ),
      col_6(
        # Box: Plot box ----
        box(
          width = 12,
          title = "Curve plot",
          status = "success",
          collapsible = TRUE,

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
