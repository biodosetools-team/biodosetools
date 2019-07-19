# Fitting Modules ---------------------------------

transFittingUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  bs4TabItem(
    tabName = label,
    h2("Translocations: Dose-effect fitting"),

    fluidRow(
      # Card: Stains color options ----
      bs4MyCard(
        width = 6,
        title = "Stains color options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

        topButton = div(
          # Help button
          bsButton(
            ns("help_colors"),
            label = "",
            icon = icon("question"),
            style = "default", size = "default"
          ),

          # Help modal
          bs4MyModal(
            id = ns("help_colors_dialog"),
            title = "Help: Stain color data input",
            trigger = ns("help_colors"),
            size = "large",

            withMathJax(includeMarkdown("help/help_colors_data_input.md")),
            div(
              class = "hot-improved",
              rHandsontableOutput(ns("help_chromosome_hot"))
            ),
            withMathJax(includeMarkdown("help/help_colors_data_input_b.md"))

          )
        ),

        fluidRow(
          column(
            width = 12,

            fluidRow(
              innerColumn(
                width = 6,

                awesomeRadio(
                  inputId = ns("trans_sex"),
                  status = "warning",
                  label = "Sex",
                  choices = c(
                    "Male"   = "male",
                    "Female" = "female"
                  ),
                  selected = "male"
                ),

                selectizeInput(
                  inputId = ns("trans_chromosome_select"),
                  label = "Chromosomes",
                  choices = c( 1:21, "X", "Y"),
                  options = list(
                    placeholder = 'Select stained chromosomes'
                  ),
                  multiple = TRUE
                )
              ),

              innerColumn(
                width = 6,

                widgetLabel("Stain color scheme"),
                mySwitchInput(
                  inputId = ns("trans_m_fish_scheme"),
                  size = "mini",
                  onStatus = "options",
                  sideLabel = "Use M-Fish",
                  value = FALSE
                ),

                conditionalPanel(
                  condition = "!input.trans_m_fish_scheme",
                  ns = ns,
                  selectizeInput(
                    inputId = ns("trans_color_select"),
                    label = "Stain colors",
                    choices = c(
                      "Red",
                      "Green",
                      "Yellow",
                      "Orange",
                      "Purple",
                      "Magenta",
                      "Cyan"
                    ),
                    options = list(
                      placeholder = 'Select observed colors'#,
                      # maxItems = 5
                      # TODO: use renderUI to force maxItems ot be length(trans_color_select)
                    ),
                    multiple = TRUE
                  )
                )
              )
            ),

            br(),
            actionButton(ns("button_upd_chrom_table"), class = "options-button", "Generate table")
          )
        )
      ),

      column(
        width = 6,

        # Card: Chromosome-color table ----
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Chromosome data",
          status = "inputs", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
          fluidRow(
            column(
              width = 12,

              div(
                class = "hot-improved",
                rHandsontableOutput(outputId = ns("chromosome_table"))
              )
            ),
            div(
              style = "padding-left: 7.5px; padding-top: 23px;",
              actionButton(ns("button_calc_genome_fraction"), class = "inputs-button", "Calculate fraction")
            )
          )
        ),

        # Card: Conversion factor to full genome ----
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Genomic conversion factor",
          status = "results", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
          fluidRow(
            column(
              width = 12,

              uiOutput(ns("genome_fraction"))

            )
          )
        )

      )
    ),

    fluidRow(
      # Card: Data input options ----
      bs4MyCard(
        width = 6,
        title = "Data input options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

        topButton = div(
          # Help button
          bsButton(
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
              withMathJax(includeMarkdown("help/help_count_data_input.md"))
            ),
            conditionalPanel(
              condition = "input.help_count_data_option == 'load'",
              ns = ns,
              withMathJax(includeMarkdown("help/help_count_data_load.md"))
            ),
            conditionalPanel(
              condition = "input.help_count_data_option == 'aggr'",
              ns = ns,
              withMathJax(includeMarkdown("help/help_count_data_aggregated.md"))
            )
          )
        ),

        fluidRow(
          column(
            width = 12,

            # Name of translocations
            textInput(
              inputId = ns("trans_name"),
              label = "Name of translocations",
              placeholder = "Input type of translocations"
            ),

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
              sideLabel = "Only provide total number of translocations",
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
              numericInput(ns("num_aberrs"), "Maximum number of translocations per cell", value = 5)
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
          bsButton(
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
            radioGroupButtons(
              inputId = ns("help_fitting_options_option"),
              label = NULL,
              choices = c(
                "Fitting formula" = "formula",
                "Fitting model"   = "model"
              )
            ),
            # Contents
            conditionalPanel(
              condition = "input.help_fitting_options_option == 'formula'",
              ns = ns,
              withMathJax(includeMarkdown("help/help_fitting_options_formula.md"))
            ),
            conditionalPanel(
              condition = "input.help_fitting_options_option == 'model'",
              ns = ns,
              withMathJax(includeMarkdown("help/help_fitting_options_model.md"))
            )
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
            ),
            # Detection limits
            widgetLabel("Detection limits"),
            textInput(ns("detection_lims_cells"), "Number of cells", value = "150 500 1000")
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
        div(style = "height: 10px;", br()),
        actionButton(ns("button_fit"), class = "inputs-button", "Calculate fitting"),
        div(
          class = "side-widget-tall",
          # Translocation frequency
          selectInput(
            ns("frequency_select"),
            # label = "Translocation frequency",
            label = NULL,
            width = "180px",
            choices = list(
              "Measured by FISH" = "measured_freq",
              "Full genome"      = "full_gen_freq"
            ),
            selected = "measured_freq"
          )
        )
      )
    ),


    fluidRow(
      column(
        width = 6,
        # tabCard: Fit results ----
        div(
          # Ugly fix for inner fluidRow() padding
          style = "margin-left: -7.5px; margin-right: -7.5px",
          bs4TabCard(
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
                rHandsontableOutput(ns("fit_coeffs"))
              )
            ),
            bs4MyTabPanel(
              tabName = "Summary statistics",
              h6("Model-level statistics"),
              div(
                class = "hot-improved",
                rHandsontableOutput(ns("fit_model_statistics"))
              ),

              br(),
              h6("Correlation matrix"),
              div(
                class = "hot-improved",
                rHandsontableOutput(ns("fit_cor_mat"))
              ),

              br(),
              h6("Variance-covariance matrix"),
              div(
                class = "hot-improved",
                rHandsontableOutput(ns("fit_var_cov_mat"))
              )
            ),
            bs4MyTabPanel(
              tabName = "Detection limits",
              h6("Detection limits"),
              div(
                class = "hot-improved",
                rHandsontableOutput(ns("fit_detection_lims"))
              )
            )
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
              radioGroupButtons(
                inputId = ns("help_fit_data_save_option"),
                label = NULL,
                choices = c(
                  "Fitting results" = "data",
                  "Report"          = "report"
                )
              ),
              # Contents
              conditionalPanel(
                condition = "input.help_fit_data_save_option == 'data'",
                ns = ns,
                withMathJax(includeMarkdown("help/help_fit_data_save.md"))
              ),
              conditionalPanel(
                condition = "input.help_fit_data_save_option == 'report'",
                ns = ns,
                withMathJax(includeMarkdown("help/help_fit_data_save_report.md"))
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
          downloadButton(ns("save_report"), class = "export-button", "Download report")

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
