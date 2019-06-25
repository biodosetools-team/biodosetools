# Dose Estimation Modules -------------------------------------------

transEstimateUI <- function(id, label) { #, locale = i18n) {
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

        topButton = div(
          # Help button
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
                  choices = global_fitting_formulas,
                  selected = "lin-quad"
                )
              ),
              div(class = "widget-sep", br()),
              actionButton(ns("button_gen_table"), class = "options-button", style = "margin-left: -10px; margin-bottom: 2px;", "Generate tables"),

              br(),
              br(),
              selectInput(
                ns("frequency_select"),
                label = "Translocation frequency",
                choices = list(
                  "Measured by FISH" = "measured_freq",
                  "Full genome"      = "full_gen_freq"
                ),
                selected = "measured_freq"
              )
            ),

            conditionalPanel(
              condition = "!input.load_fit_data_check & input.frequency_select == 'measured_freq'",
              ns = ns,

              numericInput(
                ns("fit_fraction_value"),
                "Genomic conversion factor",
                value = NA,
                min = 0,
                max = 1,
                step = 0.001
              )
            ),

            conditionalPanel(
              condition = "!input.load_fit_data_check",
              ns = ns,

              widgetLabel("Coefficients"),
              rHandsontableOutput(ns("fit_coeffs_hot")),

              br(),
              widgetLabel("Variance-covariance matrix"),
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
        )
      ),
      # tabCard: Curve fitting overview ----

      bs4MyTabCard(
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

          br(),
          h6("Translocation frequency"),
          uiOutput(ns("fit_trans_frequency_message")),

          br(),
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

            withMathJax(includeMarkdown("help/help_colors_data_input.md"))

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
                awesomeCheckbox(
                  inputId = ns("trans_m_fish_scheme"),
                  status = "warning",
                  label = "Use M-Fish",
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
                      "Orange",
                      "Purple",
                      "Yellow",
                      "Cyan",
                      "Magenta"
                    ),
                    options = list(
                      placeholder = 'Select used colors'#,
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

              rHandsontableOutput(outputId = ns("chromosome_table"))
            ),
            div(
              style = "padding-left: 7.5px; padding-top: 23px;",
              actionButton(ns("button_calc_fraction"), class = "inputs-button", "Calculate fraction")
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

              uiOutput(ns("fraction"))

            )
          )
        )

      )
    ),

    fluidRow(
      # Card: Data input options ----
      bs4MyCard(
        width = 5,
        title = "Data input options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

        topButton = div(
          # Help button
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
              # numericInput(ns("num_cases"), "Number of cases", value = 1),
              numericInput(ns("num_aberrs"), "Maximum number of translocations per cell", value = 5)
            ),
            conditionalPanel(
              condition = "input.load_case_data_check",
              ns = ns,
              fileInput(ns("load_case_data"), label = "File input", accept = c("txt/csv", "text/comma-separated-values", "text/plain", ".csv", ".txt", ".dat"))
            ),

            # Confounders selection
            widgetLabel("Confounders"),
            awesomeCheckbox(
              inputId = ns("trans_confounders"),
              status = "warning",
              label = "Use confounders",
              value = FALSE
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
        )
      ),
      # Card: hot Cases & confounders input ----
      column(
        width = 7,
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Data input",
          status = "inputs", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

          # Confounders
          conditionalPanel(
            condition = "input.trans_confounders",
            ns = ns,
            div(
              class = "side-widget",
              style = "padding-right: 10px;",

              numericInput(
                ns("trans_confounder_age"),
                width = 75,
                "Age",
                value = 25,
                min = 0,
                step = 1
              )
            ),

            div(
              class = "side-widget",
              style = "max-width: 140px; margin-right: -40px;",
              widgetLabel("Sex", 14),
              switchInput(
                ns("trans_confounder_sex"),
                value = FALSE
              )
            ),

            div(
              class = "side-widget",
              style = "max-width: 140px; margin-right: -40px;",
              widgetLabel("Smoking", 14),
              switchInput(
                ns("trans_confounder_smoke"),
                value = FALSE
              )
            ),

            div(
              class = "side-widget",
              style = "padding-right: 10px;",
              selectInput(
                ns("trans_confounder_race"),
                label = "Race",
                width = "150px",
                choices = list(
                  "Races" = c(
                    "White"  = "white",
                    "Asian"  = "asian",
                    "Black"  = "black",
                    "Others" = "other"
                  ),
                  "None" = c(
                    "Not specified" = "none"
                  )
                ),
                selected = "none"
              )
            ),

            div(
              class = "side-widget",
              style = "padding-right: 10px;",
              selectInput(
                ns("trans_confounder_region"),
                label = "Lab region",
                width = "180px",
                choices = list(
                  "Regions" = c(
                    "North America"  = "n-america",
                    "Western Europe" = "w-europe",
                    "Central Europe" = "c-europe",
                    "Eastern Europe" = "e-europe",
                    "Asia"           = "asia"
                  ),
                  "None" = c(
                    "Not specified"  = "none"
                  )
                ),
                selected = "none"
              )
            )
          ),


          # div(
          #   class = "side-widget",
          #   style = "padding-right: 10px;",
          #   conditionalPanel(
          #     condition = "input.trans_confounders.includes('age')",
          #     ns = ns,
          #     numericInput(
          #       ns("trans_confounder_age"),
          #       width = 75,
          #       "Age",
          #       value = 25,
          #       min = 0,
          #       step = 1
          #     )
          #   )
          # ),
          #
          # div(
          #   class = "side-widget",
          #   style = "max-width: 140px; margin-right: -40px;",
          #   conditionalPanel(
          #     condition = "input.trans_confounders.includes('sex')",
          #     ns = ns,
          #     widgetLabel("Sex", 14),
          #     switchInput(
          #       ns("trans_confounder_sex"),
          #       value = TRUE
          #     )
          #   )
          # ),
          #
          # div(
          #   class = "side-widget",
          #   style = "max-width: 140px; margin-right: -40px;",
          #   conditionalPanel(
          #     condition = "input.trans_confounders.includes('smoke')",
          #     ns = ns,
          #     widgetLabel("Smoking", 14),
          #     switchInput(
          #       ns("trans_confounder_smoke"),
          #       value = TRUE
          #     )
          #   )
          # ),
          #
          # div(
          #   class = "side-widget",
          #   style = "padding-right: 10px;",
          #   conditionalPanel(
          #     condition = "input.trans_confounders.includes('race')",
          #     ns = ns,
          #     selectInput(
          #       ns("trans_confounder_race"),
          #       label = "Race",
          #       width = "100px",
          #       choices = list(
          #         "White"  = "white",
          #         "Asian"  = "asian",
          #         "Black"  = "black",
          #         "Others" = "other"
          #       ),
          #       selected = "white"
          #     )
          #   )
          # ),
          #
          # div(
          #   class = "side-widget",
          #   style = "padding-right: 10px;",
          #   conditionalPanel(
          #     condition = "input.trans_confounders.includes('region')",
          #     ns = ns,
          #     selectInput(
          #       ns("trans_confounder_region"),
          #       label = "Lab region",
          #       width = "180px",
          #       choices = list(
          #         "North America"  = "n-america",
          #         "Asia"           = "asia",
          #         "Western Europe" = "w-europe",
          #         "Central Europe" = "c-europe",
          #         "Eastern Europe" = "e-europe"
          #       ),
          #       selected = "w-europe"
          #     )
          #   )
          # ),

          # Cases table
          div(
            style="height = auto;",
            rHandsontableOutput(ns("case_data_hot"))
          ),
          # Button
          br(),
          actionButton(ns("button_upd_params"), class = "inputs-button", "Calculate parameters")
        ),

        # Card: Estimation options ----
        bs4MyCard(
          width = 12,
          title = "Dose estimation options",
          status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,

          topButton = div(
            # Help button
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
                  "Exposure"             = "exposure",
                  "Assessment"           = "assess",
                  "Error calculation"    = "error",
                  "Survival coefficient" = "surv_coeff"
                )

              ),
              # Contents
              conditionalPanel(
                condition = "input.help_estimate_options_option == 'exposure'",
                ns = ns,
                withMathJax(includeMarkdown("help/help_dose_exposure.md"))
              ),
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
          ),

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
                "Whole-body"    = "whole-body",
                "Partial-body"  = "partial-body",
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
              condition = "input.assessment_select != 'partial-body'",
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
              condition = "input.assessment_select == 'partial-body'",
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

          actionButton(ns("button_estimate"), class = "options-button", "Estimate dose")
        )
      )
    ),

    fluidRow(
      column(
        width = 6,
        # tabCard: Estimation results ----
        uiOutput(ns("estimate_results_ui")),

        # Card: Export data and results ----
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Save results",
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
          ),

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
          downloadButton(ns("save_report"), class = "export-button", "Download report")
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
