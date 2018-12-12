# Dose Estimateion Modules ------------------------------------------

estimateUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabItem(
    tabName = label,
    h2("Dose Estimation"),
    fluidRow(
      # Input and data boxes ----
      column(
        width = 6,
        box(
          width = 12,
          title = "Load Data",
          status = "primary", solidHeader = F, collapsible = T,

          # Local data slider
          materialSwitch(
            inputId = ns("local_data"),
            label = "Load my own data:",
            status = "primary",
            value = TRUE,
            right = FALSE
          ),

          # Input
          conditionalPanel(
            condition = "input.local_data",
            ns = ns,
            fileInput(ns("file"), label = "File input")
          ),

          # Tooltips
          bsTooltip(ns("local_data"), "List of doses",
            "right",
            options = list(container = "body")
          ),
          bsTooltip(ns("file"), "File selection",
            "right",
            options = list(container = "body")
          ),

          # Button
          actionButton(ns("button_load"), "Use data")
        )
      )#,
      # Results tabBox ----
      # column(
      #   width = 8,
      #   tabBox(
      #     width = 12,
      #     side = "left",
      #     # selected = "Tab3",
      #     tabPanel("Result of curve fit", verbatimTextOutput(ns("result"))),
      #     tabPanel("Coefficients", verbatimTextOutput(ns("bstat"))),
      #     tabPanel("Variance-covariance matrix", verbatimTextOutput(ns("vakoma"))),
      #     tabPanel("Correlation matrix", verbatimTextOutput(ns("corma")))
      #   )
      # )
    )
  )
}

