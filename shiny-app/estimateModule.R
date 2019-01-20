# Dose Estimateion Modules ------------------------------------------

estimateUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabItem(
    tabName = label,
    h2("Dose estimation"),
    fluidRow(
      # Input and data boxes ----
      box(
        width = 6,
        title = "Load data",
        status = "warning", solidHeader = F, collapsible = T,

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
      ),

      # Data input options ----
      box(
        width = 4,
        title = "Data input options",
        status = "warning", solidHeader = F, collapsible = T,
        fluidRow(
          column(
            width = 12,
            # Inputs
            numericInput(ns("num.doses"), "Number of cases", value = 1),
            numericInput(ns("num.dicentrics"), "Maximum number of dicentrics per cell", value = 5),
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
      )
    ),

    # Hot Table ----
    fluidRow(
      box(
        width = 12,
        title = "Data input",
        status = "primary", solidHeader = F, collapsible = T, collapsed = F,
        rHandsontableOutput(ns("hotable"))
      )
    )
  )
}
