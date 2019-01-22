# Dose Estimateion Modules ------------------------------------------

estimateUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabItem(
    tabName = label,
    h2("Dose estimation"),
    fluidRow(
      # Data input options ----
      box(
        width = 5,
        title = "Data input options",
        status = "warning", solidHeader = F, collapsible = T,
        fluidRow(
          column(
            width = 12,
            # Load data from file
            awesomeCheckbox(
              inputId = ns("load_count_data_check"),
              label = "Load data from file",
              value = FALSE, status = "warning"
            ),
            # Inputs
            conditionalPanel(
              condition = "!input.load_count_data_check",
              ns = ns,
              numericInput(ns("num_cases"), "Number of cases", value = 1),
              numericInput(ns("num_dicentrics"), "Maximum number of dicentrics per cell", value = 5),
              # Help button
              bsButton(ns("help_input_cases_data"),
                       class = "rightAlign",
                       label = "",
                       icon = icon("question"),
                       style = "default", size = "default"
              ),
              bsModal(
                id = ns("help_input_cases_data_dialog"),
                title = "Help: Count data input",
                trigger = ns("help_input_cases_data"),
                size = "large",
                withMathJax(includeMarkdown("help/input_count_data.md"))
                # TODO: Make new help dialogue
              )
            ),
            conditionalPanel(
              condition = "input.load_count_data_check",
              ns = ns,
              fileInput(ns("load_count_data"), label = "File input"),
              # Help button
              bsButton(ns("help_load_count_data"),
                       class = "rightAlign",
                       label = "",
                       icon = icon("question"),
                       style = "default", size = "default"
              ),
              bsModal(
                id = ns("help_load_count_data_dialog"),
                title = "Help: Loading count data",
                trigger = ns("help_load_count_data"),
                size = "large",
                withMathJax(includeMarkdown("help/load_count_data.md"))
              )
            ),
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
      # Curve fitting options
      box(
        width = 5,
        title = "Curve fitting data options",
        status = "warning", solidHeader = F, collapsible = T,
        p("Work in progress...")
      )
    ),

    # Hot Table ----
    fluidRow(
      box(
        width = 12,
        title = "Data input",
        status = "primary", solidHeader = F, collapsible = T, collapsed = F,
        rHandsontableOutput(ns("hotable")),
        # Button
        br(),
        actionButton(ns("button_check_dist"), "Check distribution"),
        # Help button
        bsButton(ns("help_check_distribution"),
                 # class = "rightAlign",
                 label = "",
                 icon = icon("question"),
                 style = "default", size = "default"
        ),
        div(class = "widget-sep", br()),
        actionButton(ns("button_fit"), class = "inputs-button", "Estimate dose"),
        bsModal(
          id = ns("help_check_distribution_dialog"),
          title = "Help: Count data input",
          trigger = ns("help_check_distribution"),
          size = "large",
          withMathJax(includeMarkdown("help/load_count_data.md"))
          # TODO: Make new help dialogue
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
      load_count_data <- input$load_count_data_check
      count_data <- input$load_count_data
      num_cases <- as.numeric(input$num_cases)
      num_dicentrics <- as.numeric(input$num_dicentrics) + 1
    })

    if (!load_count_data) {
      # Doses data frame
      # data_dose <- data.frame(
      #   D = rep(0.0, num_cases)
      # )

      # Base data frame
      data_base <- data.frame(
        matrix(
          0,
          nrow = num_cases,
          ncol = num_dicentrics
        )
      )

      colnames(data_base) <- paste0("C", seq(0, num_dicentrics - 1, 1))

      # Full data frame
      # full_data <- cbind(data_dose, data_base) %>%
        # dplyr::mutate(D = as.numeric(D))
      full_data <- data_base
    } else {
      full_data <- read.csv(count_data$datapath, header = TRUE) %>%
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
          DI = 0,
          u = 0
        ) %>%
        # select(D, N, X, everything())
        select(N, X, everything())

      first_dicent_index <- 3
      last_dicent_index <- ncol(mytable) - 2
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
      }

      return(mytable)
    }
  })

  # Output ----
  output$hotable <- renderRHandsontable({
    hot <- rhandsontable(
      changed_data()
    )
    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })
}
