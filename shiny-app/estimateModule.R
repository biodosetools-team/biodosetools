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
              inputId = ns("load_cases_data_check"),
              label = "Load data from file",
              value = FALSE, status = "warning"
            ),
            # Inputs
            conditionalPanel(
              condition = "!input.load_cases_data_check",
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
                title = "Help: Cases data input",
                trigger = ns("help_input_cases_data"),
                size = "large",
                withMathJax(includeMarkdown("help/input_cases_data.md"))
              )
            ),
            conditionalPanel(
              condition = "input.load_cases_data_check",
              ns = ns,
              fileInput(ns("load_cases_data"), label = "File input"),
              # Help button
              bsButton(ns("help_load_cases_data"),
                       class = "rightAlign",
                       label = "",
                       icon = icon("question"),
                       style = "default", size = "default"
              ),
              bsModal(
                id = ns("help_load_cases_data_dialog"),
                title = "Help: Loading cases data",
                trigger = ns("help_load_cases_data"),
                size = "large",
                withMathJax(includeMarkdown("help/load_cases_data.md"))
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
      # Hot Table ----
      box(
        width = 7,
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
          title = "Help: Check distribution",
          trigger = ns("help_check_distribution"),
          size = "large",
          withMathJax(includeMarkdown("help/check_distribution.md"))
          # TODO: Make new help dialogue
        )
      )

    ),


    fluidRow(
      # Curve fitting options ----
      box(
        width = 5,
        title = "Curve fitting data options",
        status = "warning", solidHeader = F, collapsible = T,
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
              p("Work in progress"),
              # TODO: fit data input
              # Help button
              bsButton(ns("help_input_fit_data"),
                       class = "rightAlign",
                       label = "",
                       icon = icon("question"),
                       style = "default", size = "default"
              ),
              bsModal(
                id = ns("help_input_fit_data_dialog"),
                title = "Help: Manual fit data input",
                trigger = ns("help_input_fit_data"),
                size = "large",
                withMathJax(includeMarkdown("help/input_fit_data.md"))
              )
            ),
            # Load from file ----
            conditionalPanel(
              condition = "input.load_fit_data_check",
              ns = ns,
              fileInput(ns("load_fit_data"), label = "File input"),
              # Help button
              bsButton(ns("help_load_fit_data"),
                       class = "rightAlign",
                       label = "",
                       icon = icon("question"),
                       style = "default", size = "default"
              ),
              bsModal(
                id = ns("help_load_fit_data_dialog"),
                title = "Help: Loading fit data",
                trigger = ns("help_load_fit_data"),
                size = "large",
                withMathJax(includeMarkdown("help/load_fit_data.md"))
              )
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
      # Curve fitting overview ----
      # box(
      #   width = 5,
      #   title = "Curve fitting overview",
      #   status = "success", solidHeader = F, collapsible = T,
      #   p("Work in progress...")
      # )
      tabBox(
        width = 7,
        side = "left",
        tabPanel(
          title = "Result of curve fit",
          h4("Fit summary"),
          verbatimTextOutput(ns("fit_results")),
          h4("Coefficients"),
          rHandsontableOutput(ns("fit_coeffs"))
        ),
        tabPanel(
          title = "Summary statistics",
          h4("Correlation matrix"),
          rHandsontableOutput(ns("cor_mat")),
          h4("Variance-covariance matrix"),
          rHandsontableOutput(ns("var_cov_mat"))
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

    # # Generalized curves
    # yield_fun <- function(x) {
    #   general_fit_coeffs[[1]] +
    #     general_fit_coeffs[[2]] * x +
    #     general_fit_coeffs[[3]] * x * x
    # }
    #
    # chisq_df <- nrow(fit_coeffs)
    # R_factor <- sqrt(qchisq(.95, df = chisq_df))
    #
    # yield_error_fun <- function(x) {
    #   sqrt(
    #     general_var_cov_mat[["x0", "x0"]] +
    #       general_var_cov_mat[["x1", "x1"]] * x * x +
    #       general_var_cov_mat[["x2", "x2"]] * x * x * x * x +
    #       2 * general_var_cov_mat[["x0", "x1"]] * x +
    #       2 * general_var_cov_mat[["x0", "x2"]] * x * x +
    #       2 * general_var_cov_mat[["x1", "x2"]] * x * x * x
    #   )
    # }
    #
    # # Plot data
    # plot_data <- broom::augment(fit_results)
    #
    # curves_data <- data.frame(dose = seq(0, max(x1 / x0), length.out = 100)) %>%
    #   mutate(
    #     yield = yield_fun(dose),
    #     yield_low = yield_fun(dose) - R_factor * yield_error_fun(dose),
    #     yield_upp = yield_fun(dose) + R_factor * yield_error_fun(dose)
    #   )
    #
    # # Make plot
    # gg_curve <- ggplot(plot_data / x0) +
    #   # Observed data
    #   geom_point(aes(x = x1, y = aberr)) +
    #   # Fitted curve
    #   stat_function(
    #     data = data.frame(x = c(0, max(x1 / x0))),
    #     mapping = aes(x),
    #     fun = function(x) yield_fun(x),
    #     linetype = "dashed"
    #   ) +
    #   # Confidence bands (Merkle, 1983)
    #   geom_ribbon(data = curves_data, aes(x = dose, ymin = yield_low, ymax = yield_upp), alpha = 0.25) +
    #   labs(x = "Dose (Gy)", y = "Aberrations / Cells") +
    #   theme_bw()

    # Make list of results to return
    results_list <- list(
      fit_results = fit_results,
      fit_coeffs = fit_coeffs,
      var_cov_mat = var_cov_mat,
      cor_mat = cor_mat
      # gg_curve = gg_curve
    )

    return(results_list)
  })

  # Results outputs ----
  output$fit_results <- renderPrint({
    # "Result of curve fit 'fit_results'"
    if(input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_results"]]
  })

  output$fit_coeffs <- renderRHandsontable({
    # "Coefficients 'fit_coeffs'"
    if(input$button_view_fit_data <= 0) return(NULL)
    rhandsontable(data()[["fit_coeffs"]])
  })

  output$var_cov_mat <- renderRHandsontable({
    # "variance-covariance matrix 'var_cov_mat'"
    if(input$button_view_fit_data <= 0) return(NULL)
    rhandsontable(data()[["var_cov_mat"]])
  })

  output$cor_mat <- renderRHandsontable({
    # "Correlation matrix 'cor_mat'"
    if(input$button_view_fit_data <= 0) return(NULL)
    rhandsontable(data()[["cor_mat"]])
  })

  # output$plot <- renderPlot(
  #   res = 120, {
  #     if(input$button_view_fit_data <= 0) return(NULL)
  #     data()[["gg_curve"]]
  #   }
  # )

  # Export plot ----
  # output$save_plot <- downloadHandler(
  #   filename = function() {
  #     paste("fitting-curve-", Sys.Date(), input$save_plot_format, sep = "")
  #   },
  #   content = function(file) {
  #     ggsave(plot = data()[["gg_curve"]], filename = file,
  #            width = 6, height = 4.5, dpi = 96,
  #            device = gsub("\\.", "", input$save_plot_format))
  #   }
  # )
}
