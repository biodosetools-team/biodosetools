# Advanced Fitting Modules ---------------------------------

dicentFittingAdvUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  bs4TabItem(
    tabName = label,
    h2("Dose-effect Fitting"),

    fluidRow(
      # Card: Data input options ----
      bs4MyCard(
        width = 5,
        title = "Data input options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
        fluidRow(
          column(
            width = 12,
            awesomeCheckbox(
              inputId = ns("load_count_data_check"),
              label = "Load data from file",
              value = FALSE, status = "warning"
            ),
            # Manual input ----
            conditionalPanel(
              condition = "!input.load_count_data_check",
              ns = ns,
              numericInput(ns("num_doses"), "Number of doses", value = 11),
              numericInput(ns("num_dicentrics"), "Maximum number of dicentrics per cell", value = 5)
            ),
            # Load from file ----
            conditionalPanel(
              condition = "input.load_count_data_check",
              ns = ns,
              fileInput(ns("load_count_data"), label = "File input", accept = c("txt/csv", "text/comma-separated-values", "text/plain", ".csv", ".txt", ".dat"))
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
        ),

        # Help button
        topButton =
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
              "Manual input" = "manual",
              "Load data"    = "load"
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
          )
        )
      ),

      # Card: Fitting options ----
      bs4MyCard(
        width = 5,
        title = "Fitting options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
        fluidRow(
          column(
            width = 12,
            # Fitting formula
            selectInput(
              ns("formula_select"),
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
            ),
            # Fitting model
            selectInput(
              ns("family_select"),
              label = "Fitting model",
              choices = list("Poisson" = "poisson", "Quasipoisson" = "quasipoisson"),
              selected = "poisson"
            ),
            # Use dispersion factor
            awesomeCheckbox(
              inputId = ns("slider_disp_select"),
              label = "Use σ²/y = 1",
              value = FALSE, status = "warning"
            )
          )
        ),
        # Help button
        topButton =
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
      )
    ),

    # Card: hot Count data input ----
    fluidRow(
      bs4MyCard(
        width = 12,
        title = "Data input",
        status = "inputs", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
        rHandsontableOutput(ns("hotable")),
        # Button
        br(),
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
        # tabBox: Fit results ----
        div(
          # Ugly fix for inner fluidRow() padding
          style = "margin-left: -7.5px; margin-right: -7.5px",
          bs4TabCard(
            id = ns("fit_results_tabs"),
            width = 12,
            side = "left",
            solidHeader = TRUE,
            closable = FALSE,
            bs4TabPanel(
              tabName = "Result of curve fit",
              active = TRUE,
              # h6("Fit summary"),
              # verbatimTextOutput(ns("fit_results")),
              h6("Fit formula"),
              verbatimTextOutput(ns("fit_formula")),
              h6("Coefficients"),
              rHandsontableOutput(ns("fit_coeffs"))
            ),
            bs4TabPanel(
              tabName = "Summary statistics",
              h6("Model-level statistics"),
              rHandsontableOutput(ns("fit_statistics")),
              h6("Correlation matrix"),
              rHandsontableOutput(ns("cor_mat")),
              h6("Variance-covariance matrix"),
              rHandsontableOutput(ns("var_cov_mat"))
            )
          )
        ),

        # Card: Export data and results ----
        bs4MyCard(
          width = 12,
          noPadding = TRUE,
          title = "Export results",
          status = "export", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
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


dicentFittingAdvHotTable <- function(input, output, session, stringsAsFactors) {

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
      num_doses <- as.numeric(input$num_doses)
      num_dicentrics <- as.numeric(input$num_dicentrics) + 1
    })

    if (!load_count_data) {
      # Doses data frame
      data_dose <- data.frame(
        D = rep(0.0, num_doses)
      )

      # Base data frame
      data_base <- data.frame(
        matrix(
          0,
          nrow = num_doses,
          ncol = num_dicentrics
        )
      )

      colnames(data_base) <- paste0("C", seq(0, num_dicentrics - 1, 1))

      # Full data frame
      full_data <- cbind(data_dose, data_base) %>%
        dplyr::mutate(D = as.numeric(D))
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
        select(D, N, X, everything())

      first_dicent_index <- 4
      last_dicent_index <- ncol(mytable) - 2
      num_rows <- nrow(mytable)

      mytable <- mytable %>%
        mutate(
          D = as.numeric(D),
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
    hot <- changed_data() %>%
      rhandsontable() %>%
      hot_cols(colWidths = 50) %>%
      hot_col(c(2, 3, seq(ncol(changed_data()) - 1, ncol(changed_data()), 1)), readOnly = TRUE) %>%
      hot_col(ncol(changed_data()), renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value > 1.96) {
              td.style.background = 'pink';
             }
           }") %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })
}

dicentFittingAdvTable <- function(input, output, session, stringsAsFactors) {
  table <- reactive({
    input$button_fit

    isolate({
      count_data <- hot_to_r(input$hotable)

      dose <- count_data[["D"]]
      aberr <- count_data[["X"]]
      cell <- count_data[["N"]]
    })

    data.frame(
      Dose = dose,
      Aberrations = aberr,
      Cells = cell
    )
  })

  # Output ----
  output$table <- renderTable({
    table()
  })
}


dicentFittingAdvResults <- function(input, output, session, stringsAsFactors) {

  # Calculations ----
  data <- reactive({
    input$button_fit

    isolate({
      count_data <- hot_to_r(input$hotable)

      dose <- count_data[["D"]]
      aberr <- count_data[["X"]]
      cell <- count_data[["N"]]
      disp <- count_data[["DI"]]

      model_formula <- input$formula_select
      model_family <- input$family_select
      disp_select <- input$slider_disp_select
    })

    # Construct predictors and model data
    x0 <- cell
    x1 <- cell * dose
    x2 <- cell * dose * dose
    model_data <- list(x0, x1, x2, aberr)

    # Weight selection
    if (disp_select) {
      weights <- rep(1, length(dose))
    } else {
      weights <- 1 / disp
    }

    # Select model formula
    if (model_formula == "lin-quad") {
      fit_formula <- as.formula("aberr ~ -1 + x0 + x1 + x2")
    } else if (model_formula == "lin") {
      fit_formula <- as.formula("aberr ~ -1 + x0 + x1")
    }
    else if (model_formula == "lin-quad-no-int") {
      fit_formula <- as.formula("aberr ~ -1 + x1 + x2")
    }
    else if (model_formula == "lin-no-int") {
      fit_formula <- as.formula("aberr ~ -1 + x1")
    }

    fit_link <- "identity"

    # Calculate fit
    fit_results <- glm(
      formula = fit_formula,
      family = eval(parse(text = paste(model_family, "(link =", fit_link, ")"))),
      weights = weights,
      data = model_data
    )

    # Summarise fit
    fit_summary <- summary(fit_results, correlation = TRUE)
    cor_mat <- fit_summary$correlation
    var_cov_mat <- vcov(fit_results)
    # fit_coeffs <- fit_summary$coefficients
    fit_coeffs <- broom::tidy(fit_results) %>%
      select(-statistic) %>%
      column_to_rownames(var = "term")
    # rownames(fit_coeffs) <- fit_coeffs[["term"]]


    # Generalized variance-covariance matrix
    general_fit_coeffs <- numeric(length = 3L)
    names(general_fit_coeffs) <- c("x0", "x1", "x2")

    for (var in rownames(fit_coeffs)) {
      general_fit_coeffs[var] <- fit_coeffs[var, "estimate"]
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

    # Generalized curves
    yield_fun <- function(x) {
      general_fit_coeffs[[1]] +
        general_fit_coeffs[[2]] * x +
        general_fit_coeffs[[3]] * x * x
    }

    chisq_df <- nrow(fit_coeffs)
    R_factor <- sqrt(qchisq(.95, df = chisq_df))

    yield_error_fun <- function(x) {
      sqrt(
        general_var_cov_mat[["x0", "x0"]] +
          general_var_cov_mat[["x1", "x1"]] * x * x +
          general_var_cov_mat[["x2", "x2"]] * x * x * x * x +
          2 * general_var_cov_mat[["x0", "x1"]] * x +
          2 * general_var_cov_mat[["x0", "x2"]] * x * x +
          2 * general_var_cov_mat[["x1", "x2"]] * x * x * x
      )
    }

    # Plot data
    plot_data <- broom::augment(fit_results)

    curves_data <- data.frame(dose = seq(0, max(x1 / x0), length.out = 100)) %>%
      mutate(
        yield = yield_fun(dose),
        yield_low = yield_fun(dose) - R_factor * yield_error_fun(dose),
        yield_upp = yield_fun(dose) + R_factor * yield_error_fun(dose)
      )

    # Make plot
    gg_curve <- ggplot(plot_data / x0) +
      # Observed data
      geom_point(aes(x = x1, y = aberr)) +
      # Fitted curve
      stat_function(
        data = data.frame(x = c(0, max(x1 / x0))),
        mapping = aes(x),
        fun = function(x) yield_fun(x),
        linetype = "dashed"
      ) +
      # Confidence bands (Merkle, 1983)
      geom_ribbon(data = curves_data, aes(x = dose, ymin = yield_low, ymax = yield_upp), alpha = 0.25) +
      labs(x = "Dose (Gy)", y = "Aberrations / Cells") +
      theme_bw()

    # Make list of results to return
    results_list <- list(
      fit_results = fit_results,
      fit_coeffs = fit_coeffs,
      var_cov_mat = var_cov_mat,
      cor_mat = cor_mat,
      gg_curve = gg_curve
    )

    return(results_list)
  })

  # Results outputs ----
  output$fit_results <- renderPrint({
    # Result of curve fit 'fit_results'
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_results"]]
  })

  output$fit_formula <- renderPrint({
    # Fitting formula
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_results"]]$formula
    # TODO: transform this into LaTeX output?
  })

  output$fit_statistics <- renderRHandsontable({
    # Model-level statistics using broom::glance
    if (input$button_fit <= 0) return(NULL)
    broom::glance(data()[["fit_results"]]) %>%
      select(logLik, df.null, df.residual, null.deviance, deviance, AIC, BIC) %>%
      mutate(null.deviance = as.character(null.deviance)) %>%
      rhandsontable()
  })

  output$fit_coeffs <- renderRHandsontable({
    # Coefficients 'fit_coeffs'
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_coeffs"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 75) %>%
      hot_cols(format = "0.000")
  })

  output$var_cov_mat <- renderRHandsontable({
    # Variance-covariance matrix 'var_cov_mat'
    if (input$button_fit <= 0) return(NULL)
    data()[["var_cov_mat"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.0000000")
  })

  output$cor_mat <- renderRHandsontable({
    # Correlation matrix 'cor_mat'
    if (input$button_fit <= 0) return(NULL)
    data()[["cor_mat"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$plot <- renderPlot(
    # Plot of the data and fitted curve
    res = 120, {
      if (input$button_fit <= 0) return(NULL)
      data()[["gg_curve"]]
    }
  )

  # Export count data ----
  output$save_count_data <- downloadHandler(
    filename = function() {
      paste("count-data-", Sys.Date(), input$save_count_data_format, sep = "")
    },
    content = function(file) {
      if (input$save_count_data_format == ".csv") {
        write.csv(hot_to_r(input$hotable), file, row.names = FALSE)
      } else if (input$save_count_data_format == ".tex") {
        print(xtable::xtable(hot_to_r(input$hotable)), type = "latex", file)
      }
    }
  )

  # Export fit coefficients ----
  output$save_fit_data <- downloadHandler(
    filename = function() {
      paste("fitting-data-", Sys.Date(), input$save_fit_data_format, sep = "")
    },
    content = function(file) {
      if (input$save_fit_data_format == ".rds") {
        saveRDS(data()[["fit_results"]], file = file)
      } #else if (input$save_fit_data_format == ".csv") {
      #   write.csv(data()[["fit_coeffs"]], file, row.names = FALSE)
      # } else if (input$save_fit_data_format == ".tex") {
      #   print(xtable::xtable(data()[["fit_coeffs"]]), type = "latex", file)
      # }
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
  output$save_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste("report-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      # normReport <- file.path("report.Rmd")
      file.copy("reports/dicentrics-fitting-report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        fit_result = data()[["fit_results"]],
        fit_coeffs = data()[["fit_coeffs"]],
        var_cov_mat = data()[["var_cov_mat"]],
        cor_mat = data()[["cor_mat"]],
        gg_curve = data()[["gg_curve"]]
      )

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}
