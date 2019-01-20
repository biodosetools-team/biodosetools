# Advanced Fitting Modules ---------------------------------

fittingAdvUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tabItem(
    tabName = label,
    h2("Dose-effect Fitting"),
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
            checkboxInput(
              inputId = ns("load_count_data_check"),
              label = "Load data from file",
              value = FALSE
            ),
            # awesomeCheckbox(
            #   inputId = ns("Id024"),
            #   label = "Load data from file",
            #   value = TRUE, status = "warning"
            # ),
            # Inputs
            conditionalPanel(
              condition = "!input.load_count_data_check",
              ns = ns,
              numericInput(ns("num_doses"), "Number of doses", value = 11),
              numericInput(ns("num_dicentrics"), "Maximum number of dicentrics per cell", value = 5),
              # Help button
              bsButton(ns("help_write_count_data"),
                       class = "rightAlign",
                       label = "",
                       icon = icon("question"),
                       style = "default", size = "default"
              ),
              bsModal(
                id = ns("help_write_count_data_dialog"),
                title = "Help: Count data input",
                trigger = ns("help_write_count_data"),
                size = "large",
                withMathJax(includeMarkdown("help/write_count_data.md"))
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

      # Fitting options ----
      box(
        width = 5,
        title = "Fitting options",
        status = "warning", solidHeader = F, collapsible = T,
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
              choices = list("Automatic" = 0, "Poisson" = 1, "Quasipoisson" = 2),
              selected = 1
            ),
            # Use dispersion factor
            checkboxInput(
              inputId = ns("slider_disp_select"),
              label = "Use σ²/y = 1",
              value = FALSE
            ),
            # Help button
            bsButton(ns("help_fit"),
              class = "rightAlign",
              label = "",
              icon = icon("question"),
              style = "default", size = "default"
            ),
            bsModal(
              id = ns("help_fit_dialog"),
              title = "Help: Fitting options",
              trigger = ns("help_fit"),
              size = "large",
              withMathJax(includeMarkdown("help/fitting_options.md"))
            )
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
        rHandsontableOutput(ns("hotable")),
        # Button
        br(),
        downloadButton(ns("save_count_data"), class = "side-widget", "Save count data"),
        div(
          class = "side-widget",
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
        # Main tabBox ----
        tabBox(
          width = 12,
          side = "left",
          tabPanel(
            title = "Result of curve fit",
            h4("Fit summary"),
            verbatimTextOutput(ns("fit_results")),
            h4("Coefficients"),
            rHandsontableOutput(ns("bstat"))
          ),
          tabPanel(
            title = "Summary statistics",
            h4("Correlation matrix"),
            rHandsontableOutput(ns("cor_mat")),
            h4("Variance-covariance matrix"),
            rHandsontableOutput(ns("var_cov_mat"))
          )
        ),
        # Export data and results ----
        box(
          width = 12,
          title = "Export results",
          status = "danger", solidHeader = F, collapsible = T, collapsed = F,
          # Download fit data & report
          downloadButton(ns("save_fit_data"), class = "side-widget", "Save fitting data"),
          div(
            class = "side-widget",
            selectInput(
              ns("save_fit_data_format"),
              label = NULL,
              width = "85px",
              choices = list(".csv", ".tex"),
              selected = ".csv"
            )
          ),
          # Download report
          div(class = "widget-sep", br()),
          downloadButton(ns("save_report"), class = "export-button", "Download report")
        )
      ),
      column(
        width = 6,
        # Plot box ----
        box(
          width = 12,
          title = "Curve plot",
          status = "success", solidHeader = F, collapsible = T, collapsed = F,
          # Plot
          plotOutput(ns("plot")),
          # Download plot
          downloadButton(ns("save_plot"), class = "results-button side-widget", "Save plot"),
          div(
            class = "side-widget",
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


fittingAdvHotTable <- function(input, output, session, stringsAsFactors) {

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

      last_dicent_index <- ncol(mytable) - 2
      num_rows <- nrow(mytable)

      mytable <- mytable %>%
        mutate(
          D = as.numeric(D),
          X = as.integer(X),
          N = as.integer(rowSums(.[4:last_dicent_index]))
        )

      # Ugly method to calculate index of dispersion
      for (row in 1:num_rows) {
        xf <- 0
        x2f <- 0
        # Calculate summatories
        for (k in seq(0, last_dicent_index - 4, 1)) {
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

fittingAdvTable <- function(input, output, session, stringsAsFactors) {
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


fittingAdvResults <- function(input, output, session, stringsAsFactors) {

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
      curve_fun <- function(x) {
        bstat[1, 1] + bstat[2, 1] * x + bstat[3, 1] * x * x
      }
    } else if (model_formula == "lin") {
      fit_formula <- as.formula("aberr ~ -1 + x0 + x1")
      curve_fun <- function(x) {
        bstat[1, 1] + bstat[2, 1] * x
      }
    }
    else if (model_formula == "lin-quad-no-int") {
      fit_formula <- as.formula("aberr ~ -1 + x1 + x2")
      curve_fun <- function(x) {
        bstat[2, 1] * x + bstat[3, 1] * x * x
      }
    }
    else if (model_formula == "lin-no-int") {
      fit_formula <- as.formula("aberr ~ -1 + x1")
      curve_fun <- function(x) {
        bstat[2, 1] * x
      }
    }

    fit_link <- "identity"

    # Select model family
    if (model_family == 1) {
      family_select <- "poisson"
    } else if (model_family == 2) {
      family_select <- "quasipoisson"
    }
    # TODO: add automatic selection?

    # Calculate fit
    fit_results <- glm(
      formula = fit_formula,
      family = eval(parse(text = paste(family_select, "(link =", fit_link, ")"))),
      weights = weights,
      data = model_data
    )

    # Summarise fit
    fit_summary <- summary(fit_results, correlation = TRUE)
    cor_mat <- fit_summary$correlation
    bstat <- fit_summary$coefficients
    var_cov_mat <- vcov(fit_results)

    plot_data <- broom::augment(fit_results)

    # Make plot
    gg_curve <- ggplot(plot_data / x0) +
      geom_point(aes(x = x1, y = aberr)) +
      geom_ribbon(aes(
        x = x1,
        ymin = .fitted - .se.fit,
        ymax = .fitted + .se.fit
      ),
      alpha = 0.25
      ) +
      stat_function(
        data = data.frame(x = c(0, max(x1 / x0))), aes(x),
        fun = function(x) curve_fun(x),
        linetype = "dashed"
      ) +
      labs(x = "Dose (Gy)", y = "Aberrations / Cells") +
      theme_bw()

    # Make list of results to return
    results_list <- list(
      fit_results = fit_results,
      bstat = bstat,
      var_cov_mat = var_cov_mat,
      cor_mat = cor_mat,
      gg_curve = gg_curve
    )

    return(results_list)
  })

  # Results outputs ----
  output$fit_results <- renderPrint({
    # "Result of curve fit 'result'"
    if(input$button_fit <= 0) return(NULL)
    data()[["fit_results"]]
  })

  output$bstat <- renderRHandsontable({
    # "Coefficients 'bstat'"
    if(input$button_fit <= 0) return(NULL)
    rhandsontable(data()[["bstat"]])
  })

  output$var_cov_mat <- renderRHandsontable({
    # "variance-covariance matrix 'var_cov_mat'"
    if(input$button_fit <= 0) return(NULL)
    rhandsontable(data()[["var_cov_mat"]])
  })

  output$cor_mat <- renderRHandsontable({
    # "Correlation matrix 'corma'"
    if(input$button_fit <= 0) return(NULL)
    rhandsontable(data()[["cor_mat"]])
  })

  output$plot <- renderPlot(
    res = 120, {
      if(input$button_fit <= 0) return(NULL)
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
      if (input$save_fit_data_format == ".csv") {
        write.csv(data()[["bstat"]], file, row.names = FALSE)
      } else if (input$save_fit_data_format == ".tex") {
        print(xtable::xtable(data()[["bstat"]]), type = "latex", file)
      }
    }
  )

  # Export plot ----
  output$save_plot <- downloadHandler(
    filename = function() {
      paste("fitting-curve-", Sys.Date(), input$save_plot_format, sep = "")
    },
    content = function(file) {
      ggsave(plot = data()[["gg_curve"]], filename = file,
             width = 6, height = 4.5, dpi = 96,
             device = gsub("\\.", "", input$save_plot_format))
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
      normReport <- file.path("report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        fit_result = data()[["fit_results"]],
        bstat = data()[["bstat"]],
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
