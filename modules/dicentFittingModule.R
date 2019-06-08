# Fitting Modules ---------------------------------

dicentFittingUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  bs4TabItem(
    tabName = label,
    h2("Dose-effect Fitting"),

    fluidRow(
      # Card: Data input options ----
      bs4MyCard(
        width = 6,
        title = "Data input options",
        status = "options", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
        fluidRow(
          column(
            width = 12,
            # Load file checkbox
            awesomeCheckbox(
              inputId = ns("load_count_data_check"),
              label = "Load data from file",
              value = FALSE, status = "warning"
            ),
            # Full/aggregated data checkbox
            awesomeCheckbox(
              inputId = ns("use_aggr_count_data_check"),
              width = "100%",
              label = "Only provide total number of dicentrics",
              value = FALSE, status = "warning"
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
              numericInput(ns("num_dicentrics"), "Maximum number of dicentrics per cell", value = 5)
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
          ),
          # Tooltips
          bsTooltip(
            ns("button_upd_table"),
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

      # Card: Fitting options ----
      bs4MyCard(
        width = 6,
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
                  "Y = C + αD + βD²" = "lin-quad"
                  # "Y = αD + βD²" = "lin-quad-no-int"
                ),
                "Linear" = c(
                  "Y = C + αD" = "lin"
                  # "Y = αD" = "lin-no-int"
                )
              ),
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
        div(style="height = auto;",
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
              h6("Fit formula"),
              uiOutput(ns("fit_formula_tex")),

              h6("Model"),
              uiOutput(ns("fit_model_summary")),

              br(),
              h6("Coefficients"),
              rHandsontableOutput(ns("fit_coeffs"))
            ),
            bs4TabPanel(
              tabName = "Summary statistics",
              h6("Model-level statistics"),
              rHandsontableOutput(ns("fit_model_statistics")),

              br(),
              h6("Correlation matrix"),
              rHandsontableOutput(ns("fit_cor_mat")),

              br(),
              h6("Variance-covariance matrix"),
              rHandsontableOutput(ns("fit_var_cov_mat"))
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


dicentFittingHotTable <- function(input, output, session, stringsAsFactors) {

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
      use_aggr_count_data <- input$use_aggr_count_data_check
      count_data <- input$load_count_data
      num_doses <- as.numeric(input$num_doses)
      num_dicentrics <- as.numeric(input$num_dicentrics) + 1
    })

    if (!load_count_data) {
      if (!use_aggr_count_data) {
        # Doses data frame
        data_doses <- data.frame(
          D = rep(0.0, num_doses)
        )

        # Base data frame
        data_base <- data.frame(
          matrix(
            0,
            nrow = num_doses,
            ncol = num_dicentrics
          )
        ) %>%
          `colnames<-`(paste0("C", seq(0, num_dicentrics - 1, 1)))

        # Full data frame
        full_data <- cbind(data_doses, data_base) %>%
          dplyr::mutate(D = as.numeric(D))

      } else {
        full_data <- data.frame(
          D = rep(0.0, num_doses),
          N = rep(0, num_doses),
          X = rep(0, num_doses)
        ) %>%
          dplyr::mutate(
            D = as.numeric(D),
            N = as.integer(N),
            X = as.integer(X)
          )
      }
    } else {
      if (!use_aggr_count_data) {
        full_data <- read.csv(count_data$datapath, header = TRUE) %>%
          # Force column naming
          `colnames<-`(c("D", paste0("C", seq(0, ncol(.) - 2, 1)))) %>%
          dplyr::mutate_at(vars(starts_with("C")), as.integer)
      } else {
        full_data <- read.csv(count_data$datapath, header = TRUE) %>%
          # Force column naming
          `colnames<-`(c("D", "N", "X")) %>%
          dplyr::mutate_at(c("N", "X"), as.integer)
      }
    }

    return(full_data)
  })

  # Reactive data frame ----
  changed_data <- reactive({
    # Create button dependency for updating dimensions
    input$button_upd_table

    isolate({
      use_aggr_count_data <- input$use_aggr_count_data_check
    })

    # Create button dependency for updating N, X, DI, u values
    if (!use_aggr_count_data) {
      input$button_upd_params
    }

    isolate({
      if (is.null(input$count_data_hot) || isolate(table_reset$value == 1)) {

        table_reset$value <- 0
        mytable <- previous()

        # Initial renderization of the table
        if (!use_aggr_count_data) {
          # Calculated columns
          mytable <- mytable %>%
            dplyr::mutate(
              N = 0,
              X = 0,
              DI = 0,
              u = 0
            ) %>%
            dplyr::select(D, N, X, everything()) %>%
            dplyr::mutate(
              D = as.numeric(D)
            ) %>%
            dplyr::mutate_at(
              c("X", "N", grep("C", names(.), value = TRUE)),
              as.integer
            )
        }
        return(mytable)
      } else if (!identical(previous(), input$count_data_hot)) {
        mytable <- as.data.frame(hot_to_r(input$count_data_hot))

        if (!use_aggr_count_data) {
          # Calculated columns
          mytable <- mytable %>%
            dplyr::mutate(
              N = 0,
              X = 0,
              DI = 0,
              u = 0
            ) %>%
            dplyr::select(D, N, X, everything())

          first_dicent_index <- 4
          last_dicent_index <- ncol(mytable) - 2
          num_rows <- nrow(mytable)

          mytable <- mytable %>%
            dplyr::mutate(
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
        } else {
          mytable <- mytable %>%
            dplyr::mutate(
              D = as.numeric(D)
            )
        }

        return(mytable)
      }
    })
  })

  # Output ----
  output$count_data_hot <- renderRHandsontable({
    hot <- changed_data() %>%
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 50) %>%
      hot_col(c(1), format = "0.000", colWidths = 60) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)

    if (ncol(changed_data()) > 3) {
      hot <- hot %>%
        hot_col(c(2, 3, seq(ncol(changed_data()) - 1, ncol(changed_data()), 1)), readOnly = TRUE) %>%
        hot_col(ncol(changed_data()), renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value > 1.96) {
              td.style.background = 'pink';
             }
           }")
    }

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })
}

dicentFittingResults <- function(input, output, session, stringsAsFactors) {

  data <- reactive({
  # Calculations ----
    input$button_fit

    isolate({
      count_data <- hot_to_r(input$count_data_hot)

      model_formula <- input$formula_select
      model_family <- input$family_select
    })


    # Fitting functions ----

    prepare_maxlik_count_data <- function(count_data, model_formula) {
      if (ncol(count_data) > 3) {
        # Full distribution data
        dose_vec <- rep(
          count_data[["D"]],
          count_data[["N"]]
        )

        cell_vec <- rep(
          rep(1, nrow(count_data)),
          count_data[["N"]]
        )

        dics_vec <- rep(
          count_data %>%
            names() %>%
            grep("C", ., value = TRUE) %>%
            gsub("C", "", .) %>%
            rep(nrow(count_data)) %>%
            as.numeric()
          ,
          count_data %>%
            .[, grep("C", names(.), value = T)] %>%
            as.matrix() %>%
            t() %>%
            as.numeric()
        )

        parsed_data <- data.frame(
          aberr = dics_vec,
          dose = dose_vec,
          C = cell_vec) %>%
          mutate(
            α = dose * C,
            β = dose^2 * C) %>%
          dplyr::select(aberr, C, α, β, dose)
      } else {
        # Aggregated data only
        parsed_data <- count_data %>%
          dplyr::rename(
            aberr = X,
            C = N
          ) %>%
          dplyr::mutate(
            α = D * C,
            β = D^2 * C
          ) %>%
          dplyr::select(aberr, C, α, β)
      }

      # Delete C column for models with no intercept

      if (stringr::str_detect(model_formula, "no-int")) {
        parsed_data <- parsed_data %>%
          dplyr::select(-C)
      }

      # Return data frame
      return(parsed_data)
    }

    get_fit_glm_method <- function(count_data, model_formula, model_family, fit_link = "identity") {

      # Parse count data
      doses <- count_data[["D"]]
      aberr <- count_data[["X"]]
      cells <- count_data[["N"]]
      if (ncol(count_data) > 3) {
        # Full distribution data
        disp <- count_data[["DI"]]
      } else {
        # Aggregated data only
        disp <- rep(1, nrow(count_data))
      }

      # Construct predictors and model data
      C <- cells
      α <- cells * doses
      β <- cells * doses * doses
      model_data <- list(C, α, β, aberr)
      weights <- 1 / disp

      # Select model formula
      if (model_formula == "lin-quad") {
        fit_formula_raw <- "aberr ~ -1 + C + α + β"
        fit_formula_tex <- "Y = C + \\alpha D + \\beta D^{2}"
      } else if (model_formula == "lin") {
        fit_formula_raw <- "aberr ~ -1 + C + α"
        fit_formula_tex <- "Y = C + \\alpha D"
      }
      else if (model_formula == "lin-quad-no-int") {
        fit_formula_raw <- "aberr ~ -1 + α + β"
        fit_formula_tex <- "Y = \\alpha D + \\beta D^{2}"
      }
      else if (model_formula == "lin-no-int") {
        fit_formula_raw <- "aberr ~ -1 + α"
        fit_formula_tex <- "Y = \\alpha D"
      }
      fit_formula <- as.formula(fit_formula_raw)


      # Perform automatic fit calculation
      if (model_family == "poisson") {
        # Poisson model
        fit_results <- glm(
          formula = fit_formula,
          family = poisson(link = fit_link),
          data = model_data
        )
        fit_dispersion <- NULL
      } else {
        # Automatic and Quasi-poisson model
        fit_results <- glm(
          formula = fit_formula,
          family = quasipoisson(link = fit_link),
          weights = weights,
          data = model_data
        )
        fit_dispersion <- summary(fit_results)$dispersion
        # Check if Poisson model is more suitable
        if (fit_dispersion <= 1) {
          fit_results <- glm(
            formula = fit_formula,
            family = poisson(link = fit_link),
            data = model_data
          )
          fit_dispersion <- NULL
        }
      }

      # Summarise fit
      fit_summary <- summary(fit_results, correlation = TRUE)
      fit_cor_mat <- fit_summary$correlation
      fit_var_cov_mat <- vcov(fit_results)
      fit_coeffs_vec <- stats::coef(fit_results)

      # Model-specific statistics
      fit_model_statistics <- cbind(
        logLik =   stats::logLik(fit_results) %>% as.numeric(),
        deviance = fit_results[["deviance"]],
        df =       fit_results[["df.residual"]],
        AIC =      stats::AIC(fit_results),
        BIC =      stats::BIC(fit_results)
      )

      # Correct p-values depending on model dispersion
      t_value <- fit_coeffs_vec / sqrt(diag(fit_var_cov_mat))

      # Make coefficients table
      if (is.null(fit_dispersion)) {
        # For Poisson model
        fit_coeffs <- cbind(
          estimate =  fit_coeffs_vec,
          std.error = sqrt(diag(fit_var_cov_mat)),
          statistic = t_value,
          p.value =   2 * pnorm(-abs(t_value))
        ) %>%
          `row.names<-`(names(fit_coeffs_vec)) %>%
          `colnames<-`(c("estimate", "std.error", "statistic", "p.value"))

        # Summary of model used
        fit_model_summary <- paste("A Poisson model assuming equidispersion was used as dispersion ≤ 1.")
      } else if (fit_dispersion > 1) {
        # For Quasi-poisson model
        fit_coeffs <- cbind(
          estimate =  fit_coeffs_vec,
          std.error = sqrt(diag(fit_var_cov_mat)),
          statistic = t_value,
          p.value =   2 * 2 * pt(-abs(t_value), fit_results$df.residual)
        ) %>%
          `row.names<-`(names(fit_coeffs_vec)) %>%
          `colnames<-`(c("estimate", "std.error", "statistic", "p.value"))

        # Summary of model used
        fit_model_summary <- paste0("A Quasi-poisson model accounting for overdispersion was used as dispersion (=", round(fit_dispersion, 2), ") > 1.")
      }

      # Return objects
      fit_results_list <- list(
        # Raw data
        fit_raw_data = count_data %>% as.matrix(),
        # Formulas
        fit_formula_raw = fit_formula_raw,
        fit_formula_tex = fit_formula_tex,
        # Coefficients
        fit_coeffs = fit_coeffs,
        fit_cor_mat = fit_cor_mat,
        fit_var_cov_mat = fit_var_cov_mat,
        # Model statistics
        fit_dispersion = fit_dispersion,
        fit_model_statistics = fit_model_statistics,
        # Algorithm and model summary
        fit_algorithm = "glm",
        fit_model_summary = fit_model_summary
      )

      return(fit_results_list)
    }

    get_fit_maxlik_method <- function(data, model_formula, model_family, fit_link) {
      # type can be "poisson", "quasipoisson" or "automatic"
      # in case of automatic the script will choose a quasipoisson model if deviance > df (see below)
      # start should include starting values for the coefficients of the regression model
      # Please note that most parts of this code are from Oliviera et al. this should be cited somewhere

      # Parse full data into aggregated format
      if ("dose" %in% colnames(data)) {
        data_aggr <- data %>%
          dplyr::group_by(aberr, dose) %>%
          dplyr::summarise(n = n()) %>%
          dplyr::group_by(dose) %>%
          dplyr::summarise(
            C = sum(n),
            X = sum(ifelse(aberr > 0, n * aberr, 0))
          ) %>%
          dplyr::mutate(
            α = dose * C,
            β = dose^2 * C
          ) %>%
          dplyr::rename(aberr = X) %>%
          select(aberr, dose, C, α, β)
      } else {
        data_aggr <- data
      }

      # Select model formula
      if (model_formula == "lin-quad") {
        fit_formula_raw <- "aberr ~ -1 + C + α + β"
        fit_formula_tex <- "Y = C + \\alpha D + \\beta D^{2}"
      } else if (model_formula == "lin") {
        fit_formula_raw <- "aberr ~ -1 + C + α"
        fit_formula_tex <- "Y = C + \\alpha D"
      }
      else if (model_formula == "lin-quad-no-int") {
        fit_formula_raw <- "aberr ~ -1 + α + β"
        fit_formula_tex <- "Y = \\alpha D + \\beta D^{2}"
      }
      else if (model_formula == "lin-no-int") {
        fit_formula_raw <- "aberr ~ -1 + α"
        fit_formula_tex <- "Y = \\alpha D"
      }
      fit_formula <- as.formula(fit_formula_raw)

      if (stringr::str_detect(model_formula, "no-int")) {
        data_aggr <- data_aggr %>%
          dplyr::select(-C)
      }

      # Find starting values for the mean
      mustart <- lm(fit_formula, data = data_aggr)$coefficients
      if (mustart[1] <= 0) {
        mustart[1] <- 0.001
      }

      # Black magic
      mf <- match.call()
      m <- match(c("formula", "data"), names(mf), 0)
      mf <- mf[c(1, m)]
      mf$drop.unused.levels <- TRUE

      if (length(fit_formula[[3]]) > 1 && identical(fit_formula[[3]][[1]], as.name("|"))) {
        ff <- fit_formula
        fit_formula[[3]][1] <- call("+")
        mf$formula <- fit_formula
        ffc <- . ~ .
        ffz <- ~.
        ffc[[2]] <- ff[[2]]
        ffc[[3]] <- ff[[3]][[2]]
        ffz[[3]] <- ff[[3]][[3]]
        ffz[[2]] <- NULL
      } else {
        ffz <- ffc <- ff <- fit_formula
        ffz[[2]] <- NULL
      }

      if (inherits(try(terms(ffz), silent = TRUE), "try-error")) {
        ffz <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])), deparse(ffz))))
      }

      mf[[1]] <- as.name("model.frame")
      mf <- eval(mf, parent.frame())
      mt <- attr(mf, "terms")
      mtX <- terms(ffc, data = data)
      X <- model.matrix(mtX, mf)
      mtZ <- terms(ffz, data = data)
      mtZ <- terms(update(mtZ, ~.), data = data)
      Z <- model.matrix(mtZ, mf)
      Y <- model.response(mf, "numeric")

      if (all(X[, 1] == 1)) {
        intercept <- TRUE
      } else {
        intercept <- FALSE
      }

      # Summarise black magic
      ndic <- max(Y)
      n <- length(Y)
      linkstr <- "logit"
      linkobj <- make.link(linkstr)
      linkinv <- linkobj$linkinv
      grad <- NULL
      kx <- NCOL(X)
      Y0 <- Y <= 0
      Y1 <- Y > 0

      # Find starting values for the mean
      if (fit_link == "log") {
        if (is.null(mustart)) mustart <- as.numeric(glm.fit(X, Y, family = poisson())$coefficients)
      } else {
        if (is.null(mustart)) {
          stop("If link=identity, starting values must be provided")
        } else {
          mustart <- mustart
        }
      }

      # Model constraints
      npar <- kx
      if (intercept) {
        A <- rbind(X, c(1, rep(0, npar - 1)))
        B <- rep(0, n + 1)
      } else {
        A <- X
        B <- rep(0, n)
      }

      # Loglikelihood function
      loglik <- function(parms) {
        if (fit_link == "log") {
          mu <- as.vector(exp(X %*% parms[1:npar]))
        } else {
          mu <- as.vector(X %*% parms[1:npar])
        }
        loglikh <- sum(-mu + Y * log(mu) - lgamma(Y + 1))
        return(loglikh)
      }

      # Perform fitting
      if (fit_link == "log") {
        constraints <- NULL
        fit_results <- maxLik::maxLik(logLik = loglik, grad = grad, start = mustart, constraints = constraints, iterlim = 1000)
      } else {
        fit_results <- maxLik::maxLik(logLik = loglik, grad = grad, start = mustart, constraints = list(ineqA = A, ineqB = B), iterlim = 1000)
      }
      hess <- maxLik::hessian(fit_results)

      if (fit_link == "log") {
        mu <- as.vector(exp(X %*% fit_results$estimate[1:npar]))
      } else {
        mu <- as.vector(X %*% fit_results$estimate[1:npar])
      }

      # Summarise fit
      fit_summary <- summary(fit_results)
      fit_var_cov_mat <- base::solve(-hess)
      fit_coeffs_vec <- fit_results$estimate
      fit_dispersion <- sum(((Y - mu)^2) / (mu * (n - npar)))

      # Model-specific statistics
      fit_model_statistics <- cbind(
        logLik = stats::logLik(fit_results),
        deviance = sum(poisson(link = "identity")$dev.resids(Y, mu, 1)),
        df = n - npar,
        AIC = -2 * stats::logLik(fit_results) + 2 * length(fit_coeffs_vec),
        BIC = -2 * stats::logLik(fit_results) + log(n) * length(fit_coeffs_vec)
      )

      # Correct p-values depending on model dispersion
      if (model_family == "poisson" | (model_family == "automatic" & fit_dispersion <= 1)) {
        t_value <- fit_coeffs_vec / sqrt(diag(fit_var_cov_mat))

        # For Poisson model
        fit_coeffs <- cbind(
          estimate =  fit_coeffs_vec,
          std.error = sqrt(diag(fit_var_cov_mat)),
          statistic = t_value,
          p.value =   2 * pnorm(-abs(t_value))
        ) %>%
          `row.names<-`(names(fit_coeffs_vec))

        # Summary of model used
        fit_model_summary <- paste("A Poisson model assuming equidispersion was used as dispersion ≤ 1.")
      } else if (model_family == "quasipoisson" | (model_family == "automatic" & fit_dispersion > 1)) {
        fit_var_cov_mat <- fit_var_cov_mat * fit_dispersion
        t_value <- fit_coeffs_vec / sqrt(diag(fit_var_cov_mat))

        # For Quasi-poisson model
        fit_coeffs <- cbind(
          estimate =  fit_coeffs_vec,
          std.error = sqrt(diag(fit_var_cov_mat)),
          statistic = t_value,
          p.value =   2 * 2 * pt(-abs(t_value), fit_model_statistics[, "df"] %>% as.numeric())
        ) %>%
          `row.names<-`(names(fit_coeffs_vec))

        # Summary of model used
        fit_model_summary <- paste0("A Quasi-poisson model accounting for overdispersion was used as dispersion (=", round(fit_dispersion, 2), ") > 1.")
      }

      # Calculate correlation matrix
      fit_cor_mat <- fit_var_cov_mat
      for (x_var in rownames(fit_var_cov_mat)) {
        for (y_var in colnames(fit_var_cov_mat)) {
          fit_cor_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var] / (fit_coeffs[x_var, "std.error"] * fit_coeffs[y_var, "std.error"])
        }
      }

      # Return objects
      fit_results_list <- list(
        # Raw data
        fit_raw_data = data_aggr %>% as.matrix(),
        # Formulas
        fit_formula_raw = fit_formula_raw,
        fit_formula_tex = fit_formula_tex,
        # Coefficients
        fit_coeffs = fit_coeffs,
        fit_cor_mat = fit_cor_mat,
        fit_var_cov_mat = fit_var_cov_mat,
        # Model statistics
        fit_dispersion = fit_dispersion,
        fit_model_statistics = fit_model_statistics,
        # Algorithm and model summary
        fit_algorithm = "constraint-maxlik-optimization",
        fit_model_summary = fit_model_summary
      )

      return(fit_results_list)
    }


    get_fit_results <- function(data, model_formula, model_family, fit_link = "identity") {
      # If glm produces an error, constraint ML maximization is performed
      tryCatch({
        # Perform fitting
        fit_results_list <- get_fit_glm_method(count_data, model_formula, model_family, fit_link)

        # Return results
        return(fit_results_list)
      },
      error = function(error_message) {
        message("Warning: Problem with glm -> constraint ML optimization will be used instead of glm")
        # Perform fitting
        prepared_data <- prepare_maxlik_count_data(count_data, model_formula)
        fit_results_list <- get_fit_maxlik_method(prepared_data, model_formula, model_family, fit_link)
        fit_results_list[["fit_raw_data"]] <- count_data %>% as.matrix()

        # Return results
        return(fit_results_list)
      })
    }

    # Curve function ----

    get_dose_curve <- function(fit_results_list) {
      # Read objects from fit results list
      count_data <- fit_results_list[["fit_raw_data"]] %>% as.data.frame()
      fit_coeffs <- fit_results_list[["fit_coeffs"]]
      fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]

      # Generalized variance-covariance matrix
      general_fit_coeffs <- numeric(length = 3L) %>%
        `names<-`(c("C", "α", "β"))

      for (var in row.names(fit_coeffs)) {
        general_fit_coeffs[[var]] <- fit_coeffs[var, "estimate"] %>% as.numeric()
      }

      # Generalized fit coefficients
      general_fit_var_cov_mat <- matrix(0, nrow = 3, ncol = 3) %>%
        `row.names<-`(c("C", "α", "β")) %>%
        `colnames<-`(c("C", "α", "β"))

      for (x_var in rownames(fit_var_cov_mat)) {
        for (y_var in colnames(fit_var_cov_mat)) {
          general_fit_var_cov_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var]
        }
      }

      # Generalized curves
      yield_fun <- function(d) {
        general_fit_coeffs[["C"]] +
          general_fit_coeffs[["α"]] * d +
          general_fit_coeffs[["β"]] * d^2
      }

      chisq_df <- nrow(fit_coeffs)
      R_factor <- sqrt(qchisq(0.95, df = chisq_df))

      yield_error_fun <- function(d) {
        sqrt(
          general_fit_var_cov_mat[["C", "C"]] +
            general_fit_var_cov_mat[["α", "α"]] * d^2 +
            general_fit_var_cov_mat[["β", "β"]] * d^4 +
            2 * general_fit_var_cov_mat[["C", "α"]] * d +
            2 * general_fit_var_cov_mat[["C", "β"]] * d^2 +
            2 * general_fit_var_cov_mat[["α", "β"]] * d^3
        )
      }

      # Plot data
      plot_data <- count_data %>%
        dplyr::mutate(
          yield = X / N,
          dose = D
        ) %>%
        dplyr::select(dose, yield)

      curves_data <- data.frame(dose = seq(0, max(plot_data[["dose"]]), length.out = 100)) %>%
        dplyr::mutate(
          yield = yield_fun(dose),
          yield_low = yield_fun(dose) - R_factor * yield_error_fun(dose),
          yield_upp = yield_fun(dose) + R_factor * yield_error_fun(dose)
        )

      # Make plot
      gg_curve <- ggplot(plot_data) +
        # Observed data
        geom_point(aes(x = dose, y = yield)) +
        # Fitted curve
        stat_function(
          data = data.frame(x = c(0, max(plot_data[["dose"]]))),
          mapping = aes(x),
          fun = function(x) yield_fun(x),
          linetype = "dashed"
        ) +
        # Confidence bands (Merkle, 1983)
        geom_ribbon(data = curves_data, aes(x = dose, ymin = yield_low, ymax = yield_upp), alpha = 0.25) +
        labs(x = "Dose (Gy)", y = "Dicentrics/cells") +
        theme_bw()

      # Return object
      return(gg_curve)
    }

    # Perform calculations ----
    fit_results_list <- get_fit_results(count_data, model_formula, model_family, fit_link = "identity")
    gg_curve <- get_dose_curve(fit_results_list)

    # Make list of results to return
    results_list <- fit_results_list
    results_list[["gg_curve"]] <- gg_curve

    return(results_list)
  })

  # Results outputs ----
  output$fit_formula_tex <- renderUI({
    # Fitting formula
    if (input$button_fit <= 0) return(NULL)
    withMathJax(paste0("$$", data()[["fit_formula_tex"]], "$$"))
  })

  output$fit_model_summary <- renderUI({
    # Fitting formula
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_model_summary"]]
  })

  output$fit_model_statistics <- renderRHandsontable({
    # Model-level statistics
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_model_statistics"]] %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 70)
  })

  output$fit_coeffs <- renderRHandsontable({
    # Coefficients 'fit_coeffs'
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_coeffs"]] %>%
      formatC(format = "e", digits = 3) %>%
      as.data.frame() %>%
      dplyr::select(-statistic) %>%
      as.matrix() %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(halign = "htRight")
  })

  output$fit_var_cov_mat <- renderRHandsontable({
    # Variance-covariance matrix 'var_cov_mat'
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_var_cov_mat"]] %>%
      formatC(format = "e", digits = 3) %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(halign = "htRight")
  })

  output$fit_cor_mat <- renderRHandsontable({
    # Correlation matrix 'cor_mat'
    if (input$button_fit <= 0) return(NULL)
    data()[["fit_cor_mat"]] %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 100) %>%
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
        write.csv(hot_to_r(input$count_data_hot), file, row.names = FALSE)
      } else if (input$save_count_data_format == ".tex") {
        print(xtable::xtable(hot_to_r(input$count_data_hot)), type = "latex", file)
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
        results_list <- data()
        results_list[["gg_curve"]] <- NULL
        saveRDS(results_list, file = file)
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
      paste("dicentrics-fitting-report-", Sys.Date(), ".html", sep = "")
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
        fit_results_list = data()
      )

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}
