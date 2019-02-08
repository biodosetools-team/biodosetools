# Dose Estimateion Modules ------------------------------------------

estimateUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabItem(
    tabName = label,
    h2("Dose estimation"),

    fluidRow(
      # box: Curve fitting options ----
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
              bsButton(
                ns("help_input_fit_data"),
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
                withMathJax(includeMarkdown("help/help_input_fit_data.md"))
              )
            ),
            # Load from file ----
            conditionalPanel(
              condition = "input.load_fit_data_check",
              ns = ns,
              fileInput(ns("load_fit_data"), label = "File input"),
              # Help button
              bsButton(
                ns("help_load_fit_data"),
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
                withMathJax(includeMarkdown("help/help_load_fit_data.md"))
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
      # tabBox: Curve fitting overview ----
      tabBox(
        width = 7,
        side = "left",
        tabPanel(
          title = "Result of curve fit",
          # h4("Fit summary"),
          # verbatimTextOutput(ns("fit_results")),
          h4("Fit formula"),
          verbatimTextOutput(ns("fit_formula")),
          h4("Coefficients"),
          rHandsontableOutput(ns("fit_coeffs"))
        ),
        tabPanel(
          h4("Model-level statistics"),
          rHandsontableOutput(ns("fit_statistics")),
          title = "Summary statistics",
          h4("Correlation matrix"),
          rHandsontableOutput(ns("cor_mat")),
          h4("Variance-covariance matrix"),
          rHandsontableOutput(ns("var_cov_mat"))
        )
      )
    ),

    fluidRow(
      # box: Data input options ----
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
              bsButton(
                ns("help_input_cases_data"),
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
                withMathJax(includeMarkdown("help/help_input_cases_data.md"))
              )
            ),
            conditionalPanel(
              condition = "input.load_cases_data_check",
              ns = ns,
              fileInput(ns("load_cases_data"), label = "File input"),
              # Help button
              bsButton(
                ns("help_load_cases_data"),
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
                withMathJax(includeMarkdown("help/help_load_cases_data.md"))
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
      # hot: Cases input ----
      box(
        width = 7,
        title = "Data input",
        status = "primary", solidHeader = F, collapsible = T, collapsed = F,
        rHandsontableOutput(ns("hotable"))
      ),

      # box: Estimation options ----
      box(
        width = 7,
        title = "Dose estimation options",
        status = "warning", solidHeader = F, collapsible = T, collapsed = F,

        # Assessment selection
        div(
          class = "side-widget-tall",
          selectInput(
            ns("assessment_select"),
            label = "Assessment",
            # label = NULL,
            width = "150px",
            choices = list(
              "Whole body" = "whole-body",
              "Partial" = "partial",
              "Heterogeneous" = "hetero"
            ),
            selected = "whole-body"
          )
        ),
        # Help button
        bsButton(
          ns("help_assessment"),
          # class = "side-widget",
          label = "",
          icon = icon("question"),
          style = "default", size = "default"
        ),
        bsModal(
          id = ns("help_assessment_dialog"),
          title = "Help: Assessment selection",
          trigger = ns("help_assessment"),
          size = "large",
          withMathJax(includeMarkdown("help/help_assessment.md"))
        ),
        div(class = "widget-sep", br()),

        # Curve method selection
        div(
          class = "side-widget-tall",
          selectInput(
            ns("curve_method_select"),
            label = "Error calculation",
            # label = NULL,
            width = "150px",
            choices = list(
              "Merkle's method" = "merkle",
              "Simple method" = "simple"
            ),
            selected = "simple"
          )
        ),
        # Help button
        bsButton(
          ns("help_curve_method"),
          # class = "side-widget",
          label = "",
          icon = icon("question"),
          style = "default", size = "default"
        ),
        bsModal(
          id = ns("help_assessment_dialog"),
          title = "Help: Assessment selection",
          trigger = ns("help_curve_method"),
          size = "large",
          withMathJax(includeMarkdown("help/help_assessment.md"))
        ),

        # Gamma selection
        conditionalPanel(
          condition = "input.assessment_select != 'whole-body'",
          ns = ns,
          splitLayout(
            cellWidths = c("50%", "50%"),
            numericInput(
              ns("gamma"), "Gamma",
              value = 0.3706479, step = 0.01
            ),
            numericInput(
              ns("gamma_error"), "Std. error Gamma",
              value = 0.009164707, step = 0.0001
            )
          ),
          # Help button
          bsButton(
            ns("help_gamma"),
            # class = "side-widget-tall",
            label = "",
            icon = icon("question"),
            style = "default", size = "default"
          )
        ),
        br(),
        actionButton(ns("button_estimate"), class = "options-button", "Estimate dose")
      )
    ),

    fluidRow(
      # tabBox: Estimations ----
      column(
        width = 6,
        tabBox(
          width = 12,
          side = "left",
          selected = "Partial/Heterogeneous",
          tabPanel(
            title = "Whole body",
            h4("Estimated dose"),
            rHandsontableOutput(ns("est_doses_whole"))
          ),
          tabPanel(
            title = "Partial/Heterogeneous",
            h4("Observed fraction of irradiated cells and its yield"),
            rHandsontableOutput(ns("est_yields")),
            h4("Dose recieved by the irradiated fraction"),
            div(
              class = "side-widget",
              rHandsontableOutput(ns("est_doses"))
            ),
            bsButton(
              ns("help_mixed_yields"),
              # class = "rightAlign",
              label = "",
              icon = icon("question"),
              style = "default", size = "default"
            ),
            bsModal(
              id = ns("help_mixed_yields_dialog"),
              title = "Help: Assessment selection",
              trigger = ns("help_mixed_yields"),
              size = "large",
              withMathJax(includeMarkdown("help/help_mixed_yields.md"))
            ),
            h4("Initial fraction of irradiated cells"),
            rHandsontableOutput(ns("est_frac"))
          )
        )
      ),
      # box: Plot curves ----
      column(
        width = 6,
        box(
          width = 12,
          title = "Curve plot",
          status = "success", solidHeader = F, collapsible = T, collapsed = F,
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
      # Base data frame
      full_data <- data.frame(
        matrix(
          0,
          nrow = num_cases,
          ncol = num_dicentrics
        )
      )

      colnames(full_data) <- paste0("C", seq(0, num_dicentrics - 1, 1))
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
          y = 0,
          y_err = 0,
          DI = 0,
          u = 0
        ) %>%
        # select(D, N, X, everything())
        select(N, X, everything())

      first_dicent_index <- 3
      last_dicent_index <- ncol(mytable) - 4
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
        mytable[row, "y"] <- mytable[row, "X"] / mytable[row, "N"]
        mytable[row, "y_err"] <- sqrt(var / mytable[row, "N"])
      }

      return(mytable)
    }
  })

  # Output ----
  output$hotable <- renderRHandsontable({
    # Read number of columns
    num_cols <- ncol(changed_data())

    #
    hot <- changed_data() %>%
      rhandsontable() %>%
      hot_cols(colWidths = 50) %>%
      hot_col(c(1, 2, seq(num_cols - 3, num_cols, 1)), readOnly = TRUE) %>%
      hot_col(ncol(changed_data()), renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value > 1.96) {
              td.style.background = 'pink';
             }
           }")
    # hot_table(highlightCol = TRUE, highlightRow = TRUE)

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

    # Make list of results to return
    results_list <- list(
      fit_results = fit_results,
      fit_coeffs = fit_coeffs,
      var_cov_mat = var_cov_mat,
      cor_mat = cor_mat
    )

    return(results_list)
  })

  # Results outputs ----
  output$fit_results <- renderPrint({
    # Result of curve fit 'fit_results'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_results"]]
  })

  output$fit_formula <- renderPrint({
    # Fitting formula
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_results"]]$formula
    # TODO: transform this into LaTeX output?
  })

  output$fit_statistics <- renderRHandsontable({
    # Model-level statistics using broom::glance
    if (input$button_view_fit_data <= 0) return(NULL)
    broom::glance(data()[["fit_results"]]) %>%
      select(logLik, df.null, df.residual, null.deviance, deviance, AIC, BIC) %>%
      mutate(null.deviance = as.character(null.deviance)) %>%
      rhandsontable()
  })

  output$fit_coeffs <- renderRHandsontable({
    # Coefficients 'fit_coeffs'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_coeffs"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 75) %>%
      hot_cols(format = "0.000")
  })

  output$var_cov_mat <- renderRHandsontable({
    # Variance-covariance matrix 'var_cov_mat'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["var_cov_mat"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.0000000")
  })

  output$cor_mat <- renderRHandsontable({
    # Correlation matrix 'cor_mat'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["cor_mat"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })
}


estimateMixedResults <- function(input, output, session, stringsAsFactors) {
  data <- reactive({
    # Calcs: get variables ----
    input$button_estimate

    isolate({
      # Fit data
      load_fit_data <- input$load_fit_data_check
      fit_data <- input$load_fit_data
      assessment <- input$assessment_select
      curve_method <- input$curve_method_select

      # Cases data
      cases_data <- hot_to_r(input$hotable)

      aberr <- cases_data[["X"]]
      cell <- cases_data[["N"]]
      yield_obs <- cases_data[["y"]]

      counts <- cases_data[1, ] %>%
        select(contains("C")) %>%
        as.numeric()

      # Gamma input
      gamma <- input$gamma
      gamma_error <- input$gamma_error
    })

    # Get fitting data ----
    if (load_fit_data) {
      fit_results <- readRDS(fit_data$datapath)
    }

    # Summarise fit
    fit_summary <- summary(fit_results, correlation = TRUE)
    # cor_mat <- fit_summary$correlation
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
    x0 <- fit_results$data[[1]]
    x1 <- fit_results$data[[2]]

    curves_data <- data.frame(dose = seq(0, max(x1 / x0), length.out = 100)) %>%
      mutate(
        yield = yield_fun(dose),
        yield_low = yield_fun(dose) - R_factor * yield_error_fun(dose),
        yield_upp = yield_fun(dose) + R_factor * yield_error_fun(dose)
      )

    # Make base plot
    gg_curve <- ggplot(plot_data / x0) +
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


    # Get cases data ----
    # Data test is stored in vector y
    y <- rep(seq(0, length(counts) - 1, 1), counts)
    x <- c(rep(1, length(y)))
    fit <- mixtools::poisregmixEM(y, x, addintercept = F, k = 2)

    # Input of the parameters of the dose-effect linear-quadratic model
    beta0 <- fit_coeffs[1, "Estimate"]
    beta1 <- fit_coeffs[2, "Estimate"]
    beta2 <- fit_coeffs[3, "Estimate"]

    # Input of the Variance-covariance matrix of the parameters
    sigma <- numeric(49)
    dim(sigma) <- c(7, 7)
    sigma[1, 1] <- var_cov_mat[1, 1]
    sigma[2, 2] <- var_cov_mat[2, 2]
    sigma[3, 3] <- var_cov_mat[3, 3]
    sigma[1, 2] <- var_cov_mat[1, 2]
    sigma[1, 3] <- var_cov_mat[1, 3]
    sigma[2, 3] <- var_cov_mat[2, 3]
    sigma[2, 1] <- sigma[1, 2]
    sigma[3, 1] <- sigma[1, 3]
    sigma[3, 2] <- sigma[2, 3]

    # Input of the parameter gamma and its variance
    gam <- gamma
    sigma[4, 4] <- gamma_error


    # Projection functions ----
    project_yield_base <- function(yield) {
      uniroot(function(dose) {
        yield_fun(dose) - yield
      }, c(0.1, 30))$root
    }

    if (curve_method == "merkle") {
      project_yield_lower <- function(yield) {
        uniroot(function(dose) {
          yield_fun(dose) + R_factor * yield_error_fun(dose) - yield
        }, c(0.1, 30))$root
      }

      project_yield_upper <- function(yield) {
        uniroot(function(dose) {
          yield_fun(dose) - R_factor * yield_error_fun(dose) - yield
        }, c(0.1, 30))$root
      }
    } else if (curve_method == "simple") {
      project_yield_lower <- function(yield) {
        uniroot(function(dose) {
          yield_fun(dose) - yield
        }, c(0.1, 30))$root
      }

      project_yield_upper <- function(yield) {
        uniroot(function(dose) {
          yield_fun(dose) - yield
        }, c(0.1, 30))$root
      }
    }


    # Calcs: whole-body estimation ----

    gardner_confidence_table <- data.table::fread("libs/gardner-confidence-table.csv")

    aberr_row <- gardner_confidence_table[which(gardner_confidence_table[["S"]] == round(aberr, 0)), ]

    aberr_obs_l <- aberr_row[["Sl"]]
    aberr_obs_u <- aberr_row[["Su"]]

    yield_obs_l <- aberr_obs_l / cell
    yield_obs_u <- aberr_obs_u / cell

    # Calculate projections
    x_whole <- project_yield_base(yield_obs)
    x_l_whole <- project_yield_lower(yield_obs_l)
    x_u_whole <- project_yield_upper(yield_obs_u)

    # Whole-body estimation results
    est_doses_whole <- data.frame(
      yield = c(yield_obs_l, yield_obs, yield_obs_u),
      dose = c(x_l_whole, x_whole, x_u_whole)
    )

    row.names(est_doses_whole) <- c("lower", "base", "upper")


    # Calcs: mixed dose estimation ----

    # First parameter is the mixing proportion
    # the second and third parameters are the yields
    loglik <- function(b) {
      loglik <- sum(log(b[1] * dpois(y, b[2]) + (1 - b[1]) * dpois(y, b[3])))

      return(-loglik)
    }

    MLE <- optim(
      c(fit$lambda[1], exp(fit$beta)[1], exp(fit$beta)[2]),
      loglik,
      method = c("L-BFGS-B"),
      lower = c(0.01, 0.01, 0.01), upper = c(0.99, Inf, Inf), hessian = T
    )

    st <- solve(MLE$hessian)
    m1 <- MLE$par[2]
    m2 <- MLE$par[3]
    f1 <- MLE$par[1]

    if (m1 < m2) {
      m1 <- MLE$par[3]
      m2 <- MLE$par[2]
      f1 <- 1 - f1
      stm <- st
      stm[2, 2] <- st[3, 3]
      stm[3, 3] <- st[2, 2]
      stm[1, 2] <- st[1, 3]
      stm[1, 3] <- st[1, 2]
      stm[2, 1] <- stm[1, 2]
      stm[3, 1] <- stm[1, 3]
      st <- stm
    }

    sigma[5, 5] <- st[1, 1]
    sigma[6, 6] <- st[2, 2]
    sigma[7, 7] <- st[3, 3]
    sigma[5, 6] <- st[1, 2]
    sigma[5, 7] <- st[1, 3]
    sigma[6, 7] <- st[2, 3]
    sigma[6, 5] <- st[1, 2]
    sigma[7, 5] <- st[1, 3]
    sigma[7, 6] <- st[2, 3]

    # Estimated parameters and its standard errors
    # First parameter is the mixing proportion
    # the second and third parameters are the yields
    estim <- c(f1, m1, m2)
    std_estim <- sqrt(diag(st))

    est_yields <- data.frame(
      y_estimate = c(estim[2], estim[3]),
      y_std_err = c(std_estim[2], std_estim[3]),
      f_estimate = c(estim[1], 1 - estim[1]),
      f_std_err = rep(std_estim[1], 2)
    )

    row.names(est_yields) <- c("x1", "x2")

    # Estimated received doses
    x1 <- project_yield_base(m1)

    if (m2 <= 0.01) {
      x2 <- 0
    } else {
      x2 <- project_yield_base(m2)
    }

    est_doses <- data.frame(
      x1 = x1,
      x2 = x2
    )

    frac <- function(g, f, mu1, mu2) {
      x1 <- project_yield_base(mu1)

      if (mu2 <= 0.01) {
        x2 <- 0
      } else {
        x2 <- project_yield_base(mu2)
      }

      frac <- f / (f + (1 - f) * exp(g * (x2 - x1)))

      return(frac)
    }

    # Estimated fraction of irradiated blood for dose x1
    est_F1 <- frac(gam, f1, m1, m2)
    est_F2 <- 1 - est_F1

    # Approximated standard error
    std_err_F1 <- est_F1 * (1 - est_F1) * sqrt((x2 - x1)^2 * sigma[4, 4] + st[1, 1] / (f1^2 * (1 - f1)^2))

    est_frac <- data.frame(
      estimate = c(est_F1, est_F2),
      std_err = rep(std_err_F1, 2)
    )

    row.names(est_frac) <- c("x1", "x2")

    # Gradient
    # h <- 0.000001
    # if (m2 > 0.01) {
    #   c1 <- (frac(beta0 + h, beta1, beta2, gam, f1, m1, m2) - F) / h
    #   c2 <- (frac(beta0, beta1 + h, beta2, gam, f1, m1, m2) - F) / h
    #   c3 <- (frac(beta0, beta1, beta2 + h, gam, f1, m1, m2) - F) / h
    #   c5 <- (frac(beta0, beta1, beta2, gam + h, f1, m1, m2) - F) / h
    #   c6 <- (frac(beta0, beta1, beta2, gam, f1 + h, m1, m2) - F) / h
    #   c7 <- (frac(beta0, beta1, beta2, gam, f1, m1 + h, m2) - F) / h
    #   c8 <- (frac(beta0, beta1, beta2, gam, f1, m1, m2 + h) - F) / h
    #   grad <- c(c1, c2, c3, c5, c6, c7, c8)
    #   sqrt(t(grad) %*% sigma %*% grad)
    # }
    #
    # if (m2 <= 0.01) {
    #   c1 <- (frac(beta0 + h, beta1, beta2, gam, f1, m1, m2) - F) / h
    #   c2 <- (frac(beta0, beta1 + h, beta2, gam, f1, m1, m2) - F) / h
    #   c3 <- (frac(beta0, beta1, beta2 + h, gam, f1, m1, m2) - F) / h
    #   c5 <- (frac(beta0, beta1, beta2, gam + h, f1, m1, m2) - F) / h
    #   c6 <- (frac(beta0, beta1, beta2, gam, f1 + h, m1, m2) - F) / h
    #   c7 <- (frac(beta0, beta1, beta2, gam, f1, m1 + h, m2) - F) / h
    #   grad <- c(c1, c2, c3, c5, c6, c7)
    #   sigma2 <- sigma[1:6, 1:6]
    #   sqrt(t(grad) %*% sigma2 %*% grad)
    # }


    # Update plot ----

    gg_curve <- gg_curve +
      # Estimated whole-body doses
      geom_point(data = est_doses_whole, aes(x = dose, y = yield))

    # Make list of results to return
    results_list <- list(
      est_doses_whole = est_doses_whole,
      est_yields = est_yields,
      est_doses = est_doses,
      est_frac = est_frac,
      gg_curve = gg_curve
    )

    return(results_list)
  })

  # Results outputs ----
  output$est_yields <- renderRHandsontable({
    # Estimated yields
    if (input$button_estimate <= 0) return(NULL)
    data()[["est_yields"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_doses_whole <- renderRHandsontable({
    # Estimated recieved doses (whole-body)
    if (input$button_estimate <= 0) return(NULL)
    data()[["est_doses_whole"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_doses <- renderRHandsontable({
    # Estimated recieved doses (partial/heterogeneous)
    if (input$button_estimate <= 0) return(NULL)
    data()[["est_doses"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$est_frac <- renderRHandsontable({
    # Estimated fraction of irradiated blood for dose x1
    if (input$button_estimate <= 0) return(NULL)
    data()[["est_frac"]] %>%
      rhandsontable() %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  output$plot <- renderPlot(
    # Plot of the data and fitted curve
    res = 120, {
      if (input$button_estimate <= 0) return(NULL)
      data()[["gg_curve"]]
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
  # output$save_report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = function() {
  #     paste("report-", Sys.Date(), ".html", sep = "")
  #   },
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     normReport <- file.path("report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #
  #     # Set up parameters to pass to Rmd document
  #     params <- list(
  #       fit_result = data()[["fit_results"]],
  #       fit_coeffs = data()[["fit_coeffs"]],
  #       var_cov_mat = data()[["var_cov_mat"]],
  #       cor_mat = data()[["cor_mat"]],
  #       gg_curve = data()[["gg_curve"]]
  #     )
  #
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport,
  #                       output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
}
