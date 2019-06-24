# General Estimate Modules ---------------------------------

generalEstimateFittingCurveHotTables <- function(input, output, session, stringsAsFactors) {

  # Reset tables ----
  table_reset <- reactiveValues(value = 0)
  table_var_reset <- reactiveValues(value = 0)

  observeEvent(input$button_gen_table, {
    table_reset$value <- 1
    table_var_reset$value <- 1
  })

  # Initialize data frames ----
  previous_coeffs <- reactive({

    # Create button dependency for updating dimensions
    input$button_gen_table

    isolate({
      formula_select <- input$formula_select
    })

    if (formula_select == "lin-quad") {
      fit_coeffs_names <- c("C", "α", "β")
    } else if (formula_select == "lin-quad-no-int") {
      fit_coeffs_names <- c("α", "β")
    } else if (formula_select == "lin") {
      fit_coeffs_names <- c("C", "α")
    } else if (formula_select == "lin-no-int") {
      fit_coeffs_names <- c("α")
    }

    full_data <- data.frame(
      matrix(
        0.0,
        nrow = length(fit_coeffs_names),
        ncol = 2
      )
    ) %>%
      `row.names<-`(fit_coeffs_names) %>%
      `colnames<-`(c("estimate", "std.error"))

    return(full_data)
  })

  previous_var <- reactive({

    # Create button dependency for updating dimensions
    input$button_gen_table

    isolate({
      model_formula <- input$formula_select
    })

    if (model_formula == "lin-quad") {
      fit_coeffs_names <- c("C", "α", "β")
    } else if (model_formula == "lin-quad-no-int") {
      fit_coeffs_names <- c("α", "β")
    } else if (model_formula == "lin") {
      fit_coeffs_names <- c("C", "α")
    } else if (model_formula == "lin-no-int") {
      fit_coeffs_names <- c("α")
    }

    full_data <- matrix(
      0.0,
      nrow = length(fit_coeffs_names),
      ncol = length(fit_coeffs_names)
    ) %>%
      `row.names<-`(fit_coeffs_names) %>%
      `colnames<-`(fit_coeffs_names) %>%
      as.data.frame()

    return(full_data)
  })

  # Reactive data frames ----
  changed_coeffs_data <- reactive({
    # Create button dependency for updating dimensions
    input$button_gen_table

    if (is.null(input$fit_coeffs_hot) || isolate(table_reset$value == 1)) {
      table_reset$value <- 0
      return(previous_coeffs())
    } else if (!identical(previous_coeffs(), input$fit_coeffs_hot)) {
      fit_coeffs_names <- row.names(hot_to_r(input$fit_coeffs_hot))

      mytable <- as.data.frame(hot_to_r(input$fit_coeffs_hot)) %>%
        dplyr::mutate_all(as.numeric) %>%
        `row.names<-`(fit_coeffs_names)

      return(mytable)
    }
  })

  changed_var_data <- reactive({
    # Create button dependency for updating dimensions
    input$button_gen_table

    if (is.null(input$fit_var_cov_mat_hot) || isolate(table_var_reset$value == 1)) {
      table_var_reset$value <- 0
      return(previous_var())
    } else if (!identical(previous_var(), input$fit_var_cov_mat_hot)) {
      fit_coeffs_names <- row.names(hot_to_r(input$fit_var_cov_mat_hot))

      mytable <- as.data.frame(hot_to_r(input$fit_var_cov_mat_hot)) %>%
        dplyr::mutate_all(as.numeric) %>%
        `row.names<-`(fit_coeffs_names)

      return(mytable)
    }
  })

  # Output ----
  output$fit_coeffs_hot <- renderRHandsontable({
    # Read number of columns
    num_cols <- ncol(changed_coeffs_data())

    # Convert to hot and format table
    hot <- changed_coeffs_data() %>%
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.000000")

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })

  output$fit_var_cov_mat_hot <- renderRHandsontable({
    # Read number of columns
    num_cols <- ncol(changed_var_data())

    # Convert to hot and format table
    hot <- changed_var_data() %>%
      rhandsontable(width = "100%", height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.00000000")

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })
}


generalEstimateFittingCurve <- function(input, output, session, stringsAsFactors, aberr_module) {

  # Calculations ----
  data <- reactive({
    input$button_view_fit_data

    isolate({
      load_fit_data <- input$load_fit_data_check
      fit_data <- input$load_fit_data
    })

    # Model statistic function for translocations
    get_model_statistics <- function(model_data, fit_coeffs_vec, genome_fraction,
                                     response = "yield", link = "identity") {
      # Calculate from theory or use statistics calculated by glm
      if (type == "theory") {
        # Renormalize data if necessary
        if (response == "yield") {
          model_data[["X"]] <- model_data[["X"]] / (model_data[["N"]] *  genome_fraction)
        }

        # Generalized variance-covariance matrix
        general_fit_coeffs <- numeric(length = 3L) %>%
          `names<-`(c("C", "α", "β"))

        for (var in names(fit_coeffs_vec)) {
          general_fit_coeffs[[var]] <- fit_coeffs_vec[[var]]
        }

        # Predict yield / aberrations
        predict_eta <- function(data, coeffs) {
          coeffs[["C"]] * rep(1, nrow(data)) +
            coeffs[["α"]] * data[["D"]]  +
            coeffs[["β"]] * data[["D"]] * data[["D"]]
        }

        eta_sat <- model_data[["X"]]
        eta <- predict_eta(model_data, general_fit_coeffs)

        num_data <- length(eta_sat)
        num_params <- sum(fit_coeffs_vec != 0)

        # Calculate logLik depending on fitting link
        if (link == "identity") {
          logLik <- sum(log(eta) * eta_sat - eta - log(factorial(eta_sat)))
        } else if (link == "log") {
          logLik <- sum(eta * eta_sat - exp(eta) - log(factorial(eta_sat)))
        }

        # Calculate model-specific statistics
        fit_model_statistics <- cbind(
          logLik =   logLik,
          deviance = sum(2 * (eta_sat * log(eta_sat / eta) - (eta_sat - eta))),
          df =       num_data - num_params,
          AIC =      2 * num_params - 2 * logLik,
          BIC =      log(num_data) * num_params - 2 * logLik
        )
      }

      return(fit_model_statistics)
    }

    if (load_fit_data) {
      fit_results_list <- readRDS(fit_data$datapath)

      genome_fraction <- fit_results_list[["genome_frac"]]

      # Additional info for translocations module
      if (aberr_module == "translocations") {

        # Message about used translocation frequency
        if (fit_results_list[["frequency_select"]] == "measured_freq") {
          trans_frequency_message <- paste0("The provided observed fitting curve has been converted to full genome, with a genomic conversion factor of ", round(genome_fraction, 3), ".")
        } else {
          trans_frequency_message <- "The provided fitting curve is already full genome."
        }
        fit_results_list[["fit_trans_frequency_message"]] <- trans_frequency_message

        # Conversion of coefficients and statistics
        if (fit_results_list[["frequency_select"]] == "measured_freq") {

          # Update coefficients
          fit_results_list[["fit_coeffs"]][,"estimate"] <- fit_results_list[["fit_coeffs"]][,"estimate"] / genome_fraction
          fit_results_list[["fit_coeffs"]][,"std.error"] <- fit_results_list[["fit_coeffs"]][,"std.error"] / genome_fraction

          # Update variance-covariance matrix
          fit_results_list[["fit_var_cov_mat"]] <- fit_results_list[["fit_var_cov_mat"]] / genome_fraction^2

          # Update model-specific statistics
          fit_results_list[["fit_model_statistics"]] <- get_model_statistics(
            fit_results_list[["fit_raw_data"]],
            fit_results_list[["fit_coeffs"]][,"estimate"],
            genome_fraction
          )
        }
      }
    } else {
      model_formula <- input$formula_select
      # Parse formula
      if (model_formula == "lin-quad") {
        fit_formula_tex <- "Y = C + \\alpha D + \\beta D^{2}"
      } else if (model_formula == "lin") {
        fit_formula_tex <- "Y = C + \\alpha D"
      }
      else if (model_formula == "lin-quad-no-int") {
        fit_formula_tex <- "Y = \\alpha D + \\beta D^{2}"
      }
      else if (model_formula == "lin-no-int") {
        fit_formula_tex <- "Y = \\alpha D"
      }

      fit_coeffs_raw <- hot_to_r(input$fit_coeffs_hot)
      fit_coeffs <- fit_coeffs_raw %>%
        cbind(statistic = c(rep(0, nrow(fit_coeffs_raw)))) %>%
        as.matrix()

      fit_var_cov_mat <- hot_to_r(input$fit_var_cov_mat_hot) %>%
        as.matrix()

      fit_cor_mat <- fit_var_cov_mat
      for (x_var in rownames(fit_var_cov_mat)) {
        for (y_var in colnames(fit_var_cov_mat)) {
          fit_cor_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var] /
            (fit_coeffs[x_var, "std.error"] * fit_coeffs[y_var, "std.error"])
        }
      }

      fit_results_list <- list(
        fit_formula_tex = fit_formula_tex,
        fit_coeffs = fit_coeffs,
        fit_var_cov_mat = fit_var_cov_mat,
        fit_cor_mat = fit_cor_mat
      )

      # Additional info for translocations module
      if (aberr_module == "translocations") {
        # Message about used translocation frequency
        if (input$frequency_select == "measured_freq") {
          trans_frequency_message <- paste0("The provided observed fitting curve has been converted to full genome, with a genomic conversion factor of ", input$fit_fraction_value, ".")
        } else {
          trans_frequency_message <- "The provided fitting curve is already full genome."
        }
        fit_results_list[["fit_trans_frequency_message"]] <- trans_frequency_message
      }
    }

    return(fit_results_list)
  })

  # Results outputs ----
  output$fit_formula_tex <- renderUI({
    # Fitting formula
    if (input$button_view_fit_data <= 0) return(NULL)
    withMathJax(paste0("$$", data()[["fit_formula_tex"]], "$$"))
  })

  output$fit_trans_frequency_message <- renderUI({
    # Fitting formula
    if (input$button_view_fit_data <= 0 | aberr_module != "translocations") return(NULL)
    data()[["fit_trans_frequency_message"]]
  })

  output$fit_model_statistics <- renderRHandsontable({
    # Model-level statistics
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_model_statistics"]] %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 70)
  })

  output$fit_coeffs <- renderRHandsontable({
    # Coefficients 'fit_coeffs'
    if (input$button_view_fit_data <= 0) return(NULL)
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
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_var_cov_mat"]] %>%
      formatC(format = "e", digits = 3) %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(halign = "htRight")
  })

  output$fit_cor_mat <- renderRHandsontable({
    # Correlation matrix 'cor_mat'
    if (input$button_view_fit_data <= 0) return(NULL)
    data()[["fit_cor_mat"]] %>%
      # Convert to hot and format table
      rhandsontable(width = 375, height = "100%") %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.000")
  })
}
