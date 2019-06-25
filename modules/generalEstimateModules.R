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
                                     response = "yield", link = "identity", type = "theory") {
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

generalEstimateCaseHotTable <- function(input, output, session, stringsAsFactors, aberr_module, fraction_value = NULL) {

  # Reset table ----
  table_reset <- reactiveValues(value = 0)

  observeEvent(input$button_upd_table, {
    table_reset$value <- 1
  })

  # Translocation confounder function ----
  get_translocation_rate <- function(cells, fraction, age_value,
                                     sex_bool = FALSE, smoker_bool = FALSE,
                                     race_value = "none", region_value = "none") {
    age_trans_frequency <- function(age) {
      trans_frequency <- exp(-7.925) + exp(-9.284) * (age * exp(0.01062 * age))
      return(trans_frequency)
    }

    sex_trans_list <- c(1, 1, 0.92) %>%
      `names<-`(c("none", "male", "female"))
    smoke_trans_list <- c(1, 1.19) %>%
      `names<-`(c("FALSE", "TRUE"))
    race_trans_list <- c(1, 1, 1.23, 0.84, 1.06) %>%
      `names<-`(c("none", "white", "asian", "black", "other"))
    region_trans_list <- c(1, 1, 0.99, 1.75, 1.75, 0.86) %>%
      `names<-`(c("none", "n-america", "w-europe", "c-europe", "e-europe", "asia"))

    sex_value <- ifelse(sex_bool, input$trans_sex, "none")

    sex_trans_frequency <- sex_trans_list[[sex_value]]
    smoke_trans_frequency <- smoke_trans_list[[as.character(smoker_bool)]]
    race_trans_frequency <- race_trans_list[[race_value]]
    region_trans_frequency <- region_trans_list[[region_value]]

    expected_aberr <- cells * fraction *
      age_trans_frequency(age_value) *
      sex_trans_frequency *
      smoke_trans_frequency *
      race_trans_frequency *
      region_trans_frequency

    return(expected_aberr)
  }

  # Initialize data frame ----
  previous <- reactive({

    # Create button dependency for updating dimensions
    input$button_upd_table

    isolate({
      load_case_data <- input$load_case_data_check
      case_data <- input$load_case_data
      # num_cases <- as.numeric(input$num_cases)
      num_cases <- 1
      num_aberrs <- as.numeric(input$num_aberrs) + 1
    })

    if (!load_case_data) {
      # Base data frame
      full_data <- data.frame(
        matrix(
          0,
          nrow = num_cases,
          ncol = num_aberrs
        )
      ) %>%
        `colnames<-`(paste0("C", seq(0, num_aberrs - 1, 1)))

    } else {
      full_data <- read.csv(case_data$datapath, header = TRUE) %>%
        dplyr::mutate_at(vars(starts_with("C")), as.integer)
    }

    return(full_data)
  })

  # Reactive data frame ----
  changed_data <- reactive({
    # Create button dependency for updating dimensions
    input$button_upd_table
    input$button_upd_params

    isolate({
      if (is.null(input$case_data_hot) || isolate(table_reset$value == 1)) {
        table_reset$value <- 0
        mytable <- previous()

        # Initial renderization of the table
        if (aberr_module == "dicentrics") {
          mytable <- mytable %>%
            dplyr::mutate(
              N = 0,
              X = 0,
              y = 0,
              y_err = 0,
              DI = 0,
              u = 0
            ) %>%
            dplyr::select(N, X, everything()) %>%
            dplyr::mutate_at(
              c("X", "N", grep("C", names(.), value = TRUE)),
              as.integer
            )
        } else if (aberr_module == "translocations") {
          mytable <- mytable %>%
            dplyr::mutate(
              N = 0,
              X = 0,
              Xt = 0,
              y = 0,
              y_err = 0,
              yt = 0,
              yt_err = 0,
              DI = 0,
              u = 0
            ) %>%
            dplyr::select(N, X, Xt, everything()) %>%
            dplyr::mutate_at(
              c("X", "N", grep("C", names(.), value = TRUE)),
              as.integer
            )
        }

      } else if (!identical(previous(), input$case_data_hot)) {
        mytable <- as.data.frame(hot_to_r(input$case_data_hot))

        # Calculated columns
        if (aberr_module == "dicentrics") {
          mytable <- mytable %>%
            dplyr::mutate(
              N = 0,
              X = 0,
              y = 0,
              y_err = 0,
              DI = 0,
              u = 0
            ) %>%
            dplyr::select(N, X, everything())
        } else if (aberr_module == "translocations") {
          mytable <- mytable %>%
            dplyr::mutate(
              N = 0,
              X = 0,
              Xt = 0,
              y = 0,
              y_err = 0,
              yt = 0,
              yt_err = 0,
              DI = 0,
              u = 0
            ) %>%
            dplyr::select(N, X, Xt, everything())
        }

        # Index variables
        if (aberr_module == "dicentrics") {
          first_trans_index <- 3
          last_trans_index <- ncol(mytable) - 4
        } else if (aberr_module == "translocations") {
          first_trans_index <- 4
          last_trans_index <- ncol(mytable) - 6
        }

        num_rows <- nrow(mytable)

        mytable <- mytable %>%
          dplyr::mutate(
            X = as.integer(X),
            N = as.integer(rowSums(.[first_trans_index:last_trans_index]))
          )

        # Ugly method to calculate index of dispersion
        for (row in 1:num_rows) {
          xf <- 0
          x2f <- 0
          # Calculate summatories
          for (k in seq(0, last_trans_index - first_trans_index, 1)) {
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

        # Calculate expected translocation rate
        if (aberr_module == "translocations") {
          fraction <- fraction_value$frac()
          mytable <- mytable %>%
            dplyr::mutate(
              Xt = ifelse(
                input$trans_confounders,
                get_translocation_rate(
                  N, fraction,
                  age_value = input$trans_confounder_age,
                  sex_bool = input$trans_confounder_sex,
                  smoker_bool = input$trans_confounder_smoke,
                  race_value = input$trans_confounder_race,
                  region_value = input$trans_confounder_region
                ),
                0
              ),
              yt = (X - Xt) / (N * fraction),
              yt_err = 0
            )
        }

        return(mytable)
      }
    })
  })

  # Output ----
  output$case_data_hot <- renderRHandsontable({
    # Read number of columns
    num_cols <- ncol(changed_data())

    # Convert to hot and format table
    hot <- changed_data() %>%
      rhandsontable(width = "100%", height = "100%") %>%
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
