#' Dose Estimation Fitting Curve Hottable Server Module
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param stringsAsFactors stringsAsFactors.
#' @param aberr_module Aberration module.
#' @param genome_fraction Genomic fraction used in translocations.
#'
#' @import shiny rhandsontable
#' @noRd
mod_estimate_fit_curve_hot_server <- function(input, output, session, stringsAsFactors) {

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
      fit_coeffs_names <- c("coeff_C", "coeff_alpha", "coeff_beta")
    } else if (formula_select == "lin-quad-no-int") {
      fit_coeffs_names <- c("coeff_alpha", "coeff_beta")
    } else if (formula_select == "lin") {
      fit_coeffs_names <- c("coeff_C", "coeff_alpha")
    } else if (formula_select == "lin-no-int") {
      fit_coeffs_names <- c("coeff_alpha")
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
      fit_coeffs_names <- c("coeff_C", "coeff_alpha", "coeff_beta")
    } else if (model_formula == "lin-quad-no-int") {
      fit_coeffs_names <- c("coeff_alpha", "coeff_beta")
    } else if (model_formula == "lin") {
      fit_coeffs_names <- c("coeff_C", "coeff_alpha")
    } else if (model_formula == "lin-no-int") {
      fit_coeffs_names <- c("coeff_alpha")
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

    if (is.null(input$fit_coeffs_hot) | isolate(table_reset$value == 1)) {
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

    if (is.null(input$fit_var_cov_mat_hot) | isolate(table_var_reset$value == 1)) {
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
      fix_coeff_names(type = "rows", output = "rhot") %>%
      rhandsontable(
        width = (50 + num_cols * 100),
        height = "100%"
      ) %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.000000")

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })

  output$fit_var_cov_mat_hot <- renderRHandsontable({
    # Read number of columns
    num_cols <- ncol(changed_var_data())

    hot <- changed_var_data() %>%
      fix_coeff_names(type = "rows", output = "rhot") %>%
      fix_coeff_names(type = "cols", output = "rhot") %>%
      # Convert to hot and format table
      rhandsontable(
        width = (50 + num_cols * 100),
        height = "100%"
      ) %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.00000000")

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })
}

#' Dose Estimation Fitting Curve Server Module
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param stringsAsFactors stringsAsFactors.
#' @param aberr_module Aberration module.
#' @param genome_fraction Genomic fraction used in translocations.
#'
#' @import shiny rhandsontable
#' @noRd
mod_estimate_fit_curve_server <- function(input, output, session, stringsAsFactors, aberr_module) {

  # Calculations ----
  data <- reactive({
    input$button_view_fit_data

    isolate({
      load_fit_data <- input$load_fit_data_check
      fit_data <- input$load_fit_data
    })

    input$button_view_fit_data

    isolate({
      if (load_fit_data) {
        fit_results_list <- readRDS(fit_data$datapath)

        # Additional info for translocations module
        if (aberr_module == "translocations") {
          fit_genome_fraction <- fit_results_list[["genome_fraction"]]

          # Message about used translocation frequency
          if (fit_results_list[["frequency_select"]] == "measured_freq") {
            trans_frequency_message <- paste0("The provided observed fitting curve has been converted to full genome, with a genomic conversion factor of ", round(fit_genome_fraction, 3), ".")
          } else {
            trans_frequency_message <- "The provided fitting curve is already full genome."
          }
          fit_results_list[["fit_trans_frequency_message"]] <- trans_frequency_message

          # Conversion of coefficients and statistics
          if (fit_results_list[["frequency_select"]] == "measured_freq") {

            # Update coefficients
            fit_results_list[["fit_coeffs"]][, "estimate"] <- fit_results_list[["fit_coeffs"]][, "estimate"] / fit_genome_fraction
            fit_results_list[["fit_coeffs"]][, "std.error"] <- fit_results_list[["fit_coeffs"]][, "std.error"] / fit_genome_fraction

            # Update variance-covariance matrix
            fit_results_list[["fit_var_cov_mat"]] <- fit_results_list[["fit_var_cov_mat"]] / fit_genome_fraction^2

            # Update model-specific statistics
            fit_results_list[["fit_model_statistics"]] <- get_model_statistics(
              model_data = fit_results_list[["fit_raw_data"]],
              fit_coeffs_vec = fit_results_list[["fit_coeffs"]][, "estimate"],
              response = "yield", link = "identity", type = "theory",
              genome_fraction = fit_genome_fraction,
              aberr_module = aberr_module
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
          # cbind(statistic = c(rep(0, nrow(fit_coeffs_raw)))) %>%
          as.matrix()

        if (input$use_var_cov_matrix) {
          fit_var_cov_mat <- hot_to_r(input$fit_var_cov_mat_hot) %>%
            as.matrix()
        } else {
          # Calculate var-cov matrix when none is provided
          fit_var_cov_mat <- matrix(
            0,
            nrow = nrow(fit_coeffs),
            ncol = nrow(fit_coeffs)
          ) %>%
            `colnames<-`(rownames(fit_coeffs)) %>%
            `rownames<-`(rownames(fit_coeffs))

          for (x_var in rownames(fit_var_cov_mat)) {
            fit_var_cov_mat[[x_var, x_var]] <- fit_coeffs[x_var, "std.error"] * fit_coeffs[x_var, "std.error"]
          }
        }

        fit_cor_mat <- fit_var_cov_mat
        for (x_var in rownames(fit_var_cov_mat)) {
          for (y_var in colnames(fit_var_cov_mat)) {
            fit_cor_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var] /
              (fit_coeffs[x_var, "std.error"] * fit_coeffs[y_var, "std.error"])
          }
        }

        # Conversion of coefficients and statistics
        if (aberr_module == "translocations") {
          if (input$frequency_select == "measured_freq") {
            fit_genome_fraction <- input$fit_genome_fraction

            # Update coefficients
            fit_coeffs[, "estimate"] <- fit_coeffs[, "estimate"] / fit_genome_fraction
            fit_coeffs[, "std.error"] <- fit_coeffs[, "std.error"] / fit_genome_fraction

            # Update variance-covariance matrix
            fit_var_cov_mat <- fit_var_cov_mat / fit_genome_fraction^2
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
            trans_frequency_message <- paste0("The provided observed fitting curve has been converted to full genome, with a genomic conversion factor of ", input$fit_genome_fraction, ".")
          } else {
            trans_frequency_message <- "The provided fitting curve is already full genome."
          }
          fit_results_list[["fit_trans_frequency_message"]] <- trans_frequency_message
        }
      }
    })

    return(fit_results_list)
  })

  # Results outputs ----
  output$fit_formula_tex <- renderUI({
    # Fitting formula
    if (input$button_view_fit_data <= 0) {
      return(NULL)
    }
    withMathJax(paste0("$$", data()[["fit_formula_tex"]], "$$"))
  })

  output$fit_trans_frequency_message <- renderUI({
    # Fitting formula
    if (input$button_view_fit_data <= 0 | aberr_module != "translocations") {
      return(NULL)
    }
    data()[["fit_trans_frequency_message"]]
  })

  output$fit_model_statistics <- renderRHandsontable({
    # Model-level statistics
    if (input$button_view_fit_data <= 0) {
      return(NULL)
    }
    num_cols <- as.numeric(ncol(data()[["fit_model_statistics"]]))

    data()[["fit_model_statistics"]] %>%
      # Convert to hot and format table
      rhandsontable(
        width = (num_cols * 70),
        height = "100%"
      ) %>%
      hot_cols(colWidths = 70)
  })

  output$fit_coeffs <- renderRHandsontable({
    # Coefficients 'fit_coeffs'
    if (input$button_view_fit_data <= 0) {
      return(NULL)
    }
    num_cols <- as.numeric(ncol(data()[["fit_coeffs"]]))

    data()[["fit_coeffs"]] %>%
      formatC(format = "e", digits = 3) %>%
      fix_coeff_names(type = "rows", output = "rhot") %>%
      # Convert to hot and format table
      rhandsontable(
        width = (50 + num_cols * 100),
        height = "100%"
      ) %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(halign = "htRight")
  })

  output$fit_var_cov_mat <- renderRHandsontable({
    # Variance-covariance matrix 'var_cov_mat'
    if (input$button_view_fit_data <= 0) {
      return(NULL)
    }
    num_cols <- as.numeric(ncol(data()[["fit_var_cov_mat"]]))

    data()[["fit_var_cov_mat"]] %>%
      formatC(format = "e", digits = 3) %>%
      fix_coeff_names(type = "rows", output = "rhot") %>%
      fix_coeff_names(type = "cols", output = "rhot") %>%
      # Convert to hot and format table
      rhandsontable(
        width = (50 + num_cols * 100),
        height = "100%"
      ) %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(halign = "htRight")
  })

  output$fit_cor_mat <- renderRHandsontable({
    # Correlation matrix 'cor_mat'
    if (input$button_view_fit_data <= 0) {
      return(NULL)
    }
    num_cols <- as.numeric(ncol(data()[["fit_cor_mat"]]))

    data()[["fit_cor_mat"]] %>%
      fix_coeff_names(type = "rows", output = "rhot") %>%
      fix_coeff_names(type = "cols", output = "rhot") %>%
      # Convert to hot and format table
      rhandsontable(
        width = (50 + num_cols * 100),
        height = "100%"
      ) %>%
      hot_cols(colWidths = 100) %>%
      hot_cols(format = "0.000")
  })
}

#' Dose Estimation Case Hottable Server Module
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param stringsAsFactors stringsAsFactors.
#' @param aberr_module Aberration module.
#' @param genome_fraction Genomic fraction used in translocations.
#'
#' @import shiny rhandsontable
#' @importFrom rlang .data
#' @noRd
mod_estimate_case_hot_server <- function(input, output, session, stringsAsFactors, aberr_module, genome_fraction = NULL) {

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
      full_data <- utils::read.csv(case_data$datapath, header = TRUE) %>%
        dplyr::rename_at(
          dplyr::vars(dplyr::everything()),
          toupper
        ) %>%
        dplyr::mutate_at(
          dplyr::vars(dplyr::starts_with("C")),
          as.integer
        ) %>%
        dplyr::select_at(
          dplyr::vars(dplyr::starts_with("C"))
        )
    }

    return(full_data)
  })

  # Reactive data frame ----
  changed_data <- reactive({
    # Create button dependency for updating dimensions
    input$button_upd_table
    input$button_upd_params

    isolate({
      if (is.null(input$case_data_hot) | isolate(table_reset$value == 1)) {
        table_reset$value <- 0
        mytable <- previous()

        # Initial renderization of the table
        if (aberr_module == "dicentrics" | aberr_module == "micronuclei") {
          mytable <- mytable %>%
            dplyr::mutate(
              N = 0,
              X = 0,
              y = 0,
              y_err = 0,
              DI = 0,
              u = 0
            ) %>%
            dplyr::select(.data$N, .data$X, dplyr::everything()) %>%
            dplyr::mutate_at(
              c("X", "N", grep("C", names(.), value = TRUE)),
              as.integer
            )
        } else if (aberr_module == "translocations") {
          mytable <- mytable %>%
            dplyr::mutate(
              N = 0,
              X = 0,
              Fp = 0,
              Fp_err = 0,
              DI = 0,
              u = 0,
              Xc = 0,
              Fg = 0,
              Fg_err = 0
            ) %>%
            dplyr::select(.data$N, .data$X, dplyr::everything()) %>%
            dplyr::mutate_at(
              c("X", "N", grep("C", names(.), value = TRUE)),
              as.integer
            )
        }
      } else if (!identical(previous(), input$case_data_hot)) {
        mytable <- as.data.frame(hot_to_r(input$case_data_hot))

        # Calculated columns
        mytable <- mytable %>%
          dplyr::mutate(
            N = as.integer(rowSums(.[grep("C", names(.))])),
            X = calculate_aberr_power(mytable, power = 1),
            X2 = calculate_aberr_power(mytable, power = 2),
            var = calculate_aberr_var(.data$X, .data$X2, .data$N),
            std_err = sqrt(.data$var / .data$N),
            mean = calculate_aberr_mean(.data$X, .data$N),
            DI = calculate_aberr_disp_index(.data$mean, .data$var),
            u = calculate_aberr_u_value(.data$X, .data$N, .data$mean, .data$var, assessment_u = 1)
          ) %>%
          dplyr::mutate_at(
            c("X", "N", grep("C", names(.), value = TRUE)),
            as.integer
          ) %>%
          dplyr::select(-.data$X2, -.data$var) %>%
          dplyr::select(.data$N, .data$X, dplyr::everything())

        # Rename mean and std_err depending on aberration module
        if (aberr_module == "dicentrics" | aberr_module == "micronuclei") {
          mytable <- mytable %>%
            dplyr::mutate(
              y = .data$mean,
              y_err = .data$std_err
            ) %>%
            dplyr::select(-.data$mean, -.data$std_err)
        } else if (aberr_module == "translocations") {
          genome_fraction <- genome_fraction$genome_fraction()

          mytable <- mytable %>%
            dplyr::mutate(
              Fp = .data$mean,
              Fp_err = .data$std_err
            ) %>%
            dplyr::select(-.data$mean, -.data$std_err) %>%
            dplyr::mutate(
              Xc = ifelse(
                input$trans_confounders & input$trans_confounders_type == "sigurdson",
                get_translocation_rate(
                  .data$N, genome_fraction,
                  age_value = input$trans_confounder_age,
                  sex_bool = input$trans_confounder_sex,
                  sex_value = input$trans_sex,
                  smoker_bool = input$trans_confounder_smoke,
                  ethnicity_value = input$trans_confounder_ethnicity,
                  region_value = input$trans_confounder_region
                ),
                ifelse(
                  input$trans_confounders & input$trans_confounders_type == "manual",
                  input$trans_expected_aberr_value * .data$N * genome_fraction,
                  0
                )
              ),
              Fg = (.data$X - .data$Xc) / (.data$N * genome_fraction),
              Fg_err = .data$Fp_err / sqrt(genome_fraction)
            )
        }

        return(mytable)
      }
    })
  })

  # Output ----
  output$case_data_hot <- renderRHandsontable({
    # Read number of columns
    num_cols <- as.numeric(ncol(changed_data()))

    # Convert to hot and format table
    hot <- changed_data() %>%
      rhandsontable(
        width = (50 + num_cols * 50),
        height = "100%"
      ) %>%
      hot_cols(colWidths = 50)
    # hot_table(highlightCol = TRUE, highlightRow = TRUE)

    if (aberr_module == "dicentrics" | aberr_module == "micronuclei") {
      hot <- hot %>%
        hot_col(c(1, 2, seq(num_cols - 3, num_cols, 1)), readOnly = TRUE) %>%
        hot_col(num_cols, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value > 1.96) {
              td.style.background = 'pink';
             }
           }")
    } else if (aberr_module == "translocations") {
      hot <- hot %>%
        hot_col(c(1, 2, seq(num_cols - 6, num_cols, 1)), readOnly = TRUE) %>%
        hot_col(num_cols - 3, renderer = "
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

#' Dose Estimation Results Server Module
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param stringsAsFactors stringsAsFactors.
#' @param aberr_module Aberration module.
#' @param genome_fraction Genomic fraction used in translocations.
#'
#' @import shiny rhandsontable
#' @importFrom rlang .data
#' @noRd
mod_estimate_results_server <- function(input, output, session, stringsAsFactors, aberr_module, genome_fraction = NULL) {
  data <- reactive({

    # Calcs: get variables ----
    input$button_estimate

    isolate({
      # Fit data
      load_fit_data <- input$load_fit_data_check
      fit_data <- input$load_fit_data
      exposure <- input$exposure_select
      assessment <- input$assessment_select

      # Cases data
      case_data <- hot_to_r(input$case_data_hot)

      # Coefficient input selection
      fraction_coeff <- input$fraction_coeff_select
    })

    # Get error/calculation methods
    error_method <- input$error_method_whole_select
    # if (assessment == "partial-body") {
    #   error_method_partial <- input$error_method_partial_select
    # }
    if (assessment == "hetero") {
      error_method_hetero <- input$error_method_hetero_select
    }

    # Get fitting data ----
    if (load_fit_data) {
      fit_results_list <- readRDS(fit_data$datapath)

      # Additional info for translocations module
      if (aberr_module == "translocations") {
        fit_genome_fraction <- fit_results_list[["genome_fraction"]]

        # Conversion of coefficients and statistics
        if (fit_results_list[["frequency_select"]] == "measured_freq") {

          # Update coefficients
          fit_results_list[["fit_coeffs"]][, "estimate"] <- fit_results_list[["fit_coeffs"]][, "estimate"] / fit_genome_fraction
          fit_results_list[["fit_coeffs"]][, "std.error"] <- fit_results_list[["fit_coeffs"]][, "std.error"] / fit_genome_fraction

          # Update variance-covariance matrix
          fit_results_list[["fit_var_cov_mat"]] <- fit_results_list[["fit_var_cov_mat"]] / fit_genome_fraction^2
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
        as.matrix() %>%
        `rownames<-`(names_from_formula(model_formula))

      if (input$use_var_cov_matrix) {
        fit_var_cov_mat <- hot_to_r(input$fit_var_cov_mat_hot) %>%
          as.matrix() %>%
          `colnames<-`(rownames(fit_coeffs)) %>%
          `rownames<-`(rownames(fit_coeffs))
      } else {
        # Calculate var-cov matrix when none is provided
        fit_var_cov_mat <- matrix(
          0,
          nrow = nrow(fit_coeffs),
          ncol = nrow(fit_coeffs)
        ) %>%
          `colnames<-`(rownames(fit_coeffs)) %>%
          `rownames<-`(rownames(fit_coeffs))

        for (x_var in rownames(fit_var_cov_mat)) {
          fit_var_cov_mat[[x_var, x_var]] <- fit_coeffs[x_var, "std.error"] * fit_coeffs[x_var, "std.error"]
        }
      }

      # Conversion of coefficients and statistics
      if (aberr_module == "translocations") {
        if (input$frequency_select == "measured_freq") {
          fit_genome_fraction <- input$fit_genome_fraction

          # Update coefficients
          fit_coeffs[, "estimate"] <- fit_coeffs[, "estimate"] / fit_genome_fraction
          fit_coeffs[, "std.error"] <- fit_coeffs[, "std.error"] / fit_genome_fraction

          # Update variance-covariance matrix
          fit_var_cov_mat <- fit_var_cov_mat / fit_genome_fraction^2
        }
      }

      fit_results_list <- list(
        fit_formula_tex = fit_formula_tex,
        fit_coeffs = fit_coeffs,
        fit_var_cov_mat = fit_var_cov_mat
      )
    }

    # browser()

    # Parse fitting data
    fit_coeffs <- fit_results_list[["fit_coeffs"]]
    fit_var_cov_mat <- fit_results_list[["fit_var_cov_mat"]]
    fit_formula_tex <- fit_results_list[["fit_formula_tex"]]

    # Generalized variance-covariance matrix
    general_fit_coeffs <- numeric(length = 3L) %>%
      `names<-`(c("coeff_C", "coeff_alpha", "coeff_beta"))

    for (var in rownames(fit_coeffs)) {
      general_fit_coeffs[var] <- fit_coeffs[var, "estimate"]
    }

    # Generalized fit coefficients
    general_var_cov_mat <- matrix(0, nrow = 3, ncol = 3) %>%
      `row.names<-`(c("coeff_C", "coeff_alpha", "coeff_beta")) %>%
      `colnames<-`(c("coeff_C", "coeff_alpha", "coeff_beta"))

    for (x_var in rownames(fit_var_cov_mat)) {
      for (y_var in colnames(fit_var_cov_mat)) {
        general_var_cov_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var]
      }
    }

    # Protracted variables ----
    if (exposure == "protracted") {
      protracted_time <- input$protracted_time
      protracted_life_time <- input$protracted_life_time
      protracted_g_value <- protracted_g_function(protracted_time, protracted_life_time)
    } else if (exposure == "protracted_high") {
      protracted_g_value <- 0
      # Used in report (dummy values)
      protracted_time <- NA
      protracted_life_time <- NA
    } else {
      protracted_g_value <- 1
      # Used in report (dummy values)
      protracted_time <- NA
      protracted_life_time <- NA
    }

    # Confidence intervals ----

    # Select whole-body CI depending on selected method
    if (grepl("merkle", error_method, fixed = TRUE)) {
      conf_int_curve <- paste0("0.", gsub("\\D", "", error_method)) %>% as.numeric()
      conf_int_yield <- conf_int_curve

      # Parse CI text for plot legend
      conf_int_text_whole <- paste0(
        "(", round(100 * conf_int_curve, 0), "%",
        "-", round(100 * conf_int_yield, 0), "%", ")"
      )
    } else if (error_method == "delta") {
      conf_int_curve <- 0.83
      conf_int_yield <- conf_int_curve
      conf_int_delta <- 0.95

      # Parse CI text for plot legend
      conf_int_text_whole <- paste0("(", round(100 * conf_int_delta, 0), "%", ")")
    }

    # Initialize partial-body and heterogeneous CI text for plot legend
    conf_int_text_partial <- NULL
    conf_int_text_hetero <- NULL

    # Select partial-body and heterogeneous CI depending on selected method
    if (assessment == "partial-body") {
      conf_int_dolphin <- 0.95

      # Parse CI text for plot legend
      conf_int_text_partial <- paste0("(", round(100 * conf_int_dolphin, 0), "%", ")")
    } else if (assessment == "hetero") {
      conf_int_curve_hetero <- paste0("0.", gsub("\\D", "", error_method_hetero)) %>% as.numeric()
      conf_int_yield_hetero <- conf_int_curve_hetero

      # Parse CI text for plot legend
      conf_int_text_hetero <- paste0(
        "(", round(100 * conf_int_curve_hetero, 0), "%",
        "-", round(100 * conf_int_yield_hetero, 0), "%", ")"
      )
    }

    conf_int_curve <- conf_int_curve %>%
      correct_conf_int(general_var_cov_mat, protracted_g_value, type = "curve")
    conf_int_yield <- conf_int_yield %>%
      correct_conf_int(general_var_cov_mat, protracted_g_value, type = "yield")

    # Calculations ----

    # Parse genome fraction
    if (aberr_module == "translocations") {
      parsed_genome_fraction <- genome_fraction$genome_fraction()
    } else {
      parsed_genome_fraction <- 1
    }

    # Calculate whole-body results
    if (grepl("merkle", error_method, fixed = TRUE)) {
      message("\nPerforming whole-body dose estimation (Merkle)...")
      results_whole <- estimate_whole_body(
        case_data,
        general_fit_coeffs,
        general_var_cov_mat,
        conf_int_yield,
        conf_int_curve,
        protracted_g_value,
        parsed_genome_fraction,
        aberr_module
      )
    } else if (error_method == "delta") {
      message("\nPerforming whole-body dose estimation (Delta)...")
      results_whole <- estimate_whole_body_delta(
        case_data,
        general_fit_coeffs,
        general_var_cov_mat,
        conf_int_delta,
        protracted_g_value,
        cov = TRUE,
        aberr_module
      )
    }

    # Parse results
    est_doses_whole <- results_whole[["est_doses"]]
    AIC_whole <- results_whole[["AIC"]]

    if (assessment == "partial-body") {
      # Input of the parameter gamma
      if (fraction_coeff == "gamma") {
        gamma <- input$gamma_coeff
      } else if (fraction_coeff == "d0") {
        gamma <- 1 / input$d0_coeff
      }

      # Calculate partial results
      message("\nPerforming partial-body dose estimation (Dolphin)...")
      results_partial <- estimate_partial_dolphin(
        case_data,
        general_fit_coeffs,
        general_var_cov_mat,
        conf_int_dolphin,
        protracted_g_value,
        cov = TRUE,
        parsed_genome_fraction,
        aberr_module,
        gamma
      )

      # Parse results
      est_doses_partial <- results_partial[["est_doses"]]
      est_frac_partial <- results_partial[["est_frac"]]
      AIC_partial <- results_partial[["AIC"]]
    } else if (assessment == "hetero") {
      # Input of the parameter gamma and its variance
      if (fraction_coeff == "gamma") {
        gamma <- input$gamma_coeff
        gamma_error <- input$gamma_error
      } else if (fraction_coeff == "d0") {
        gamma <- 1 / input$d0_coeff
        gamma_error <- 0
      }

      # Calculate heterogeneous result
      message("\nPerforming heterogeneous dose estimation...")
      results_hetero <- estimate_hetero(
        case_data,
        general_fit_coeffs,
        general_var_cov_mat,
        conf_int_yield_hetero,
        conf_int_curve_hetero,
        protracted_g_value,
        gamma,
        gamma_error
      )

      # Parse results
      est_mixing_prop_hetero <- results_hetero[["est_mixing_prop"]]
      est_yields_hetero <- results_hetero[["est_yields"]]
      est_doses_hetero <- results_hetero[["est_doses"]]
      est_frac_hetero <- results_hetero[["est_frac"]]
      AIC_hetero <- results_hetero[["AIC"]]
    }

    # Make plot ----
    message("\nPlotting dose estimation results...")

    # Data set for dose plotting
    if (assessment == "whole-body") {
      est_full_doses <- data.frame(
        dose = c(est_doses_whole[["dose"]]),
        yield = c(est_doses_whole[["yield"]]),
        type = c(rep("Whole-body", 3)),
        level = rep(c("Lower", "Estimate", "Upper"), 1)
      )
    } else if (assessment == "partial-body") {
      est_full_doses <- data.frame(
        dose = c(est_doses_whole[["dose"]], est_doses_partial[["dose"]]),
        yield = c(est_doses_whole[["yield"]], est_doses_partial[["yield"]]),
        type = c(rep("Whole-body", 3), rep("Partial-body", 3)),
        level = rep(c("Lower", "Estimate", "Upper"), 2)
      )
    } else if (assessment == "hetero") {
      est_full_doses <- data.frame(
        dose = c(est_doses_whole[["dose"]], est_doses_hetero[["dose1"]], est_doses_hetero[["dose2"]]),
        yield = c(est_doses_whole[["yield"]], est_yields_hetero[["yield1"]], est_yields_hetero[["yield2"]]),
        type = c(rep("Whole-body", 3), rep("Heterogeneous 1", 3), rep("Heterogeneous 2", 3)),
        level = rep(c("Lower", "Estimate", "Upper"), 3)
      )
    }

    # Name of the aberration to use in the y-axis
    aberr_name <- to_title(aberr_module)
    if (aberr_module == "translocations") {
      if (nchar(input$trans_name) > 0) {
        aberr_name <- input$trans_name
      }
    }

    # Get dose estimation curve
    gg_curve <- get_estimated_dose_curve(
      est_full_doses,
      general_fit_coeffs,
      general_var_cov_mat,
      protracted_g_value,
      conf_int_yield,
      conf_int_curve,
      conf_int_text_whole,
      conf_int_text_partial,
      conf_int_text_hetero,
      aberr_name
    )

    message("\nDone!")

    # Return list ----

    # Make basic list of results to return
    est_results_list <- list(
      # Used in app
      assessment = assessment,
      # Whole-body
      est_doses_whole = est_doses_whole,
      # Partial
      est_doses_partial = NA,
      est_frac_partial = NA,
      # Heterogeneous
      est_mixing_prop_hetero = NA,
      est_yields_hetero = NA,
      est_doses_hetero = NA,
      est_frac_hetero = NA,
      # AICs
      AIC_whole = AIC_whole,
      AIC_partial = NA,
      AIC_hetero = NA,
      # Plot
      gg_curve = gg_curve,
      # Required for report
      fit_coeffs = fit_coeffs,
      protraction = c(0, 0, 0),
      fit_formula_tex = fit_formula_tex,
      case_data = case_data,
      case_description = input$case_description,
      results_comments = input$results_comments
    )

    if (assessment == "partial-body") {
      # Partial
      est_results_list[["est_doses_partial"]] <- est_doses_partial
      est_results_list[["est_frac_partial"]] <- est_frac_partial
      # Reset Heterogeneous
      est_results_list[["est_mixing_prop_hetero"]] <- NA
      est_results_list[["est_yields_hetero"]] <- NA
      est_results_list[["est_doses_hetero"]] <- NA
      est_results_list[["est_frac_hetero"]] <- NA
      # AICs
      est_results_list[["AIC_partial"]] <- AIC_partial
      est_results_list[["AIC_hetero"]] <- NA
    } else if (assessment == "hetero") {
      # Heterogeneous
      est_results_list[["est_mixing_prop_hetero"]] <- est_mixing_prop_hetero
      est_results_list[["est_yields_hetero"]] <- est_yields_hetero
      est_results_list[["est_doses_hetero"]] <- est_doses_hetero
      est_results_list[["est_frac_hetero"]] <- est_frac_hetero
      # Reset Partial
      est_results_list[["est_doses_partial"]] <- NA
      est_results_list[["est_frac_partial"]] <- NA
      # AICs
      est_results_list[["AIC_partial"]] <- NA
      est_results_list[["AIC_hetero"]] <- AIC_hetero
    }

    # Check if protracted correction was applied
    if (exposure == "protracted" & any(grep("beta", fit_formula_tex))) {
      est_results_list[["protraction"]] <- c(1, protracted_time, protracted_life_time)
    }

    # Additional results if using translocations
    if (aberr_module == "translocations") {
      est_results_list[["genome_fraction"]] <- genome_fraction$genome_fraction()
      est_results_list[["chromosome_table"]] <- hot_to_r(input$chromosome_table)
      est_results_list[["trans_sex"]] <- input$trans_sex

      if (!input$trans_confounders) {
        est_results_list[["confounders"]] <- NULL
      } else if (input$trans_confounders & input$trans_confounders_type == "sigurdson") {
        est_results_list[["confounders"]] <- c(
          age_value = input$trans_confounder_age,
          sex_bool = input$trans_confounder_sex,
          smoker_bool = input$trans_confounder_smoke,
          ethnicity_value = input$trans_confounder_ethnicity,
          region_value = input$trans_confounder_region
        )
      } else if (input$trans_confounders & input$trans_confounders_type == "manual") {
        est_results_list[["confounders"]] <- input$trans_expected_aberr_value
      }
    }

    return(est_results_list)
  })

  # Results outputs ----

  # renderUI: Estimate results tabBox ----
  output$estimate_results_ui <- renderUI({
    assessment <- input$assessment_select

    # Help modal
    hetero_modal <- bsplus::bs_modal(
      id = session$ns("help_dose_mixed_yields_modal"),
      title = "Help: Heterogeneous exposures",
      size = "large",

      body = tagList(
        include_help("estimate/dose_mixed_yields.md")
      )
    )

    if (assessment == "whole-body") {
      # Whole-body
      return_tabbox <- tabBox(
        id = "estimate_results_tabs",
        width = 12,
        side = "left",

        title = help_modal_button(
          container = "tabbox",
          session$ns("help_dose_mixed_yields"),
          session$ns("help_dose_mixed_yields_modal")
        ),

        tabPanel(
          title = "Whole-body",
          h5("Whole-body exposure estimation"),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_yields_whole"))
          ),
          br(),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_doses_whole"))
          ) # ,

          # br(),
          # h5("Relative quality of the estimation"),
          # div(
          #   class = "hot-improved",
          #   rHandsontableOutput(session$ns("AIC_whole"))
          # )
        )
      )
    } else if (assessment == "partial-body") {
      # Partial-body
      return_tabbox <- tabBox(
        id = "estimate_results_tabs",
        width = 12,
        side = "left",

        title = help_modal_button(
          container = "tabbox",
          session$ns("help_dose_mixed_yields"),
          session$ns("help_dose_mixed_yields_modal")
        ),

        tabPanel(
          title = "Whole-body",
          h5("Whole-body exposure estimation"),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_yields_whole"))
          ),
          br(),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_doses_whole"))
          ) # ,

          # br(),
          # h5("Relative quality of the estimation"),
          # div(
          #   class = "hot-improved",
          #   rHandsontableOutput(session$ns("AIC_whole"))
          # )
        ),
        tabPanel(
          title = "Partial-body",
          h5("Partial-body exposure estimation"),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_yields_partial"))
          ),
          br(),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_doses_partial"))
          ),

          br(),
          h5("Initial fraction of irradiated cells"),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_frac_partial"))
          ) # ,

          # br(),
          # h5("Relative quality of the estimation"),
          # div(
          #   class = "hot-improved",
          #   rHandsontableOutput(session$ns("AIC_partial"))
          # )
        )
      )
    } else if (assessment == "hetero") {
      # Heterogeneous
      return_tabbox <- tabBox(
        id = "estimate_results_tabs",
        width = 12,
        side = "left",

        title = help_modal_button(
          container = "tabbox",
          session$ns("help_dose_mixed_yields"),
          session$ns("help_dose_mixed_yields_modal")
        ),

        tabPanel(
          title = "Whole-body",
          h5("Whole-body exposure estimation"),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_yields_whole"))
          ),
          br(),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_doses_whole"))
          ) # ,

          # br(),
          # h5("Relative quality of the estimation"),
          # div(
          #   class = "hot-improved",
          #   rHandsontableOutput(session$ns("AIC_whole"))
          # )
        ),
        tabPanel(
          title = "Heterogeneous",
          h5("Observed fraction of irradiated cells and its yield"),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_mixing_prop_hetero"))
          ),

          br(),
          h5("Heterogeneous exposure estimation"),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_yields_hetero"))
          ),
          br(),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_doses_hetero"))
          ),

          br(),
          h5("Initial fraction of irradiated cells"),
          div(
            class = "hot-improved",
            rHandsontableOutput(session$ns("est_frac_hetero"))
          ) # ,

          # br(),
          # h5("Relative quality of the estimation"),
          # div(
          #   class = "hot-improved",
          #   rHandsontableOutput(session$ns("AIC_hetero"))
          # )
        )
      )
    } else {
      return(NULL)
    }

    return(tagList(return_tabbox, hetero_modal))
  })

  # Estimated recieved doses (whole-body)
  output$est_yields_whole <- renderRHandsontable({
    if (input$button_estimate <= 0) {
      return(NULL)
    }
    data()[["est_doses_whole"]] %>%
      dplyr::select(.data$yield) %>%
      t() %>%
      as.data.frame() %>%
      # Convert to hot and format table
      rhandsontable(
        width = 320,
        height = "100%",
        rowHeaderWidth = 80
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # Estimated recieved doses (whole-body)
  output$est_doses_whole <- renderRHandsontable({
    if (input$button_estimate <= 0) {
      return(NULL)
    }
    data()[["est_doses_whole"]] %>%
      dplyr::select(.data$dose) %>%
      t() %>%
      as.data.frame() %>%
      # Convert to hot and format table
      rhandsontable(
        width = 320,
        height = "100%",
        rowHeaders = "dose (Gy)",
        rowHeaderWidth = 80
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # Estimated recieved doses (partial-body)
  output$est_yields_partial <- renderRHandsontable({
    if (input$button_estimate <= 0 | data()[["assessment"]] != "partial-body") {
      return(NULL)
    }
    data()[["est_doses_partial"]] %>%
      dplyr::select(.data$yield) %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`("yield") %>%
      # Convert to hot and format table
      rhandsontable(
        width = 320,
        height = "100%",
        rowHeaderWidth = 80
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # Estimated recieved doses (partial-body)
  output$est_doses_partial <- renderRHandsontable({
    if (input$button_estimate <= 0 | data()[["assessment"]] != "partial-body") {
      return(NULL)
    }
    data()[["est_doses_partial"]] %>%
      dplyr::select(.data$dose) %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`("dose (Gy)") %>%
      # Convert to hot and format table
      rhandsontable(
        width = 320,
        height = "100%",
        rowHeaderWidth = 80
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # Estimated fraction of irradiated blood for dose dose1 (partial-body)
  output$est_frac_partial <- renderRHandsontable({
    if (input$button_estimate <= 0 | data()[["assessment"]] != "partial-body") {
      return(NULL)
    }
    data()[["est_frac_partial"]] %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`("fraction") %>%
      # Convert to hot and format table
      rhandsontable(
        width = 320,
        height = "100%",
        rowHeaderWidth = 80
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # Estimated yields (heterogeneous)
  output$est_mixing_prop_hetero <- renderRHandsontable({
    if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
      return(NULL)
    }
    data()[["est_mixing_prop_hetero"]] %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      # `colnames<-`(c("y_estimate", "y_std_err", "f_estimate", "f_std_err")) %>%
      `colnames<-`(c("yield", "yield.err", "frac", "frac.err")) %>%
      `row.names<-`(c("dose1", "dose2")) %>%
      # Convert to hot and format table
      rhandsontable(
        width = 405,
        height = "100%",
        rowHeaderWidth = 85
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # Estimated recieved doses (heterogeneous)
  output$est_yields_hetero <- renderRHandsontable({
    if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
      return(NULL)
    }
    data()[["est_yields_hetero"]] %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`(c("yield1", "yield2")) %>%
      # Convert to hot and format table
      rhandsontable(
        width = 325,
        height = "100%",
        rowHeaderWidth = 85
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # Estimated recieved doses (heterogeneous)
  output$est_doses_hetero <- renderRHandsontable({
    if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
      return(NULL)
    }
    data()[["est_doses_hetero"]] %>%
      t() %>%
      as.data.frame() %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("lower", "estimate", "upper")) %>%
      `row.names<-`(c("dose1 (Gy)", "dose2 (Gy)")) %>%
      # Convert to hot and format table
      rhandsontable(
        width = 325,
        height = "100%",
        rowHeaderWidth = 85
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # Estimated fraction of irradiated blood for dose dose1 (heterogeneous)
  output$est_frac_hetero <- renderRHandsontable({
    if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
      return(NULL)
    }
    data()[["est_frac_hetero"]] %>%
      # Fix possible NA values
      dplyr::mutate_if(is.logical, as.double) %>%
      `colnames<-`(c("estimate", "std.err")) %>%
      `row.names<-`(c("dose1", "dose2")) %>%
      # Convert to hot and format table
      rhandsontable(
        width = 245,
        height = "100%",
        rowHeaderWidth = 85
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # AIC for estimated dose (whole)
  output$AIC_whole <- renderRHandsontable({
    if (input$button_estimate <= 0) {
      return(NULL)
    }
    data()[["AIC_whole"]] %>%
      matrix() %>%
      `colnames<-`(c("AIC")) %>%
      rhandsontable(
        width = 80,
        height = "100%"
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # AIC for estimated dose (partial-body)
  output$AIC_partial <- renderRHandsontable({
    if (input$button_estimate <= 0 | data()[["assessment"]] != "partial-body") {
      return(NULL)
    }
    data()[["AIC_partial"]] %>%
      matrix() %>%
      `colnames<-`(c("AIC")) %>%
      rhandsontable(
        width = 80,
        height = "100%"
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # AIC for estimated dose (heterogeneous)
  output$AIC_hetero <- renderRHandsontable({
    if (input$button_estimate <= 0 | data()[["assessment"]] != "hetero") {
      return(NULL)
    }
    data()[["AIC_hetero"]] %>%
      matrix() %>%
      `colnames<-`(c("AIC")) %>%
      rhandsontable(
        width = 80,
        height = "100%"
      ) %>%
      hot_cols(colWidths = 80) %>%
      hot_cols(format = "0.000")
  })

  # Plot of the data and fitted curve
  output$plot <- renderPlot(
    res = 120,
    {
      if (input$button_estimate <= 0) {
        return(NULL)
      }
      data()[["gg_curve"]]
    }
  )


  # Export plot ----
  output$save_plot <- downloadHandler(
    filename = function() {
      paste("estimate-curve-", Sys.Date(), input$save_plot_format, sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(
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
      paste0(aberr_module, "-estimate-report-", Sys.Date(), input$save_report_format)
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      temp_report <- file.path(tempdir(), paste0(aberr_module, "-report.Rmd"))
      local_report <- load_rmd_report(
        paste0(
          aberr_module,
          "-estimate-report-",
          gsub("^\\.", "", input$save_report_format),
          ".Rmd"
        )
      )

      file.copy(local_report, temp_report, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        est_results_list = data()
      )

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        input = temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}
