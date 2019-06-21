# General Fitting Modules ----------------------------------

generalFittingCountsHotTable <- function(input, output, session, stringsAsFactors) {

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
      num_aberrs <- as.numeric(input$num_aberrs) + 1
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
            ncol = num_aberrs
          )
        ) %>%
          `colnames<-`(paste0("C", seq(0, num_aberrs - 1, 1)))

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

          first_aberr_index <- 4
          last_aberr_index <- ncol(mytable) - 2
          num_rows <- nrow(mytable)

          mytable <- mytable %>%
            dplyr::mutate(
              D = as.numeric(D),
              X = as.integer(X),
              N = as.integer(rowSums(.[first_aberr_index:last_aberr_index]))
            )

          # Ugly method to calculate index of dispersion
          for (row in 1:num_rows) {
            xf <- 0
            x2f <- 0
            # Calculate summatories
            for (k in seq(0, last_aberr_index - first_aberr_index, 1)) {
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
