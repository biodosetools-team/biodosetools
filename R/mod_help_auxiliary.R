# Auxiliary help tables ------------------------------------

helpChromosomeHotTable <- function(input, output, session, stringsAsFactors) {

  # Tables ----
  data <- data.frame(
    Chromosome = c(1,4,12),
    Red = c(FALSE, TRUE, FALSE),
    Green = c(TRUE, FALSE, FALSE),
    Yellow = c( FALSE, FALSE, TRUE)
  ) %>%
    dplyr::mutate_at("Chromosome", as.integer)

  output$help_chromosome_hot <- rhandsontable::renderRHandsontable({
    num_cols <- as.numeric(ncol(data))

    # Convert to hot and format table
    hot <- data %>%
      rhandsontable::rhandsontable(width = (80 + num_cols * 85), height = "100%") %>%
      rhandsontable::hot_col(1, colWidths = 115, readOnly = TRUE) %>%
      rhandsontable::hot_col(2:ncol(data), colWidths = 85) %>%
      rhandsontable::hot_cols(halign = "htCenter")

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))
    return(hot)
  })
}

