# General translocation Modules ----------------------------

transChromosomeTable <- function(input, output, session, stringsAsFactors) {
  table <- reactive({
    input$button_upd_chrom_table

    isolate({
      chromosome_list <- input$trans_chromosome_select
      color_scheme <- input$trans_m_fish_scheme
    })

    # Modify table rendering depending on stain method
    if(color_scheme) {
      data <- data.frame(
        Chromosome = chromosome_list
      )
    } else {
      color_list <- input$trans_color_select

      data <- matrix(
        nrow = length(chromosome_list),
        ncol = length(color_list) + 1
      ) %>%
        as.data.frame() %>%
        `colnames<-`(c("Chromosome", color_list))

      data[["Chromosome"]] <- chromosome_list
    }

    return(data)
  })

  # Output ----
  output$chromosome_table <- rhandsontable::renderRHandsontable({
    if (input$button_upd_chrom_table <= 0) return(NULL)

    num_cols <- as.numeric(ncol(table()))

    hot <- table() %>%
      rhandsontable::rhandsontable(width = (80 + num_cols * 85), height = "100%") %>%
      rhandsontable::hot_col(1, colWidths = 115, readOnly = TRUE) %>%
      rhandsontable::hot_cols(halign = "htCenter")

    if(num_cols > 1) {
      hot <- hot %>%
        rhandsontable::hot_col(2:num_cols, colWidths = 85)
    }

    hot$x$contextMenu <- list(items = c("remove_row", "---------", "undo", "redo"))

    return(hot)
  })
}

transFractionToFullGenomeCalc <- function(input, output, session, stringsAsFactors) {

  # Calculate genomic fraction ----

  genome_fraction <- reactive({

    # Create button dependency for updating dimensions
    input$button_calc_genome_fraction

    isolate({
      dna_table <- data.table::fread("libs/dna-content-fractions.csv")
      color_scheme <- input$trans_m_fish_scheme
      chromosome_table <- rhandsontable::hot_to_r(input$chromosome_table)
      sex <- input$trans_sex
    })

    # Modify pairs of chromosome/stain color depending on stain method
    if (color_scheme) {
      chromosome <- chromosome_table[["Chromosome"]] %>% as.character()
      color <- paste("M-Fish", chromosome_table[["Chromosome"]]) %>% as.character()
    } else {
      chromosome_table_melt <- chromosome_table %>%
        tidyr::pivot_longer(
          cols = -Chromosome,
          names_to = "Stain",
          values_to = "Bool"
        )

      chromosome_table_clean <- chromosome_table_melt %>%
        dplyr::filter(Bool == TRUE)

      chromosome <- chromosome_table_clean[["Chromosome"]] %>% as.character()
      color <- chromosome_table_clean[["Stain"]]
    }

    get_genome_fraction <- function(dna_table, chromosome, color, sex) {
      # Construct color/chromosome table
      color_table <-
        cbind(
          color,
          chromosome
        ) %>%
        as.data.frame() %>%
        dplyr::mutate(
          chromosome = as.character(chromosome)
        )

      # Full table
      full_table <- dplyr::inner_join(color_table, dna_table, by = "chromosome") %>%
        dplyr::group_by(color) %>%
        dplyr::summarise(genome_fraction = sum(get(paste0("fraction_", sex))))

      # Calculate first sum
      single_sum <- full_table %>%
        dplyr::select(genome_fraction) %>%
        dplyr::summarise(sum(genome_fraction * (1 - genome_fraction))) %>%
        unname() %>%
        unlist()

      # Calculate second sum
      if (nrow(full_table) >= 2) {
        cross_sum <- full_table[["genome_fraction"]] %>%
          utils::combn(2) %>%
          t() %>%
          as.data.frame() %>%
          dplyr::summarise(sum(V1 * V2)) %>%
          unname() %>%
          unlist()
      } else {
        cross_sum <- 0
      }

      return(2 / 0.974 * (single_sum - cross_sum))
    }

    return(get_genome_fraction(dna_table, chromosome, color, sex))
  })

  # Output ----
  output$genome_fraction <- renderUI({
    if (input$button_calc_genome_fraction <= 0) return(NULL)
    genome_fraction_value <- genome_fraction()
    genome_fraction_text <- paste0("The genomic conversion factor to full genome is ", genome_fraction_value %>% round(3) %>%  as.character(), ".")
    return(genome_fraction_text)
  })

  return(
    list(genome_fraction = reactive(genome_fraction()))
  )
}
