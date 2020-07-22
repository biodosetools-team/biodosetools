#' Get genome fraction
#'
#' @param dna_table DNA content fractions table.
#' @param chromosome Chromosomes.
#' @param color Color of the stains.
#' @param sex Sex of the individual
#'
#' @return Fraction of genome hybridized.
#' @export
#' @importFrom rlang .data
get_genome_fraction <- function(dna_table, chromosome, color, sex) {
  # Construct color/chromosome table
  color_table <- cbind(
      color,
      chromosome
    ) %>%
    as.data.frame() %>%
    dplyr::mutate(
      chromosome = as.character(chromosome)
    )

  # Full table
  full_table <- dplyr::inner_join(color_table, dna_table, by = "chromosome") %>%
    dplyr::group_by(.data$color) %>%
    dplyr::summarise(
      genome_fraction = sum(base::get(paste0("fraction_", sex)))
    )

  # Calculate first sum
  single_sum <- full_table %>%
    dplyr::select(.data$genome_fraction) %>%
    dplyr::summarise(
      single_sum = sum(.data$genome_fraction * (1 - .data$genome_fraction))
    ) %>%
    dplyr::pull(.data$single_sum)

  # Calculate second sum
  if (nrow(full_table) >= 2) {
    cross_sum <- full_table[["genome_fraction"]] %>%
      utils::combn(2) %>%
      t() %>%
      as.data.frame() %>%
      dplyr::summarise(
        cross_sum = sum(.data$V1 * .data$V2)
      ) %>%
      dplyr::pull(.data$cross_sum)
  } else {
    cross_sum <- 0
  }

  return(2 / 0.974 * (single_sum - cross_sum))
}
