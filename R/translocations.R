#' Get genome fraction
#'
#' @param dna_table DNA content fractions table.
#' @param chromosome Chromosomes.
#' @param color Color of the stains.
#' @param sex Sex of the individual
#'
#' @return Fraction of genome hybridized.
#' @export
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
    dplyr::group_by(color) %>%
    dplyr::summarise(genome_fraction = sum(base::get(paste0("fraction_", sex))))

  # Calculate first sum
  single_sum <- full_table %>%
    dplyr::select(genome_fraction) %>%
    dplyr::summarise(
      single_sum = sum(genome_fraction * (1 - genome_fraction))
    ) %>%
    dplyr::pull(single_sum)

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
