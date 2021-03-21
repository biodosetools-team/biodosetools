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

#' Get Sigurdson's translocation rate
#'
#' @param cells Number of cells \code{N}
#' @param genome_fraction Genomic fraction used in translocations
#' @param age_value Age of the individual
#' @param sex_bool If \code{TRUE}, \code{sex_value} will be used
#' @param sex_value Sex of the individual, either "male" of "female"
#' @param smoker_bool Whether the individual smokes or not
#' @param ethnicity_value Ethnicity of the individual
#' @param region_value Region of the individual
#'
#' @return Translocation rate.
#' @export
get_translocation_rate_sigurdson <- function(cells, genome_fraction, age_value,
                                   sex_bool = FALSE, sex_value = "none",
                                   smoker_bool = FALSE,
                                   ethnicity_value = "none", region_value = "none") {
  age_trans_frequency <- function(age) {
    trans_frequency <- exp(-7.925) + exp(-9.284) * (age * exp(0.01062 * age))
    return(trans_frequency)
  }

  sex_trans_list <- c(1, 1, 0.92) %>%
    `names<-`(c("none", "male", "female"))
  smoke_trans_list <- c(1, 1.19) %>%
    `names<-`(c("FALSE", "TRUE"))
  ethnicity_trans_list <- c(1, 1, 1.23, 0.84, 1.06) %>%
    `names<-`(c("none", "white", "asian", "black", "other"))
  region_trans_list <- c(1, 1, 0.99, 1.75, 1.75, 0.86) %>%
    `names<-`(c("none", "n-america", "w-europe", "c-europe", "e-europe", "asia"))

  # sex_value <- ifelse(sex_bool, input$trans_sex, "none")
  sex_value <- ifelse(sex_bool, sex_value, "none")

  sex_trans_frequency <- sex_trans_list[[sex_value]]
  smoke_trans_frequency <- smoke_trans_list[[as.character(smoker_bool)]]
  ethnicity_trans_frequency <- ethnicity_trans_list[[ethnicity_value]]
  region_trans_frequency <- region_trans_list[[region_value]]

  # Expected aberrations
  expected_aberr <- cells * genome_fraction *
    age_trans_frequency(age_value) *
    sex_trans_frequency *
    smoke_trans_frequency *
    ethnicity_trans_frequency *
    region_trans_frequency

  return(expected_aberr)
}

#' Get manual translocation rate
#'
#' @param cells Number of cells \code{N}
#' @param genome_fraction Genomic fraction used in translocations
#' @param expected_aberr_value Expected aberrations
#'
#' @return Translocation rate.
#' @export
get_translocation_rate_manual <- function(cells, genome_fraction, expected_aberr_value) {
  # Expected aberrations
  expected_aberr <- expected_aberr_value * cells * genome_fraction

  return(expected_aberr)
}
