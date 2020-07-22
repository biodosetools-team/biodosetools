#' Gardner's Confidence Intervals Table
#'
#' Confidence intervals for the expectation of a Poisson variable.
#'
#' @format A data frame with 1201 rows and 3 variables:
#' \describe{
#'   \item{s_est}{Nominal value for the Poisson variable.}
#'   \item{s_low}{Lower confidence interval for the Poisson variable.}
#'   \item{s_upp}{Upper confidence interval for the Poisson variable.}
#' }
#' @source \url{https://doi.org/10.1093/biomet/46.3-4.441}
"gardner_confidence_table"

#' DNA Content Fractions of Human Chromosomes
#'
#' Normalised DNA Content of Human Chromosomes.
#'
#' @format A data frame with 24 rows and 3 variables:
#' \describe{
#'   \item{chromosome}{Chromosome.}
#'   \item{fraction_male}{Normalised content of megabases on male human DNA.}
#'   \item{fraction_female}{Normalised content of megabases on female human DNA.}
#' }
"dna_content_fractions"
