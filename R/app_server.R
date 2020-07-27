#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Dicentrics Modules ----

  # Fitting
  callModule(
    id = "dicent_fitting",
    module = mod_fitting_counts_hot_server,
    aberr_module = "dicentrics"
  )
  callModule(
    id = "dicent_fitting",
    module = mod_fitting_results_server,
    aberr_module = "dicentrics"
  )

  # Dose Estimation
  callModule(
    id = "dicent_estimate",
    module = mod_estimate_case_hot_server,
    aberr_module = "dicentrics"
  )
  callModule(
    id = "dicent_estimate",
    module = mod_estimate_fit_curve_hot_server
  )
  callModule(
    id = "dicent_estimate",
    module = mod_estimate_fit_curve_server,
    aberr_module = "dicentrics"
  )
  callModule(
    id = "dicent_estimate",
    module = mod_estimate_results_server,
    aberr_module = "dicentrics"
  )

  # Translocations Modules ----

  # Help
  callModule(
    id = "trans_fitting",
    module = mod_help_chromosome_hot_server
  )
  callModule(
    id = "trans_estimate",
    module = mod_help_chromosome_hot_server
  )

  # Fitting
  callModule(
    id = "trans_fitting",
    module = mod_trans_chromosome_hot_server
  )
  callModule(
    id = "trans_fitting",
    module = mod_trans_fraction_to_full_genome_server
  ) -> transFittingFraction
  callModule(
    id = "trans_fitting",
    module = mod_fitting_counts_hot_server,
    aberr_module = "translocations"
  )
  callModule(
    id = "trans_fitting",
    module = mod_fitting_results_server,
    aberr_module = "translocations",
    genome_fraction = transFittingFraction
  )

  # Dose Estimation
  callModule(
    id = "trans_estimate",
    module = mod_trans_chromosome_hot_server
  )
  callModule(
    id = "trans_estimate",
    module = mod_trans_fraction_to_full_genome_server
  ) -> transEstimateFraction
  callModule(
    id = "trans_estimate",
    module = mod_estimate_case_hot_server,
    aberr_module = "translocations",
    genome_fraction = transEstimateFraction
  )

  callModule(
    id = "trans_estimate",
    module = mod_estimate_fit_curve_hot_server
  )
  callModule(
    id = "trans_estimate",
    module = mod_estimate_fit_curve_server,
    aberr_module = "translocations"
  )
  callModule(
    id = "trans_estimate",
    module = mod_estimate_results_server,
    aberr_module = "translocations",
    genome_fraction = transEstimateFraction
  )

  # Micronuclei Modules ----

  # Fitting
  callModule(
    id = "micro_fitting",
    module = mod_fitting_counts_hot_server,
    aberr_module = "micronuclei"
  )
  callModule(
    id = "micro_fitting",
    module = mod_fitting_results_server,
    aberr_module = "micronuclei"
  )

  # Dose Estimation
  callModule(
    id = "micro_estimate",
    module = mod_estimate_case_hot_server,
    aberr_module = "micronuclei"
  )
  callModule(
    id = "micro_estimate",
    module = mod_estimate_fit_curve_hot_server
  )
  callModule(
    id = "micro_estimate",
    module = mod_estimate_fit_curve_server,
    aberr_module = "micronuclei"
  )
  callModule(
    id = "micro_estimate",
    module = mod_estimate_results_server,
    aberr_module = "micronuclei"
  )
}
