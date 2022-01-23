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
    id = "fitting_dicent_ui",
    module = mod_fitting_counts_hot_server,
    aberr_module = "dicentrics"
  )
  callModule(
    id = "fitting_dicent_ui",
    module = mod_fitting_results_server,
    aberr_module = "dicentrics"
  )

  # Dose Estimation
  callModule(
    id = "estimation_dicent_ui",
    module = mod_estimation_case_hot_server,
    aberr_module = "dicentrics"
  )
  callModule(
    id = "estimation_dicent_ui",
    module = mod_estimation_fit_curve_hot_server
  )
  callModule(
    id = "estimation_dicent_ui",
    module = mod_estimation_fit_curve_server,
    aberr_module = "dicentrics"
  )
  callModule(
    id = "estimation_dicent_ui",
    module = mod_estimation_results_server,
    aberr_module = "dicentrics"
  )

  # Translocations Modules ----

  # Help
  callModule(
    id = "fitting_trans_ui",
    module = mod_help_chromosome_hot_server
  )
  callModule(
    id = "estimation_trans_ui",
    module = mod_help_chromosome_hot_server
  )

  # Fitting
  callModule(
    id = "fitting_trans_ui",
    module = mod_trans_chromosome_hot_server
  )
  callModule(
    id = "fitting_trans_ui",
    module = mod_trans_fraction_to_full_genome_server
  ) -> transFittingFraction
  callModule(
    id = "fitting_trans_ui",
    module = mod_fitting_counts_hot_server,
    aberr_module = "translocations"
  )
  callModule(
    id = "fitting_trans_ui",
    module = mod_fitting_results_server,
    aberr_module = "translocations",
    genome_factor = transFittingFraction
  )

  # Dose Estimation
  callModule(
    id = "estimation_trans_ui",
    module = mod_trans_chromosome_hot_server
  )
  callModule(
    id = "estimation_trans_ui",
    module = mod_trans_fraction_to_full_genome_server
  ) -> transEstimateFraction
  callModule(
    id = "estimation_trans_ui",
    module = mod_estimation_case_hot_server,
    aberr_module = "translocations",
    genome_factor = transEstimateFraction
  )

  callModule(
    id = "estimation_trans_ui",
    module = mod_estimation_fit_curve_hot_server
  )
  callModule(
    id = "estimation_trans_ui",
    module = mod_estimation_fit_curve_server,
    aberr_module = "translocations"
  )
  callModule(
    id = "estimation_trans_ui",
    module = mod_estimation_results_server,
    aberr_module = "translocations",
    genome_factor = transEstimateFraction
  )

  # Micronuclei Modules ----

  if (golem::app_dev()) {
    # Fitting
    callModule(
      id = "fitting_micro_ui",
      module = mod_fitting_counts_hot_server,
      aberr_module = "micronuclei"
    )
    callModule(
      id = "fitting_micro_ui",
      module = mod_fitting_results_server,
      aberr_module = "micronuclei"
    )

    # Dose Estimation
    callModule(
      id = "estimation_micro_ui",
      module = mod_estimation_case_hot_server,
      aberr_module = "micronuclei"
    )
    callModule(
      id = "estimation_micro_ui",
      module = mod_estimation_fit_curve_hot_server
    )
    callModule(
      id = "estimation_micro_ui",
      module = mod_estimation_fit_curve_server,
      aberr_module = "micronuclei"
    )
    callModule(
      id = "estimation_micro_ui",
      module = mod_estimation_results_server,
      aberr_module = "micronuclei"
    )
  }
}
