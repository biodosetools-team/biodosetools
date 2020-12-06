# biodosetools 3.4.0 (2020-10-11)

* Removed all non-ASCII characters.
* Major rehaul of reports.
* Added "Report bug" button on home screen and cleaned up `dashboard_*()` code.
* Removed dependencies on {stringr} package.
* Initial `utils::citation()` support via CITATION file.
* Minimal version of R bumped to 3.5.0.
* Minimal version of {dplyr} bumped to 1.0.0.

## Bug fixes

* Replaced `C`, `α`, `β` variables (and derivatives) by `coeff_C`, `coeff_alpha`, `coeff_beta`.
* Fixed error in genome_fraction parsing for translocations in `estimate_partial_dolphin()`.
* Fixed count/case data standard error column names replacements.
* Fixed minipage issue when using more than 12 columns in {knitr} tables with column name replacements, by using `format = "latex", escape = FALSE` parameters in `kable()` call.
* Supress YAML warning when using `!expr` in PDF reports. See https://github.com/rstudio/rstudio/issues/7545.

## New functions

* `fix_coeff_names()`, to fix coefficient names in reports.
* `fix_count_data_names()`, to fix count/case data column names in PDF reports.
* `to_title()`, to replace `stringr::str_to_title()` using base R.

## Reports improvements

* New PDF reports, which are now the default output format.
* Improved HTML reports styling.
* Added explicit M-FISH usage in translocation reports.
* Added assay name in report titles.


# biodosetools 3.3.1 (2020-10-07)

## Bug fixes

* Count and case data calculations are performed using `calculate_aberr_*()` functions. Fixes #8.
* Fixed missing {dplyr} namespace in `n()` function call.
* Fixed mismatched use of `awesomeCheckbox()` and `switchInput()` in confounders input.

## New functions

* `calculate_aberr_power()`, which supersedes internal `aberr_calc()` function in server modules.
* `calculate_aberr_mean()`.
* `calculate_aberr_var()`.
* `calculate_aberr_disp_index()`.
* `calculate_aberr_u_value()`.


# biodosetools 3.3.0 (2020-07-27)


# biodosetools 3.2.1 (2020-02-13)

## Bug fixes

* Added required {pander} package to generate DOCX reports.


# biodosetools 3.2.0 (2020-02-11)

All calculations functions previously provided in `inst/app/calcs` have been made proper functions on the package.

## New functions

* `calculate_yield()` new wrapper of `yield_fun()`, `R_factor()`, and `yield_error_fun()`.
* `calculate_yield_infimum()` function to calculate infima of yields given a curve.
* `project_yield()` merged version of the `project_yield_estimate()`, `project_yield_lower()`, and `project_yield_upper()` functions.

## "New" functions

* `get_decision_threshold()`.
* `get_fit_dose_curve()`.
* `get_fit_glm_method()`.
* `get_fit_maxlik_method()`.
* `get_fit_results()`.
* `get_model_statistics()`.
* `prepare_maxlik_count_data()`.
* `AIC_from_data()`.
* `correct_boundary()`.
* `correct_conf_int()`.
* `correct_negative_vals()`.
* `correct_yield()`.
* `get_estimated_dose_curve()`.
* `protracted_g_function()`.
* `R_factor()`.
* `yield_error_fun()`.
* `yield_fun()`.
* `estimate_hetero()`.
* `estimate_partial_dolphin()`.
* `estimate_whole_body_delta()`.
* `estimate_whole_body()`.


# biodosetools 3.1.0 (2019-10-26)

Unofficial release (wasn't changed on `DESCRIPTION` file). This includes some of the changes discussed with David in Stockholm (ERPW19).

## New features

* Made var-cov matrix optional on dose estimation inputs.
* Fixed calculations to make var-cov matrix optional on dose estimation inputs.

## Fixes

* Added additional package dependencies to `DESCRIPTION` file.
* Following the ISO, renamed detection limits to decision threshold.

## Changes

* Disabled decision thresholds (for now).
* Hide AIC as a relative quality statistic of the dose estimation.
* Added mean and variance to count data tables in fitting module.


# biodosetools 3.0.0 (2019-10-12)

The app is now available as an R package on GitHub (we haven't submitted Biodose Tools to CRAN yet).

## New functions

* `runApp()` for launching Biodose Tools.
* `%>%` imported from {magrittr}.

## Fixes

* Fixed variance calculation for count data.
* Provide fallback method for NB2 when using constraint-maxlik-optimization.


# biodosetools 2019.07.27-beta

## New features

* Initial implementation of micronuclei, adding support in UI and server functions.
* Implemented negative binomial fitting calculation for micronuclei count data.
* Initial detection limit implementation for translocations.
* Added DOCX support for dicentrics and translocations.

## New functions

* New `bs4MyTabPanel()` widget to remove unnecessary padding on `tabCards`' panels.

## Fixes

* Fixed colwidths for detection limits.
* Fixed unnecessary recalculation of `num_cols` for chromosome tables in translocations modules.
* New subdirectory structure for help dialogues.
* Added buttons and example tables in help dialogues.
* Updated documentation link.

## Improvements

* Added `button_fit` dependency for detection limits calculation.
* Updated fitting reports (new hot structures & dynamic widths).
* Moved calculation functions from fitting and estimation modules to their own files in a new `calcs` directory.
* Translocations reports are complete now.

## Changes

* Disable sourcing `translations.R` for now (until {shiny-i18n} is implemented).


# biodosetools 2019.07.17-beta

## New features

* Added manual input of translocations name.
* Implemented dynamic calculation of hot width for counts, cases, and chromosome data tables.
* Implemented manual input of translocation frequency to be substracted from the observed yield.
* Started detection limit calculation in fitting module.
* Implemented Delta method for whole-body estimation.
* Added calculation of standard error of Fg for translocations' dose estimation module.

## Fixes

* Fixed depedency of the "Calculate fitting" button in the translocation module.
* Fixed calculations for whole and partial body estimations for translocations module.
* Fixed colors on chromosome hot table example.
* Fix genomic conversion factor not being read when doing fitting.
* Fix `Fp -> y` when using dicentrics in dose estimation module.
* Fixed detection limit calculation.
* Fixed dose estimation in detection limits function.
* Corrected correction of FISH to Full genome when inputting manual fitting curve in dose estimation modules.
* Corrected reading of translocations fit curve values in dose estimation module.

## Improvements

* Tweaked variable names and order in cases data for translocations dose estimation.
* Updated `help_colors_dialog` for translocations modules.
* New module for auxiliary `rhandsontable()` tables used in help dialogues.
* New help dialogues for translocations modules.
* Consistent use of `genome_fraction` in variables related to the genomic conversion factor/fraction of genome hybridised.
* ggplot2 curve generation is now wrapped inside a function on the dose estimation modules.
* Finished confounders help dialogue.
* New aberration calculation method using `purrr::map_df()` instead of a nested loops, which is about 10 times faster.
* Make clear in translocations dose estimation module that the coefficients are from the full genome.
* Added highly protracted exposure.
* Implemented calculation of detection limits from data.
* Added dose calculation to detection limits section.
* Changed `σ` to `\sigma` in help dialogues.
* Added units to result tables.
* Code clean-up on delta method for whole-body dose estimation.
* Updated plot CI text generation on legend to account for new Delta method and for separate whole-body & heterogeneous error method selection.
* CONTRIBUTORS.md is now displayed as a modal.
* Using ORCID for authors when possible.
* Updated institution links in contributors files.

## UI Improvements

* Improved `side-widget-tall` bottom margins.
* Moved translocation frequency `selectInput()` next to Calculate Fitting button for better UX.
* New `hot-improved` CSS class for better hot tables' formatting.
* All hot tables are using `hot-improved` class now.
* New contributors list style and custom CSS style for tables.
* Added experiment/aberration name on tab body title to avoid confussions.
* Improved "About" tab paddings.
* Improve widths of color columns in chromosome tables.
* Narrower color columns in chromosome tables.
* All hot tables' widths are dynamic or exact now.
* Improved small action buttons style on help dialogues.
* New results color to differentiate from main color.
* Updated UI to adapt for new Delta method for whole-body estimation.
* Updated translocations UI to adapt for new Delta method for whole-body estimation.
* Added CI for Dolphin dose estimation UI.
* Added (work in progress) Micronuclei into UI.
* Hide language selector until shiny-i18n is implemented.


# biodosetools 2019.06.25-beta

## New features

* Start translocations dose estimation module and report.
* Allow using M-Fish color scheme in translocations fitting module.
* Added method to convert from translocations measured by FISH to full genome.
* Implemented confounders modifications for translocations in `generalEstimateCaseHotTable()` module.

## Improvements

* New chromosome table structure. `transChromosomeTable()` and `transFractionToFullGenomeCalc()` have been moved to `transGeneralModule.R`, as they are reused in the dose estimation module.
* Using new chromosome table UI and server modules for translocations dose estimation.
* Merged "repeated" fitting modules into a `generalFittingModules.R` file.
* Merged `*FittingResults()` modules into `generalFittingResults()` module.
* Updated translocations fitting report to new chromosome table structure.
* New `generalEstimateFittingCurveHotTables()` and generalEstimateFittingCurve() modules.
* New `globalVariables.R` with `global_fitting_formulas` list.
* Added translocations-specific code in `generalEstimateFittingCurve()` module to take into account used translocation frequency.
* New inputs for used translocation frequency and genomic conversion factor in manual fitting input in dose estimation module.
* Moved dose estimation to `generalEstimateResults()` module.
* Modify translocations `(X - Xt)` for whole-body and partial-body dose estimation.
* Added readOnly status to `Xt`, `yt`, `yt_err` variables on `rhandsontable()` for translocations case data.

## UI improvements

* New `innerColumn` to use inside cards/boxes.
* New `widgetLabel()` function for labels above widgets without default label.
* Replaced old labels by new `widgetLabel()` function.
* Finished confounders UI in translocations dose estimation module.
* New `mySwichInput()` function based on `shinyWidgets::switchInput()` with `sideLabel` and custom status colors.
* Moved from `awesomeCheckbox()` to `mySwitchInput()` in all UI modules.
* Moved fitting curve input in translocations' dose estimation module to the top.

## Fixes

* Fix minor error in calculation of infimum yield values.
* Fixed "Color options" card and color_list object for chromosome table generation.
* Fixed missing type argument in `get_model_statistics()` in dose estimation module.
* Deleted heterogeneous estimation for translocations.


# biodosetools 2019.06.15-beta

## New features

* Conditional tabs in sidebar by implementing `bs4MyConditionalSidebarMenuItem()` function.
* Implemented AIC calculation for different dose estimation assessments.
* Added ZIP archive with sample data for testing.
* New dynamic `bs4TabCard()` using `renderUI()` and `session$ns()` to show different tabs depending on user input.
* Implemented manual calculation of case parameters (`N`, `X`, `y`, `y_est`, `DI`, `u`) in dose estimation module.

## Improvements

* Disabled automatic calculation of `N`, `X`, `DI`, `u` to avoid broken input issues when running the deployed app.
* Added `CONTRIBUTORS.md` file.
* Removed {fontawesome} package requirement.
* Added translocation frequency and genomic conversion factor to translocations fitting report.
* Chromosome table included in reports.
* Major rewrite of {ggplot2} plot in dose estimation module.
* Added Dolphin CI info into plot when using partial-body assessment.
* Changed display of CI in plot for dose estimation.
* Genomic conversion factor is used in `transFittingResults()` module for calculations instead of modifying the value of `N` in `transFittingHotTable()`.
* Added theoretical calculation of model-specific statistics (`glm` method) on fitting module.
* Added renormalisation of data for proper calculation of model-specific statistics in fitting module.
* Added theoretical model-specific statistics calculation for constraint `maxlik` optimization method in fitting module.
* Added global `get_model_statistics()` to translocations fitting module.

## UI improvements

* Cleaner version of `topButton()` for help button and help modals on fitting and dose estimation modules..
* More informative model summary dialogue in fitting module.
* Better output of genomic conversion factor.
* "Partial" is now "Partial-body". Better titles in dose estimation results card.
* New bs4MyTabCard() function with topButton and noPadding capabilities.

## Fixes

* Fixed problem with fit_formula_raw when using linear model. Disabled `*-no-int` models until Dolphin can deal with them.
* Fixed rendering problem in Firefox (and potentially other browsers) with count data `rhandsontable()` using 100% of the card's height.
* Fixed dose estimation `rhandsontables()` `div`s on Firefox having `height = 100%` of the card's own height.
* Fixed height error in cases data table in dose estimation module in Firefox.
* Fixed `glm_results object` issue in `get_model_statistics()` function.


# biodosetools 2019.05.24-beta

First public beta for laboratories and research centres.

## New features

* Initial support of reports.
* Implemented selection of CI for dose estimation (83%-83% & 95%-95%).
* Added method selection for partial body exposure estimation.
* Fixed formatting of variance covariance matrices when rendered by `rHandsontableOutput()`.
* Linear models compatibility for partial-body (Dolphin) and heterogeneous dose estimation.
* Experimental version of new Maximum Likelihood Estimation method for dose-effect fitting.
* Manual input of fitting coefficients and var/cov matrix for dose estimation.

## Improvements

* Added format/MIME type validation for file inputs.
* Render fitting formula using MathJax.
* Format curve depending on selected assessment/method (use appropriate CI) on dose estimation.
* Check if yield projection into dose is mathematically possible. Negative values of yield or dose are changed to zero as well.
* Added modification to Merkle's yield error function when using protracted exposures.
* Implemented protracted exposure for partial body dose estimation.
* Added automatic poisson/quasipoison selection on fitting.
* Correct p-values depending on fitting model dispersion.
* Added automatic correction of confidence intervals to use "simplfied" Merkle method if necessary.

## Fixes

* Fixed calculation of full genome.
* Fixed alignment of conditional error/method inputs for dose estimation.
* Changed "base" to "estimate" on CI tables for dose estimation.
* Fixed `bs4TabCard()` bug.


# biodosetools 2019.04.03-alpha

Version presented in second Team Meeting at Munich.

## New features

* Migrated UI from {shinydashboard} to {bs4Dash}.
* Added data, plot, and results export options.
* Added modal dialogues using {shinyBS}.
* Dose estimation modules for dicentrics.
* Complete implementation of dicentrics analysis.
* Experimental fitting for translocations assay.


# biodosetools 2018.11.29-alpha

First draft and proof of concept presented to the RENEB team at Barcelona.

## Features

* UI built using {shinydashboard}.
* Fitting modules for dicentrics.
* Dynamic input tables powered by {rhandsontable}.
