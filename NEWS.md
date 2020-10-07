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

Unofficial release (didn't change version on `DESCRIPTION` file). This includes some of the changes I discussed with David in Stockholm.

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

* Disable sourcing translations.R for now (until {shiny-i18n} is implemented).


# biodosetools 2019.07.17-beta


# biodosetools 2019.06.25-beta


# biodosetools 2019.06.15-beta


# biodosetools 2019.06.04-beta


# biodosetools 2019.05.24-beta

First public beta for laboratories and research centres.

## New features

* Complete implementation of Dicentrics analysis.
* Experimental fitting on Translocations.

# biodosetools 2019.04.03-alpha

Version presented in second Team Meeting at Munich.

# biodosetools 2018.11.29-alpha

First draft and proof of concept presented to the RENEB team at Barcelona.
