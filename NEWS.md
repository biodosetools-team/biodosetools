# Biodose Tools 3.2.0 (2020-02-10)

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

# Biodose Tools 3.1.0 (2019-10-26)

Unofficial release (didn't change version on `DESCRIPTION` file). This includes some of the changes I discussed with David in Stockholm.

## New features

* Made var-cov matrix optional on dose estimation inputs.
* Fixed calculations to make var-cov matrix optional on dose estimation inputs.

## Fixes

* Added additional package dependencies to `DESCRIPTION` file.
* Following the ISO, Detection limits -> Decision thereshold.

## Changes

* Disabled decision thresholds (for now).
* Hide AIC as a relative quality statistic of the dose estimation.
* Added mean and variance to count data tables in fitting module.

# Biodose Tools 3.0.0 (2019-10-12)

The app is now available as an R package on GitHub (we haven't submited Biodose Tools to CRAN yet).

## New functions

* `runApp()` for launching Biodose Tools.
* `pipe()`/`%>%` imported from `magrittr`.

## Fixes

* Fixed variance calculation for count data.
* Provide fallback method for NB2 when using constraint-maxlik-optimization.

# Biodose Tools 2019.05.24-beta

First public beta for laboratories and research centers.

## Features

* Complete implementation of Dicentrics analysis.
* Experimental fitting on Translocations.

# Biodose Tools 2019.04.03-alpha

Version presented in second Team Meeting at Munich.

# Biodose Tools 2018.11.29-alpha

First draft and proof of concept presented to the RENEB team at Barcelona.
