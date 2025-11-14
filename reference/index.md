# Package index

## Shiny app

Run Biodose Tools Shiny app.

- [`run_app()`](run_app.md) : Run the Shiny Application

## Fitting

Fit and visualise calibration dose-effect curve.

- [`fit()`](fit.md) : Perform dose-effect fitting algorithm
- [`plot_fit_dose_curve()`](plot_fit_dose_curve.md) : Plot fit dose
  curve

## Dose estimation

Estimate and visualise uncertainties on dose estimations.

- [`estimate_whole_body_merkle()`](estimate_whole_body_merkle.md) :
  Whole-body dose estimation (Merkle's method)
- [`estimate_whole_body_delta()`](estimate_whole_body_delta.md) :
  Whole-body dose estimation (delta method)
- [`estimate_partial_body_dolphin()`](estimate_partial_body_dolphin.md)
  : Partial-body dose estimation (Dolphin's method)
- [`estimate_hetero_mixed_poisson()`](estimate_hetero_mixed_poisson.md)
  : Heterogeneous dose estimation (Mixed Poisson model)
- [`plot_estimated_dose_curve()`](plot_estimated_dose_curve.md) : Plot
  dose estimation curve

## Auxiliary

Auxiliary functions used in the fitting and dose estimation pipelines.

- [`calculate_aberr_table()`](calculate_aberr_table.md) : Calculate
  aberrations table
- [`protracted_g_function()`](protracted_g_function.md) : Calculate
  protracted function \\G(x)\\
- [`calculate_genome_factor()`](calculate_genome_factor.md) : Calculate
  genomic conversion factor
- [`calculate_trans_rate_sigurdson()`](calculate_trans_rate_sigurdson.md)
  : Calculate Sigurdson's translocation rate
- [`calculate_trans_rate_manual()`](calculate_trans_rate_manual.md) :
  Calculate manual translocation rate

## Intercomparison

Graphics and functions for the interlaboratory comparison module.

- [`calc.zValue.new()`](calc.zValue.new.md) : Calculate ZScore
- [`bar_plots()`](bar_plots.md) : Bar plots dosimetry
- [`curves_plot()`](curves_plot.md) : Plot curves
- [`DI_plot()`](DI_plot.md) : Dispersion index
- [`dose_boxplot()`](dose_boxplot.md) : Boxplot dose estimates
- [`M_estimate()`](M_estimate.md) : Calculate algB
- [`plot_deviation_all()`](plot_deviation_all.md) : Plot Deviaition from
  ref dose all blind samples
- [`plot_interlab_deviation()`](plot_interlab_deviation.md) : Plot
  deviation
- [`plot_interlab_v2()`](plot_interlab_v2.md) : Plot zscore v2
- [`plot_triage()`](plot_triage.md) : Plot triage
- [`plot_triage_interlab()`](plot_triage_interlab.md) : Plot triage
  interlab
- [`plot_zscore_all()`](plot_zscore_all.md) : Plot Z-scores all blind
  samples
- [`QHampel()`](QHampel.md) : Calculate QHampel
- [`u_test_plot()`](u_test_plot.md) : U-test
- [`yield_boxplot()`](yield_boxplot.md) : Boxplot yield

## criticality accidents

- [`dose_estimation_mx()`](dose_estimation_mx.md) : Prepare data to dose
  estimate using mixed fields manual
- [`fun.estimate.criticality()`](fun.estimate.criticality.md) :
  Calculate absorbed dose for mixed fields exposure
- [`plot_estimated_dose_curve_mx()`](plot_estimated_dose_curve_mx.md) :
  Criticality Plot

## characteristic limits

- [`calculate_table()`](calculate_table.md) : Calculate the
  characteristic limits table
