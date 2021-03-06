% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_estimation.R
\name{plot_estimated_dose_curve}
\alias{plot_estimated_dose_curve}
\title{Plot dose estimation curve}
\usage{
plot_estimated_dose_curve(
  est_full_doses,
  fit_coeffs,
  fit_var_cov_mat,
  protracted_g_value,
  conf_int_yield,
  conf_int_curve,
  conf_int_text_whole,
  conf_int_text_partial,
  conf_int_text_hetero,
  aberr_name
)
}
\arguments{
\item{est_full_doses}{Data frame with yields and dose estimations. It requires dose, yield, type, level columns}

\item{fit_coeffs}{Fitting coefficients matrix}

\item{fit_var_cov_mat}{Fitting variance-covariance matrix}

\item{protracted_g_value}{Protracted G(x) value}

\item{conf_int_yield}{Confidence interval of the yield}

\item{conf_int_curve}{Confidence interval of the curve}

\item{conf_int_text_whole}{Text to display confidence interval for whole-body estimation}

\item{conf_int_text_partial}{Text to display confidence interval for partial-body estimation}

\item{conf_int_text_hetero}{Text to display confidence interval for heterogeneous estimation}

\item{aberr_name}{Name of the aberration to use in the y-axis}
}
\value{
ggplot object
}
\description{
Plot dose estimation curve
}
