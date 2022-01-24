
# Biodose Tools <img src="man/figures/logo.png" align="right" width="120"/>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/biodosetools)](https://cran.r-project.org/package=biodosetools)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
[![R-CMD-check](https://github.com/biodosetools-team/biodosetools/workflows/R-CMD-check/badge.svg)](https://github.com/biodosetools-team/biodosetools/actions)
[![pkgdown Workflow
Status](https://github.com/biodosetools-team/biodosetools/workflows/pkgdown/badge.svg)](https://biodosetools-team.github.io/biodosetools/)
[![Codecov test
coverage](https://codecov.io/gh/biodosetools-team/biodosetools/branch/master/graph/badge.svg)](https://app.codecov.io/gh/biodosetools-team/biodosetools?branch=master)
<!-- badges: end -->

## Overview

Biodose Tools is an open source project that aims to be a tool to
perform all different tests and calculations needed by biological
dosimetry laboratories. The app is developed using the
[R](https://www.r-project.org/about.html) programming language and
[Shiny](https://shiny.rstudio.com) as a framework to offer an online,
easy-to-use solution. Although the intention is to provide the
application as a website, all R routines are available as an R package,
which can be downloaded for improvement or personal use.

We also aim to clarify and explain the tests used and to propose those
considered most appropriate. Each laboratory in its routine work should
choose the most suitable method, but the project aims to reach a
consensus that will help us in case of mutual assistance or
intercomparisons.

The project is initially developed by [RENEB](https://www.reneb.net/)
association, but contributions are always welcome.

## Installation

<!-- You can install the released version of <package> from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("biodosetools") -->
<!-- ``` -->
<!-- And  -->

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("biodosetools-team/biodosetools")
```

<!-- ## Examples -->

## Citation

If you use data, results or conclusion from this work, please cite:

> A. Hernández, D. Endesfelder, J. Einbeck, P. Puig, A. Benadjaoud, M.
> Higueras, E. Ainsbury, G. Gruel, U. Kulka, L. Barrios & J. F.
> Barquinero (2022). Biodose Tools: An R Shiny Application for
> Biological Dosimetry. URL
> <https://biodosetools-team.github.io/biodosetools/>

A BibTeX entry for LaTeX users is:

``` bib
@Unpublished{,
  note = {Manuscript under construction},
  author = {Alfredo Hern{'{a}}ndez and David Endesfelder and Jochen Einbeck and Pere Puig and Amine Benadjaoud and Manuel Higueras and Elizabeth Ainsbury and Ga{"{e}}tan Gruel and Ulrike Kulka and Lleonard Barrios and Joan Francesc Barquinero},
  title = {Biodose Tools: An R Shiny Application for Biological Dosimetry},
  year = {2022},
  url = {https://biodosetools-team.github.io/biodosetools/},
}
```
