
# Biodose Tools <img src="man/figures/logo.png" align="right" width="120"/>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/biodosetools)](https://cran.r-project.org/package=biodosetools)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
[![R-CMD-check](https://github.com/biodosetools-team/biodosetools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/biodosetools-team/biodosetools/actions/workflows/R-CMD-check.yaml)
[![pkgdown Workflow
Status](https://github.com/biodosetools-team/biodosetools/workflows/pkgdown/badge.svg)](https://biodosetools-team.github.io/biodosetools/)
[![Codecov test
coverage](https://codecov.io/gh/biodosetools-team/biodosetools/branch/master/graph/badge.svg)](https://app.codecov.io/gh/biodosetools-team/biodosetools?branch=master)
[![doi](https://img.shields.io/badge/DOI-10.1080%2F09553002.2023.2176564-blue)](https://doi.org/10.1080/09553002.2023.2176564)
<!-- badges: end -->

## Overview

Biodose Tools ([Hernández et
al. 2023](https://doi.org/10.1080/09553002.2023.2176564)) is an open
source project that aims to be a tool to perform all different tests and
calculations needed by biological dosimetry laboratories. The app is
developed using the [R](https://www.r-project.org/about.html)
programming language and [Shiny](https://shiny.posit.co/) as a
framework to offer an online, easy-to-use solution. Although the
intention is to provide the application as a website, all R routines are
available as an R package, which can be downloaded for improvement or
personal use.

We also aim to clarify and explain the tests used and to propose those
considered most appropriate. Each laboratory in its routine work should
choose the most suitable method, but the project aims to reach a
consensus that will help us in case of mutual assistance or
intercomparisons.

The project is initially developed by [RENEB](https://www.reneb.net/)
association, but contributions are always welcome.

## Installation

You can install the released version of {biodosetools} from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("biodosetools")
```

Or install the development version from [GitHub](https://github.com/)
with:

``` r
devtools::install_github("biodosetools-team/biodosetools")
```

## Examples

To run the Biodose Tools app locally, we can run the following command
on the R console, which will invoke the Shiny user interface:

``` r
library(biodosetools)
run_app()
```

Detailed examples using {biodosetools}’s Shiny user interface as well as
its R API to perform dose-effect fitting and dose estimation for the
dicentric and translocation assays are available on
<https://biodosetools-team.github.io/documentation/> in the “Using
Biodose Tools” chapters.

## Citation

If you use data, results or conclusion from this work, please cite:

> Hernández, A., Endesfelder, D., Einbeck, J., Puig, P., Benadjaoud, M.
> A., Higueras, M., Ainsbury, E., Gruel, G., Oestreicher, U., Barrios,
> L., & Barquinero, J. F. (2023). Biodose Tools: an R shiny application
> for biological dosimetry. International Journal of Radiation Biology,
> 1-13. <https://doi.org/10.1080/09553002.2023.2176564>

A BibTeX entry for LaTeX users is:

``` bib
@article{BiodoseTools2023,
  author        = {Alfredo Hern{'{a}}ndez and David Endesfelder and Jochen Einbeck and Pedro Puig and Mohamed Amine Benadjaoud and Manuel Higueras and Elizabeth Ainsbury and Ga{"{e}}tan Gruel and Ursula Oestreicher and Leonardo Barrios and Joan Francesc Barquinero},
  title         = {{Biodose Tools: an R shiny application for biological dosimetry}},
  journal       = {International Journal of Radiation Biology},
  pages         = {1-13},
  year          = {2023},
  publisher     = {Taylor & Francis},
  doi           = {10.1080/09553002.2023.2176564},
  note          = {PMID: 36731491},
  URL           = {https://doi.org/10.1080/09553002.2023.2176564},
  eprint        = {https://doi.org/10.1080/09553002.2023.2176564}
}
```
