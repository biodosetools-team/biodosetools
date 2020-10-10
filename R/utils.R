.onLoad <- function(libname, pkgname) {
  # Supress YAML warning. See https://github.com/rstudio/rstudio/issues/7545
  options("yaml.eval.expr" = TRUE)
}
