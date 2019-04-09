# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(rhandsontable)
library(shinyWidgets)
library(shinyBS)
library(bs4Dash)


# Translations ---------------------------------------------

source("translations/translations.R")


# Modules --------------------------------------------------

# Dicentrics
source("modules/fittingModule.R")
source("modules/fittingAdvModule.R")
source("modules/estimateModule.R")

# Translocations
source("modules/transFittingAdvModule.R")

# Build UI -------------------------------------------------

source("ui-elements.R")

ui <- function() {
  bs4DashPage(
    sidebar_collapsed = TRUE,
    navbar = navbar,
    sidebar = sidebar,
    body = body,
    controlbar = NULL,
    footer = footer,
    title = "Biodose Tools"
  )
}
