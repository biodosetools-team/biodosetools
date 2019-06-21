# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(rhandsontable)
library(shinyWidgets)
library(shinyBS)
library(bs4Dash)


# Misc -----------------------------------------------------

source("libs/app_version.R")
source("translations/translations.R")


# Modules --------------------------------------------------

# General
source("modules/generalFittingModules.R")

# Dicentrics
source("modules/dicentFittingModules.R")
source("modules/dicentEstimateModules.R")

# Translocations
source("modules/transGeneralModules.R")
source("modules/transFittingModules.R")
source("modules/transEstimateModules.R")

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
