# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(rhandsontable)
library(shinyWidgets)
library(shinyBS)
library(bs4Dash)


# Misc -----------------------------------------------------

source("libs/app_version.R")
# source("translations/translations.R")

# Modules --------------------------------------------------

# Global variables
source("modules/globalVariables.R")
source("modules/helpAuxiliaryModules.R")

# General
source("modules/generalFittingModules.R")
source("modules/generalEstimateModules.R")

# Dicentrics
source("modules/dicentFittingModules.R")
source("modules/dicentEstimateModules.R")

# Translocations
source("modules/transGeneralModules.R")
source("modules/transFittingModules.R")
source("modules/transEstimateModules.R")

# Micronuclei
source("modules/microFittingModules.R")
source("modules/microEstimateModules.R")

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
