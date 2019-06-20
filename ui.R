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

# Dicentrics
source("modules/dicentFittingModule.R")
source("modules/dicentEstimateModule.R")

# Translocations
source("modules/transGeneralModule.R")
source("modules/transFittingModule.R")
source("modules/transEstimateModule.R")

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
