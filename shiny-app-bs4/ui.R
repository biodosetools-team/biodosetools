# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(rhandsontable)
library(shinyWidgets)
library(shinyBS)
library(bs4Dash)


# Locales ----
library(shiny.i18n)

source("global.R")

# File with translations
i18n <- Translator$new(translation_csvs_path = "translations")
# Set language
i18n$set_translation_language("it")


# Modules ----

source("modules/fittingModule.R")
source("modules/fittingAdvModule.R")
source("modules/estimateModule.R")

# Build UI -------------------------------------------------

source("ui-elements.R")

ui <- bs4DashPage(
  sidebar_collapsed = TRUE,
  navbar = navbar,
  sidebar = sidebar,
  body = body,
  controlbar = NULL,
  footer = footer,
  title = "Biodose Tools"
)
