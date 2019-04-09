# Libraries ------------------------------------------------

library(shiny)
library(fontawesome)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(bs4Dash)

# Source UI and Server -------------------------------------

options(shiny.sanitize.errors = FALSE)

source("ui.R", local=F)
source("server.R", local=F)

sihny::enableBookmarking(store = "url")
shiny::shinyApp(ui, server)
