## app.R ##shi
library(shiny)
library(shinydashboard)
# library(shinyjs)
library(markdown)

# Source UI and Server -------------------------------------

options(shiny.sanitize.errors = FALSE)

source("ui.R", local=F)
source("server.R", local=F)

shinyApp(ui, server)
