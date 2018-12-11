# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(rhandsontable)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)

# Theming
source("libs/theming.R", local = T)

# Modules
source("fittingModule.R")


# Header ---------------------------------------------------

header <- dashboardHeader(
  # title = span(tagList(icon("calculator"), "Biodose Tool"))
  title = logo_biodose,
  titleWidth = 280
)


# Sidebar --------------------------------------------------

sidebar <- dashboardSidebar(
  width = 280,
  sidebarMenu(
    id = "sidebarmenu",
    menuItem("About this App", tabName = "home", icon = icon("home"), selected = F),
    selectInput(
      "experiment_select", "Experiment",
      choices = c("Option 1", "Option 2"),
      selected = "Option 1",
      multiple = FALSE,
      selectize = TRUE
    ),
    menuItem("Dose-effect Fitting", tabName = "tab-fitting-b", icon = icon("th-list"), selected = F),
    menuItem("Advanced Fitting", tabName = "tab-fitting-c", icon = icon("th-list"), selected = T),
    menuItem("Dose Estimation", tabName = "tab-estimate", icon = icon("calculator"),
             menuItem('Partial body',
                      tabName = 'a',
                      icon = icon('line-chart'))
             ),
    menuItem("Check Distribution", tabName = "tab-check-dists", icon = icon("area-chart")),
    menuItem("Intercomparison Tests", tabName = "tab-inter-test", icon = icon("check-circle"))
  )
)


# Body -----------------------------------------------------

body <- dashboardBody(
  theme_biodose,

  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Roboto:400,500,700|Roboto+Slab:400,700)"))
  ),

  # Home ----
  tabItems(
    tabItem(
      tabName = "home",
      h2("About this project", style = "margin-left: 10%;"),
      includeMarkdown("body.md"),
      actionButton(
        inputId = "github_link", label = "Source code",
        icon = icon("github"),
        style = "margin-left: 10%; color: #fff; background-color: #6C63FF; border-color: #514bc0",
        onclick = "window.open('https://github.com/biodosimetry-uab/biodose-tools', '_blank')"
      ),
      actionButton(
        inputId = "wiki_link", label = "Documentation",
        icon = icon("book"),
        style = "color: #fff; background-color: #6C63FF; border-color: #514bc0",
        onclick = "window.open('https://biodosimetry-uab.gitbook.io/wiki/', '_blank')"
      )
    ),


    # Fitting  ----
    fittingUI(id = "fitting", label = "tab-fitting-b"),

    # Advanced Fitting ----
    fittingAdvUI(id = "adv_fitting", label = "tab-fitting-c"),


    # Dose Estimation ----
    tabItem(
      tabName = "tab-estimate",
      h2("Dose Estimation")
    )
  )
)


# Build UI -------------------------------------------------

ui <- dashboardPage(
  # skin = "purple",
  title = paste(app_name, collapse = " "),
  header,
  sidebar,
  body
)
