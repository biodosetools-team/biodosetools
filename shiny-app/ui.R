# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(dygraphs)
library(shinyjs)
library(shinyBS)

# Theming
source("libs/dashboardthemes.R", local=T)
source("libs/theming.R", local=T)


# Header ---------------------------------------------------

header <- dashboardHeader(
  # title = span(tagList(icon("calculator"), "Biodose Tool"))
  title = logo_biodose
)


# Widgets --------------------------------------------------



# Sidebar --------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarmenu",
    menuItem("About this App", tabName = "home",  icon = icon("home")),
    menuItem("Dose-effect Fitting (A)", tabName = "tab-fitting-a",  icon = icon("th-list"), selected = F),
    menuItem("Dose-effect Fitting (B)", tabName = "tab-fitting-b",  icon = icon("th-list"), selected = T),
    menuItem("Advanced Fitting", tabName = "tab-fitting-c",  icon = icon("th-list"), selected = F),
    menuItem("Dose Estimation", tabName = "tab-estimate", icon = icon("calculator")),
    menuItem("Check Distribution", tabName = "model-c", icon = icon("area-chart")),
    menuItem("Intercomparison Tests", tabName = "model-d", icon = icon("check-circle"))
  )
)


# Body -----------------------------------------------------

body <- dashboardBody(
  theme_biodose,

  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  tabItems(
    tabItem(tabName = "home",
            h2("About this project"),
            includeMarkdown("body.md"),
            actionButton(
              inputId='ab1', label="Fork this project",
              icon = icon("github"),
              onclick ="window.open(
              'https://github.com/biodosimetry-uab/biodose-tool', '_blank')"
            )
    ),

    # Fitting A ####
    tabItem(tabName = "tab-fitting-a",
            h2("Dose-effect Fitting"),
            fluidRow(
              # Sidebar with input and data
              column(width = 4,
                     box(width = 12,
                         title = "Inputs",
                         status = "primary", solidHeader = F, collapsible = T,
                         # Inputs
                         textInput(inputId = "dose",
                                   label = "Dose",
                                   value = "0,0.1,0.25,0.5,0.75,1,1.5,2,3,4,5"),
                         textInput(inputId = "aberr",
                                   label = "Aberrations",
                                   value = "8,14,22,55,100,109,100,103,108,103,107"),
                         textInput(inputId = "cells",
                                   label = "Cells",
                                   value = "5000,5002,2008,2002,1832,1168,562,332,193,103,59"),
                         # Tooltips
                         bsTooltip("dose", "List of doses",
                                   "right", options = list(container = "body")),
                         bsTooltip("aberr", "Aberrations count",
                                   "right", options = list(container = "body")),
                         bsTooltip("cells", "Cells count",
                                   "right", options = list(container = "body"))
                     ),
                     box(width = 12,
                         title = "Data",
                         status = "primary", solidHeader = F, collapsible = T, collapsed = T,
                         tableOutput('table')
                     )
              ),
              # Main tabBox
              column(width = 8,
                     tabBox(width = 12,
                            side = "left",
                            # height = "500px",
                            # selected = "Tab3",
                            tabPanel("Result of curve fit", verbatimTextOutput("result")),
                            tabPanel("Coefficients", verbatimTextOutput("bstat")),
                            tabPanel("Variance-covariance matrix", verbatimTextOutput("vakoma")),
                            tabPanel("Correlation matrix", verbatimTextOutput("corma"))
                     )
              )

            )
    ),

    # Fitting B ####
    tabItem(tabName = "tab-fitting-b",
            h2("Dose-effect Fitting"),
            fluidRow(
              # Sidebar with input and data
              column(width = 4,
                     box(width = 12,
                         title = "Inputs",
                         status = "primary", solidHeader = F, collapsible = T,
                         # Inputs
                         textInput(inputId = "dose-b",
                                   label = "Dose",
                                   value = "0,0.1,0.25,0.5,0.75,1,1.5,2,3,4,5"),
                         textInput(inputId = "aberr-b",
                                   label = "Aberrations",
                                   value = "8,14,22,55,100,109,100,103,108,103,107"),
                         textInput(inputId = "cells-b",
                                   label = "Cells",
                                   value = "5000,5002,2008,2002,1832,1168,562,332,193,103,59"),
                         # Tooltips
                         bsTooltip("dose-b", "List of doses",
                                   "right", options = list(container = "body")),
                         bsTooltip("aberr-b", "Aberrations count",
                                   "right", options = list(container = "body")),
                         bsTooltip("cells-b", "Cells count",
                                   "right", options = list(container = "body")),
                         # Button
                         actionButton("button_fit", "Calculate")
                     ),
                     box(width = 12,
                         title = "Data",
                         status = "primary", solidHeader = F, collapsible = T, collapsed = T,
                         tableOutput('table-b')
                     )
              ),
              # Main tabBox
              column(width = 8,
                     tabBox(width = 12,
                            side = "left",
                            # height = "500px",
                            # selected = "Tab3",
                            tabPanel("Result of curve fit", verbatimTextOutput("result-b")),
                            tabPanel("Coefficients", verbatimTextOutput("bstat-b")),
                            tabPanel("Variance-covariance matrix", verbatimTextOutput("vakoma-b")),
                            tabPanel("Correlation matrix", verbatimTextOutput("corma-b"))
                     )
              )

            )
    ),

    # Fitting C ####
    tabItem(tabName = "tab-fitting-c",
            h2("Dose-effect Fitting"),
            fluidRow(
              # Sidebar with input and data
              column(width = 12,
                     box(width = 12,
                         title = "Inputs",
                         status = "primary", solidHeader = F, collapsible = T,
                         numericInput("dose_num", "Dose", value = 1),
                         fluidRow(column(3, verbatimTextOutput("dose_num_value")))
                     )
                     # box(width = 12,
                     #     title = "Data",
                     #     status = "primary", solidHeader = F, collapsible = T, collapsed = T,
                     #     tableOutput('table')
                     # )
              )#,
              # Main tabBox
              # column(width = 8,
              #        tabBox(width = 12,
              #               side = "left",
              #               # height = "500px",
              #               # selected = "Tab3",
              #               tabPanel("Result of curve fit", verbatimTextOutput("result")),
              #               tabPanel("Coefficients", verbatimTextOutput("bstat")),
              #               tabPanel("Variance-covariance matrix", verbatimTextOutput("vakoma")),
              #               tabPanel("Correlation matrix", verbatimTextOutput("corma"))
              #        )
              # )

            )
    ),

    # Model C ####
    tabItem(tabName = "tab-estimate",
            h2("Dose Estimation")
    )

  ),

  fluidRow(
    column(width = 12,
           uiOutput('ui_plots')
    )
  )
)


# Build UI -------------------------------------------------

ui <- dashboardPage(
  # skin = "purple",
  title="Biodose Tools Alpha",
  header,
  sidebar,
  body
)
