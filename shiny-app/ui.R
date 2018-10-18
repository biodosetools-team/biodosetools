# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(dygraphs)
library(shinyjs)

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
    menuItem("Poisson Fit", tabName = "model-a",  icon = icon("circle"), selected = T),
    menuItem("Quasi-poisson Fit", tabName = "model-b", icon = icon("square"))
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

    # Model A ####
    tabItem(tabName = "model-a",
            h2("Model A"),
            fluidRow(
              # Sidebar with input and data
              column(width = 4,
                     box(width = 12,
                         title = "Inputs",
                         status = "primary", solidHeader = F, collapsible = T,
                         textInput('dose', 'Dose',         "0,0.1,0.25,0.5,0.75,1,1.5,2,3,4,5"),
                         textInput('aberr', 'Aberrations', "8,14,22,55,100,109,100,103,108,103,107"),
                         textInput('cells', 'Cells',       "5000,5002,2008,2002,1832,1168,562,332,193,103,59")
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

    # Model B ####
    tabItem(tabName = "model-b",
            h2("Model B")
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
  header,
  sidebar,
  body
)
