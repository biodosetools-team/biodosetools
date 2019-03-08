# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(rhandsontable)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(shiny.i18n)

# File with translations
i18n <- Translator$new(translation_csvs_path = "translations")
# Set language
i18n$set_translation_language("it")


# Theming
source("libs/theming.R", local = T)

# Modules
source("fittingModule.R")
source("fittingAdvModule.R")
source("estimateModule.R")



# Header ---------------------------------------------------

header <- dashboardHeader(
  # title = span(tagList(icon("calculator"), "Biodose Tool"))
  title = logo_biodose_tools,
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
      choices = c(
        "Dicentrics" = "dicent",
        "Micronuclei",
        "Translocations",
        "H2AX",
        "Intercomparison Tests"
        ),
      selected = "Dicentrics",
      multiple = FALSE,
      selectize = TRUE
    ),

    menuItem("Dose-effect Fitting", tabName = "tab-fitting-main", icon = icon("th-list"), startExpanded = T,
             menuSubItem("Fitting", tabName = "tab-fitting-adv", icon = icon("cogs")),
             menuSubItem("Simplified Fitting", tabName = "tab-fitting-simple",icon = icon("cog"))
    ),


    menuItem("Dose Estimation", tabName = "tab-estimate", icon = icon("calculator"), selected = T),
    # menuItem("Training", tabName = "tab-training", icon = icon("user-check"))
    # menuItem("Check Distribution", tabName = "tab-check-dists", icon = icon("area-chart")),
    # menuItem("Intercomparison Tests", tabName = "tab-inter-test", icon = icon("check-circle"))
    absolutePanel(
      bottom = 10, width = "100%",
      includeMarkdown("footer.md"),
      style = "opacity: 1"
    )
  )
)


# Body -----------------------------------------------------

body <- dashboardBody(
  theme_biodose_tools,
  theme_buttons_biodose_tools,

  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    if (TRUE) {
      tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css")
    }
  ),

  # Home ----
  tabItems(
    tabItem(
      tabName = "home",
      h2("About this project", style = "margin-left: 10%;"),
      includeMarkdown("body.md"),
      # Buttons
      div(
        style = "margin-left: 10%;",
        actionButton(
          inputId = "github_link", label = "Source code",
          icon = icon("github"),
          class = "home-button",
          onclick = "window.open('https://github.com/biodosimetry-uab/biodose-tools', '_blank')"
        ),
        div(class = "widget-sep", br()),
        actionButton(
          inputId = "wiki_link", label = "Documentation",
          icon = icon("book"),
          class = "home-button",
          onclick = "window.open('https://biodosimetry-uab.gitbook.io/wiki/', '_blank')"
        )
      )
    ),


    # Fitting  ----
    fittingUI(id = "fitting", label = "tab-fitting-simple"),

    # Advanced Fitting ----
    fittingAdvUI(id = "adv_fitting", label = "tab-fitting-adv"),

    # Dose Estimation ----
    estimateUI(id = "estimate", label = "tab-estimate", locale = i18n)

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
