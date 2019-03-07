# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(rhandsontable)
library(shinyWidgets)
library(shinyBS)

# Locales ----
library(shiny.i18n)

# File with translations
i18n <- Translator$new(translation_csvs_path = "translations")
# Set language
i18n$set_translation_language("it")

source("global.R")

# Modules ----
source("modules/fittingModule.R")
source("modules/fittingAdvModule.R")
source("modules/estimateModule.R")

# UI -------------------------------------------------------


# plot 2
x <- seq(-2 * pi, 2 * pi, length.out = 1000)
df <- data.frame(x, y1 = sin(x), y2 = cos(x))

# plot 3
x <- rnorm(200)
y <- rnorm(200)


# Navbar ---------------------------------------------------

navbar <- bs4DashNavbar(
  skin = "dark",
  status = "white",

  selectInput(
    "experiment_select",
    # label = "Experiment",
    label = NULL,
    choices = c(
      "Dicentrics",
      "Micronuclei",
      "Translocations",
      "H2AX",
      "Intercomparison Tests"
    ),
    selected = "Dicentrics",
    multiple = FALSE,
    selectize = TRUE
  ),

  # Right UI
  rightUi = NULL
)


# Sidebar --------------------------------------------------

sidebar <- bs4DashSidebar(
  skin = "light",
  status = "primary",
  title = "Biodose Tools",
  brandColor = "warning",
  url = NULL,
  src = "icon_small.svg",
  elevation = 1,
  opacity = 1,

  bs4SidebarMenu(
    # bs4SidebarHeader("Cards"),

    bs4SidebarMenuItem(
      "About this App",
      tabName = "home",
      icon = "home"
    ),

    # selectInput(
    #   "experiment_select", "Experiment",
    #   choices = c(
    #     "Dicentrics",
    #     "Micronuclei",
    #     "Translocations",
    #     "H2AX",
    #     "Intercomparison Tests"
    #   ),
    #   selected = "Dicentrics",
    #   multiple = FALSE,
    #   selectize = TRUE
    # ),

    bs4SidebarMenuItem(
      "Dose estimation",
      tabName = "tab-estimate",
      icon = "desktop"
    ),


    # bs4SidebarHeader("Cards"),
    # bs4SidebarMenuItem(
    #   "Basic cards",
    #   tabName = "cards",
    #   icon = "sliders"
    # ),
    # bs4SidebarMenuItem(
    #   "Social cards",
    #   tabName = "socialcards",
    #   icon = "id-card"
    # ),
    # bs4SidebarMenuItem(
    #   "Tab cards",
    #   tabName = "tabcards",
    #   icon = "picture-o"
    # ),
    # bs4SidebarMenuItem(
    #   "Sortable cards",
    #   tabName = "sortablecards",
    #   icon = "object-ungroup"
    # ),
    # bs4SidebarMenuItem(
    #   "Stats elements",
    #   tabName = "statsboxes",
    #   icon = "bank"
    # ),
    # bs4SidebarHeader("Boxes"),
    # bs4SidebarMenuItem(
    #   "Basic boxes",
    #   tabName = "boxes",
    #   icon = "desktop"
    # ),
    # bs4SidebarMenuItem(
    #   "Value/Info boxes",
    #   tabName = "valueboxes",
    #   icon = "suitcase"
    # ),
    # bs4SidebarHeader("BS4 gallery"),
    # bs4SidebarMenuItem(
    #   HTML(
    #     paste(
    #       "Gallery 1",
    #       bs4Badge(
    #         "new",
    #         position = "right",
    #         status = "danger"
    #       )
    #     )
    #   ),
    #   tabName = "gallery1",
    #   icon = "paint-brush"
    # ),
    # bs4SidebarMenuItem(
    #   HTML(
    #     paste(
    #       "Gallery 2",
    #       bs4Badge(
    #         "!",
    #         position = "right",
    #         status = "success"
    #       )
    #     )
    #   ),
    #   tabName = "gallery2",
    #   icon = "map"
    # )

    bs4SidebarHeader("Language"),

    pickerInput(
      inputId = "countries",
      label = NULL,
      multiple = F,
      choices = countries,

      choicesOpt = list(
        content =
          mapply(countries, flags, FUN = function(country, flagUrl) {
            HTML(paste(
              tags$img(src=flagUrl, width=20, height=15),
              country
            ))
          }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
      )
    )
  )
)



# Body -----------------------------------------------------

# Home screen
home <- bs4TabItem(
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
)

# Body
body <- bs4DashBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  bs4TabItems(
    home,
    estimateUI(id = "estimate", label = "tab-estimate")# locale = i18n)
    # basic_cards_tab,
    # social_cards_tab,
    # tab_cards_tab,
    # sortable_cards_tab,
    # statsboxes_tab,
    # boxes_tab,
    # value_boxes_tab,
    # gallery_1_tab,
    # gallery_2_tab
  )
)


# Control Bar ----------------------------------------------

controlbar <- bs4DashControlbar(
  skin = "light",
  title = "My right sidebar",
  setSliderColor(sliderId = 1, "black"),
  sliderInput("obs", "Number of observations:",
              min = 0, max = 1000, value = 500
  ),
  column(
    width = 12,
    align = "center",
    radioButtons(
      "dist",
      "Distribution type:",
      c("Normal" = "norm",
        "Uniform" = "unif",
        "Log-normal" = "lnorm",
        "Exponential" = "exp")
    )
  )
)



# Footer ---------------------------------------------------

footer <- bs4DashFooter(
  copyrights = "Version 2.0.0-alpha",
  right_text = "2019"
)

# Build UI -------------------------------------------------


ui <- bs4DashPage(
  navbar = navbar,
  sidebar = sidebar,
  body = body,
  controlbar = controlbar,
  footer = footer,
  title = "Biodose Tools"
)
