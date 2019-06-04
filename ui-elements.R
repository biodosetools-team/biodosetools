# Theming --------------------------------------------------

source("libs/theming.R", local = T)
source("libs/widgets_functions.R", local = T)
# source("libs/withLocalMathJax.R", local = T)

html_tags <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "content.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "fixes.css"),
  tags$link(rel = "icon", type = "image/x-icon", href = "favicon.png"),
  tags$link(rel = "icon", sizes = "192x192", href="favicon-highres.png")
)

# Navbar ---------------------------------------------------

navbar <- bs4DashMyNavbar(
  skin = "dark",
  status = "white",

  # TODO: Fix sidebar when using this
  div(
    style = "margin-bottom: -20px;",
    selectInput(
      "experiment_select",
      label = NULL,
      width = 175,
      choices = c(
        "Dicentrics"            = "dicent",
        "Translocations"        = "trans"#,
        # "Micronuclei"           = "micro",
        # "H2AX"                  = "h2ax",
        # "Intercomparison Tests" = "intercomp"
      ),
      selected = "Dicentrics",
      multiple = FALSE,
      selectize = TRUE
    )
  ),

  # Bookmarking
  # div(
  #   style = "margin-bottom: 3px; margin-left: 10px; ",
  #   bookmarkButton()
  # ),

  div(
    style = "margin-bottom: 2px; margin-left: 10px; ",
    actionButton(
      inputId = "github_link", label = "Give feedback",
      icon = icon("comment"),
      class = "results-button",
      onclick = "window.open('https://github.com/biodosimetry-uab/biodose-tools/issues/new', '_blank')"
    )
  ),



  # Right UI
  rightUi = NULL
)


# Sidebar --------------------------------------------------

sidebar <- bs4DashMySidebar(
  skin = "light",
  status = "primary",
  title = "Biodose Tools",
  brandColor = "biodose-tools",
  url = NULL,
  src = "icon_small.svg",
  elevation = 1,
  opacity = 1,

  bs4SidebarMenu(

    bs4MySidebarMenuItem(
      "About this App",
      tabName = "home",
      icon = "home"
    ),

    # Modules
    bs4SidebarHeader("Modules"),

    # Dicentrics
    bs4MySidebarMenuItem(
      "Fitting",
      condition = "input.experiment_select == 'dicent'",
      tabName = "tab-dicent-fitting",
      icon = "cog"
    ),
    bs4MySidebarMenuItem(
      "Dose estimation",
      condition = "input.experiment_select == 'dicent'",
      tabName = "tab-dicent-estimate",
      icon = "calculator"
      # )
    ),

    # Translocations
    bs4MySidebarMenuItem(
      condition = "input.experiment_select == 'trans'",
      HTML(
        paste(
          "Fitting",
          bs4Badge(
            "in progress",
            position = "right",
            status = "danger"
          )
        )
      ),
      tabName = "tab-trans-fitting",
      icon = "paint-brush"
    ),

    # Language selector
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

# Home screen ----
home <- bs4TabItem(
  tabName = "home",
  h2("About this project", style = "margin-left: 10%;"),
  shiny::includeMarkdown("www/body.md"),

  # Buttons
  div(
    style = "margin-left: 10%;",

    # GitHub icon
    actionButton(
      inputId = "github_link", label = "Source code",
      icon = icon("github"),
      class = "home-button",
      onclick = "window.open('https://github.com/biodosimetry-uab/biodose-tools', '_blank')"
    ),
    div(class = "widget-sep", br()),

    # Wiki/Documentation icon
    actionButton(
      inputId = "wiki_link", label = "Documentation",
      icon = icon("book"),
      class = "home-button",
      onclick = "window.open('https://biodosimetry-uab.github.io/', '_blank')"
    )
  )
)

# Body object
body <- bs4DashBody(
  html_tags,

  theme_sidebar_color_biodose_tools,
  theme_cards_biodose_tools,
  theme_buttons_biodose_tools,

  bs4TabItems(
    # Home page
    home,

    # Dicentric Modules ----

    # Fitting
    dicentFittingUI(id = "dicent_fitting", label = "tab-dicent-fitting"),
    # Dose Estimation
    dicentEstimateUI(id = "dicent_estimate", label = "tab-dicent-estimate"),# locale = i18n)

    # Translocations Modules ----

    # Fitting
    transFittingUI(id = "trans_fitting", label = "tab-trans-fitting")

    # Micronuclei Modules ----
  )
)


# Footer ---------------------------------------------------

footer <- bs4DashFooter(
  copyrights = a(paste("Version", app_version), href="https://github.com/biodosimetry-uab/biodose-tools/blob/master/NEWS.md"),
  right_text = format(Sys.time(), '%Y')
)
