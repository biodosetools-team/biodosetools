# Theming --------------------------------------------------
source("libs/theming.R", local = T)
source("libs/widgets_functions.R", local = T)

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

  div(
    style = "margin-bottom: -20px;",
    selectInput(
      "experiment_select",
      label = NULL,
      choices = c(
        "Dicentrics",
        # "Micronuclei",
        "Translocations"#,
        # "H2AX",
        # "Intercomparison Tests"
      ),
      selected = "Dicentrics",
      multiple = FALSE,
      selectize = TRUE
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

    bs4SidebarMenuItem(
      "About this App",
      tabName = "home",
      icon = "home"
    ),

    # Modules
    # bs4SidebarHeader("Modules"),
    #
    # Dicentrics
    # conditionalPanel(
    #   condition = "input.experiment_select == 'Dicentrics'",
    #   bs4SidebarMenuItem(
    #     "Fitting",
    #     tabName = "tab-fitting-adv",
    #     icon = "cogs"
    #   ),
    #
    #   bs4SidebarMenuItem(
    #     "Simplified fitting",
    #     tabName = "tab-fitting-simple",
    #     icon = "cog"
    #   ),
    #
    #   bs4SidebarMenuItem(
    #     "Dose estimation",
    #     tabName = "tab-estimate",
    #     icon = "calculator"
    #   )
    # ),
    #
    # Translocations
    # conditionalPanel(
    #   condition = "input.experiment_select == 'Translocations'",
    #   bs4SidebarMenuItem(
    #     "Fitting",
    #     tabName = "tab-trans-fitting-adv",
    #     icon = "cogs"
    #   )
    # ),

    bs4SidebarHeader("Dicentrics"),

    bs4SidebarMenuItem(
      "Fitting",
      tabName = "tab-fitting-adv",
      icon = "cogs"
    ),

    bs4SidebarMenuItem(
      "Simplified fitting",
      tabName = "tab-fitting-simple",
      icon = "cog"
    ),

    bs4SidebarMenuItem(
      "Dose estimation",
      tabName = "tab-estimate",
      icon = "calculator"
    ),

    bs4SidebarHeader("Translocations"),

    bs4SidebarMenuItem(
      "Fitting",
      tabName = "tab-trans-fitting-adv",
      icon = "cogs"
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
  html_tags,

  theme_sidebar_color_biodose_tools,
  theme_cards_biodose_tools,
  theme_buttons_biodose_tools,

  bs4TabItems(
    # Home page
    home,

    # Fitting
    fittingUI(id = "fitting", label = "tab-fitting-simple"),

    # Advanced Fitting
    fittingAdvUI(id = "adv_fitting", label = "tab-fitting-adv"),
    transFittingAdvUI(id = "trans_adv_fitting", label = "tab-trans-fitting-adv"),

    # Dose Estimation
    estimateUI(id = "estimate", label = "tab-estimate")# locale = i18n)
  )
)


# Footer ---------------------------------------------------

footer <- bs4DashFooter(
  copyrights = "Version 2.0.0-alpha",
  right_text = "2019"
)
