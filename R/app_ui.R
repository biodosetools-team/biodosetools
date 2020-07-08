#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  html_tags <- tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "content.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "fixes.css"),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.png"),
    tags$link(rel = "icon", sizes = "192x192", href = "favicon-highres.png")
  )

  # Random stuff ----

  app_version <- utils::packageVersion(pkg = "biodosetools")

  # Custom theme ---------------------------------------------

  # Custom colors
  head_color <- "#2EC27E"
  head_color_alt <- "#5B5EA8"
  accent_color <- "#311B92"
  main_color <- "#2EC27E"

  # Home colors
  home_color <- "#6C63FF"
  home_color_border <- "#514bc0"
  home_color_hover <- "#8b84ff"

  # Options colors
  options_color <- "#2B7C9A"
  options_color_border <- "#26618a"
  options_color_hover <- "#27a1d0"

  # Inputs colors
  inputs_color <- "#5B5EA8"
  inputs_color_border <- "#4c44ac"
  inputs_color_hover <- "#7275ce"

  # Results colors
  results_color <- "#2db77d"
  results_color_border <- "#13794a"
  results_color_hover <- "#37e495"

  # Export colors
  export_color <- "#F6A945"
  export_color_border <- "#eb8a0b"
  export_color_hover <- "#ffba63"


  # Sidebar theming ------------------------------------------

  theme_sidebar_color_biodose_tools <- bs4DashSidebarColor(
    back_color = main_color
  )


  # Cards theming --------------------------------------------

  theme_cards_biodose_tools <- bs4DashCardsStatus(
    options_color = options_color,
    inputs_color = inputs_color,
    results_color = results_color,
    export_color = export_color
  )


  # Button theming -------------------------------------------

  theme_buttons_biodose_tools <- bs4DashButtonsStatus(
    # Home colors
    home_color = home_color,
    home_color_border = home_color_border,
    home_color_hover = home_color_hover,

    # Options colors
    options_color = options_color,
    options_color_border = options_color_border,
    options_color_hover = options_color_hover,

    # Inputs colors
    inputs_color = inputs_color,
    inputs_color_border = inputs_color_border,
    inputs_color_hover = inputs_color_hover,

    # Results colors
    results_color = results_color,
    results_color_border = results_color_border,
    results_color_hover = results_color_hover,

    # Export colors
    export_color = export_color,
    export_color_border = export_color_border,
    export_color_hover = export_color_hover
  )


  # UI elements ----

  # Navbar

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
          "Translocations"        = "trans",
          "Micronuclei"           = "micro" # ,
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
        onclick = "window.open('https://github.com/biodosimetry-uab/biodosetools/issues/new', '_blank')"
      )
    ),



    # Right UI
    rightUi = NULL
  )


  # Sidebar

  sidebar <- bs4DashMySidebar(
    skin = "light",
    status = "primary",
    title = "Biodose Tools",
    brandColor = "biodose-tools",
    url = NULL,
    src = "www/icon_small.svg",
    elevation = 1,
    opacity = 1,

    bs4Dash::bs4SidebarMenu(
      bs4MySidebarMenuItem(
        "About this App",
        tabName = "home",
        icon = "home"
      ),

      # Modules
      bs4Dash::bs4SidebarHeader("Modules"),

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
      ),

      # Translocations
      bs4MySidebarMenuItem(
        "Fitting",
        condition = "input.experiment_select == 'trans'",
        tabName = "tab-trans-fitting",
        icon = "paint-brush"
      ),
      bs4MySidebarMenuItem(
        "Dose estimation",
        condition = "input.experiment_select == 'trans'",
        tabName = "tab-trans-estimate",
        icon = "calculator"
      ),

      bs4MySidebarMenuItem(
        condition = "input.experiment_select == 'micro'",
        HTML(
          paste(
            "Fitting",
            bs4Dash::bs4Badge(
              "in progress",
              position = "right",
              status = "danger"
            )
          )
        ),
        tabName = "tab-micro-fitting",
        icon = "cog"
      ),

      bs4MySidebarMenuItem(
        condition = "input.experiment_select == 'micro'",
        HTML(
          paste(
            "Dose estimation",
            bs4Dash::bs4Badge(
              "in progress",
              position = "right",
              status = "danger"
            )
          )
        ),
        tabName = "tab-micro-estimate",
        icon = "calculator"
      ) # ,


      # Language selector
      # bs4SidebarHeader("Language"),
      #
      # pickerInput(
      #   inputId = "countries",
      #   label = NULL,
      #   multiple = F,
      #   choices = countries,
      #
      #   choicesOpt = list(
      #     content =
      #       mapply(countries, flags, FUN = function(country, flagUrl) {
      #         HTML(paste(
      #           tags$img(src=flagUrl, width=20, height=15),
      #           country
      #         ))
      #       }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
      #   )
      # )
    )
  )



  # Home screen
  home <- bs4Dash::bs4TabItem(
    tabName = "home",
    div(
      style = "padding-bottom: 20px;",
      h2("About this project", style = "margin-left: 10%;"),
      shiny::includeMarkdown(
        # "www/about_body.md"
        system.file("app/www/about_body.md", package = "biodosetools")
      ),

      # Buttons
      div(
        style = "margin-left: 10%;",

        # GitHub icon
        actionButton(
          inputId = "github_link", label = "Source code",
          icon = icon("github"),
          class = "home-button",
          onclick = "window.open('https://github.com/biodosimetry-uab/biodosetools/', '_blank')"
        ),
        div(class = "widget-sep", br()),

        # Wiki/Documentation icon
        actionButton(
          inputId = "wiki_link", label = "Documentation",
          icon = icon("book"),
          class = "home-button",
          onclick = "window.open('https://biodosimetry-uab.github.io/documentation/', '_blank')"
        )
      )
    )
  )

  # Body object
  body <- bs4Dash::bs4DashBody(
    html_tags,

    theme_sidebar_color_biodose_tools,
    theme_cards_biodose_tools,
    theme_buttons_biodose_tools,

    bs4Dash::bs4TabItems(
      # Home page
      home,

      # Dicentric Modules

      # Fitting
      dicentFittingUI(id = "dicent_fitting", label = "tab-dicent-fitting"),
      # Dose Estimation
      dicentEstimateUI(id = "dicent_estimate", label = "tab-dicent-estimate"), # locale = i18n)

      # Translocations Modules

      # Fitting
      transFittingUI(id = "trans_fitting", label = "tab-trans-fitting"),
      # Dose Estimation
      transEstimateUI(id = "trans_estimate", label = "tab-trans-estimate"),

      # Micronuclei Modules

      # Fitting
      microFittingUI(id = "micro_fitting", label = "tab-micro-fitting"),
      # Dose Estimation
      microEstimateUI(id = "micro_estimate", label = "tab-micro-estimate")
    )
  )


  footer <- bs4Dash::bs4DashFooter(
    copyrights = a(paste("Version", app_version), href = "https://github.com/biodosimetry-uab/biodosetools/blob/master/NEWS.md"),
    # right_text = format(Sys.time(), '%Y')
    right_text = a(id = "contributors", "Contributors", href = "https://github.com/biodosimetry-uab/biodosetools/blob/master/CONTRIBUTORS.md"),

    bs4MyModal(
      id = "contributors_dialog",
      title = "Biodose Tools Contributors",
      trigger = "contributors",
      size = "large",

      shiny::includeMarkdown(
        # "www/contributors_app.md"
        system.file("app/www/contributors_app.md", package = "biodosetools")
      )
    )
  )


  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    bs4Dash::bs4DashPage(
      sidebar_collapsed = TRUE,
      navbar = navbar,
      sidebar = sidebar,
      body = body,
      controlbar = NULL,
      footer = footer,
      title = "Biodose Tools"
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "biodosetools"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
