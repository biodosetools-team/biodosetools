#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @noRd
app_ui <- function(request) {
  # Custom theme ----

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

  # Start UI ----
  tagList(
    golem_add_external_resources(),

    theme_button_status(
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
    ),

    # dashboardPage ----
    dashboardPage(
      title = "Biodose Tools",
      skin = "purple",
      header = dashboard_header(),
      sidebar = dashboard_sidebar(),
      body = dashboard_body()
    )
  )
}

#' The dashboard header
#'
#' @import shiny shinydashboard
#' @noRd
dashboard_header <- function() {
  dashboardHeader(
    # title = "Biodose Tools"
    title = span(img(src = "www/icon_small.svg", height = 35), "Biodose Tools"),
    tags$li(
      a(
        strong("About"),
        height = 40,
        href = "https://github.com/biodosimetry-uab/biodosetools",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  )
}

#' The dashboard sidebar
#'
#' @import shiny shinydashboard
#' @noRd
dashboard_sidebar <- function() {
  dashboardSidebar(
    # selectInput(
    #   inputId = "experiment_select",
    #   label = NULL,
    #   width = 175,
    #   choices = c(
    #     "Dicentrics"            = "dicent",
    #     "Translocations"        = "trans",
    #     "Micronuclei"           = "micro" # ,
    #     # "H2AX"                  = "h2ax",
    #     # "Intercomparison Tests" = "intercomp"
    #   ),
    #   selected = "Dicentrics",
    #   multiple = FALSE,
    #   selectize = TRUE
    # ),

    sidebarMenu(
      menuItem(
        "About this app",
        tabName = "home",
        icon = icon("home")
      )
    ),

    p(class = "menu-title", "Assays"),
    sidebarMenu(
      # Dicentrics
      menuItem(
        "Dicentrics",
        tabName = "dicent-menu-item",
        icon = icon("dna"),
        startExpanded = TRUE,
        # Modules
        menuItem(
          "Fitting",
          tabName = "tab-dicent-fitting",
          selected = TRUE,
          icon = icon("cog")
        ),
        menuItem(
          "Dose estimation",
          tabName = "tab-dicent-estimate",
          icon = icon("calculator")
        )
      ),
      # Translocations
      menuItem(
        "Translocations",
        tabName = "trans-menu-item",
        icon = icon("palette"),
        # Modules
        menuSubItem(
          "Fitting",
          tabName = "tab-trans-fitting",
          icon = icon("cog")
        ),
        menuSubItem(
          "Dose estimation",
          tabName = "tab-trans-estimate",
          icon = icon("calculator")
        )
      ),
      # Micronuclei
      menuItem(
        "Micronuclei",
        tabName = "micro-menu-item",
        icon = icon("dot-circle"),
        # Modules
        menuItem(
          "Fitting",
          tabName = "tab-micro-fitting",
          icon = icon("cog"),
          badgeLabel = "wip",
          badgeColor = "green"
        ),
        menuItem(
          "Dose estimation",
          tabName = "tab-micro-estimate",
          icon = icon("calculator"),
          badgeLabel = "wip",
          badgeColor = "green"
        )
      )
    ),

    p(class = "menu-title", "Other tools")
  )
}

#' The dashboard home page
#'
#' @import shiny shinydashboard
#' @noRd
dashboard_home <- function() {
  tabItem(
    tabName = "home",
    div(
      style = "padding-bottom: 20px;",
      h2("About this project", style = "margin-left: 10%;"),
      includeMarkdown(
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
}

#' The dashboard body
#'
#' @import shiny shinydashboard
#' @noRd
dashboard_body <- function() {
  dashboardBody(
    tabItems(
      dashboard_home(),

      tabItem(
        tabName = "dashboard",
        h2("Dashboard tab content")
      ),

      dicentFittingUI(id = "dicent_fitting", label = "tab-dicent-fitting")
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
    favicon(ico = "favicon", ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Biodose Tools"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
