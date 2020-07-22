#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @noRd
app_ui <- function(request) {

  tagList(
    golem_add_external_resources(),

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

    p(class = "menu-title", "Other tools"),

    p(class = "sticky-footer",
      a(paste("Version", utils::packageVersion(pkg = "biodosetools")), href = "https://github.com/biodosimetry-uab/biodosetools/blob/master/NEWS.md"))
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

      # Modules
      mod_dicent_fitting_ui(id = "dicent_fitting", label = "tab-dicent-fitting"),
      mod_dicent_estimate_ui(id = "dicent_estimate", label = "tab-dicent-estimate"),
      mod_trans_fitting_ui(id = "trans_fitting", label = "tab-trans-fitting"),
      mod_trans_estimate_ui(id = "trans_estimate", label = "tab-trans-estimate"),
      mod_micro_fitting_ui(id = "micro_fitting", label = "tab-micro-fitting"),
      mod_micro_estimate_ui(id = "micro_estimate", label = "tab-micro-estimate")
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
