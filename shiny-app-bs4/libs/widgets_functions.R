# Function: bs4DashMySidebar ----

bs4DashMySidebar <- function(..., title = NULL, skin = "dark", status = "primary",
                             brandColor = NULL, url = NULL, src = NULL, elevation = 4,
                             opacity = 0.8) {
  brandTag <- if (!is.null(title)) {
    shiny::tags$a(class = if (!is.null(brandColor)) {
      paste0("brand-link bg-", brandColor)
    } else {
      "brand-link"
    }, href = url, shiny::tags$img(
      src = src,
      class = "brand-image img-circle elevation-0", style = paste0(
        "opacity: ",
        opacity
      )
    ), shiny::tags$span(
      class = "brand-text font-weight-bold text-white",
      title
    ))
  }
  contentTag <- shiny::tags$div(class = "sidebar", shiny::tags$nav(
    class = "mt-2",
    ...
  ))
  sidebarTag <- shiny::tags$aside(class = paste0(
    "main-sidebar sidebar-",
    skin, "-", status, " elevation-", elevation
  ))
  sidebarTag <- shiny::tagAppendChildren(
    sidebarTag, brandTag,
    contentTag
  )
  sidebarTag
}


# Function: bs4DashMyNavbar----

bs4DashMyNavbar <- function(..., skin = "light", status = "white", border = TRUE,
                            sidebarIcon = "bars", controlbarIcon = "th", leftUi = NULL,
                            rightUi = NULL, fixed = FALSE) {
  navbarTag <- shiny::tags$nav(
    class = paste0(
      "main-header navbar navbar-expand bg-",
      status, " navbar-", skin, if (isTRUE(border)) {
        " border-bottom"
      } else {
        NULL
      }, if (fixed) {
        " fixed-top"
      } else {
        NULL
      }
    ), shiny::tags$ul(class = "navbar-nav", shiny::tags$li(
      class = "nav-item",
      shiny::tags$a(
        class = "nav-link", `data-widget` = "pushmenu",
        href = "#", shiny::icon(sidebarIcon)
      )
    ), leftUi),
    ...#,
    # shiny::tags$ul(
    #   class = "navbar-nav ml-auto", rightUi,
    #   shiny::tags$li(class = "nav-item", shiny::tags$a(
    #     id = "controlbar-toggle",
    #     class = "nav-link", `data-widget` = "control-sidebar",
    #     `data-slide` = "true", href = "#", shiny::icon(controlbarIcon)
    #   ))
    # )
  )
  shiny::tagList(
    shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML(paste0(
      ".fa-",
      sidebarIcon, "{\n                 color: #000;\n              }\n               .fa-",
      controlbarIcon, "{\n                 color: #000;\n               }\n              "
    ))))),
    navbarTag
  )
}
