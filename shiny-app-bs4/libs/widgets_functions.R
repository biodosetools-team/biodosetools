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
