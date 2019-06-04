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

# Function: bs4MyCard----

bs4MyCard <- function(..., title = NULL, footer = NULL, status = NULL, elevation = NULL,
         solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
         width = 6, height = NULL, collapsible = TRUE, collapsed = FALSE,
         closable = TRUE, labelStatus = NULL, labelText = NULL, labelTooltip = NULL,
         dropdownMenu = NULL, dropdownIcon = "wrench", overflow = FALSE,
         topButton = NULL, noPadding = FALSE) {
  cardCl <- if (!is.null(gradientColor)) {
    paste0("card bg-", gradientColor, "-gradient")
  }
  else {
    if (is.null(status)) {
      "card card-default"
    }
    else {
      if (isTRUE(solidHeader)) {
        paste0("card card-outline card-", status)
      }
      else {
        paste0("card card-", status)
      }
    }
  }
  if (isTRUE(collapsible) & isTRUE(collapsed)) {
    cardCl <- paste0(cardCl, " collapsed-card")
  }
  if (!is.null(elevation)) {
    cardCl <- paste0(cardCl, " elevation-", elevation)
  }
  cardToolTag <- shiny::tags$div(class = "card-tools", if (!is.null(labelText) ||
    !is.null(labelStatus) || !is.null(labelTooltip)) {
    shiny::tags$span(
      class = paste0("badge bg-", labelStatus),
      title = if (!is.null(labelTooltip)) {
        labelTooltip
      }, `data-toggle` = "tooltip", labelText
    )
  }, if (!is.null(dropdownMenu)) {
    shiny::tags$div(class = "btn-group", shiny::tags$button(
      type = "button",
      class = "btn btn-tool dropdown-toggle", `data-toggle` = "dropdown",
      shiny::icon(dropdownIcon)
    ), dropdownMenu)
  }, if (!is.null(topButton)) {
    shiny::tags$div(
      class = "btn-group",
      style = "margin-top: -3px;",
      topButton
    )
  }, if (isTRUE(collapsible)) {
    collapseIcon <- if (collapsed) {
      "plus"
    } else {
      "minus"
    }
    shiny::tags$button(
      type = "button", class = "btn btn-tool",
      `data-widget` = "collapse", shiny::icon(collapseIcon)
    )
  }, if (isTRUE(closable)) {
    shiny::tags$button(
      type = "button", class = "btn btn-tool",
      `data-widget` = "remove", shiny::tags$i(class = "fa fa-times")
    )
  })
  if (is.null(title) & (isTRUE(closable) | isTRUE(collapsible))) {
    title <- ""
  }
  headerTag <- shiny::tags$div(class = if (isTRUE(headerBorder)) {
    "card-header"
  } else {
    "card-header no-border"
  }, if (!is.null(title)) {
    shiny::tags$h3(class = "card-title", title)
  } else {
    NULL
  })
  headerTag <- if (!is.null(title)) {
    shiny::tagAppendChild(headerTag, cardToolTag)
  }
  bodyTag <- shiny::tags$div(class = "card-body", style = if (overflow) {
    "overflow-y: auto; max-height: 500px;"
  } else {
    NULL
  }, ...)
  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(class = "card-footer", style = if (overflow) {
      "overflow-y: auto; max-height: 500px;"
    } else {
      NULL
    }, footer)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  cardTag <- shiny::tags$div(class = cardCl, style = if (!is.null(style)) {
    style
  })
  cardTag <- shiny::tagAppendChildren(
    cardTag, headerTag,
    bodyTag, footerTag
  )
  shiny::tags$div(class = if (!is.null(width)) {
      paste0("col-sm-", width)
    }, style = if (noPadding) {
      "padding: 0!important"
    },  cardTag
  )
}


# Function: bs4MyModal ----
bs4MyModal <- function(id, title, trigger, ..., size) {
  if (!missing(size)) {
    if (size == "large") {
      size <- "modal-lg"
    }
    else if (size == "small") {
      size <- "modal-sm"
    }
    size <- paste("modal-dialog", size)
  }
  else {
    size <- "modal-dialog"
  }
  bsTag <- shiny::tags$div(
    class = "modal sbs-modal fade",
    id = id, tabindex = "-1", `data-sbs-trigger` = trigger,
    shiny::tags$div(class = size, shiny::tags$div(
      class = "modal-content",
      shiny::tags$div(
        class = "modal-header", shiny::tags$button(
          type = "button",
          class = "btn-group, close", `data-dismiss` = "modal", shiny::tags$span(shiny::HTML("&times;"))
        ),
        shiny::tags$h4(class = "modal-title", title)
      ),
      shiny::tags$div(class = "modal-body", list(...))#,
      # shiny::tags$div(class = "modal-footer", shiny::tags$button(
      #   type = "button",
      #   class = "btn btn-default", `data-dismiss` = "modal",
      #   "Close"
      # ))
    ))
  )
  # htmltools::attachDependencies(bsTag, shinyBSDep)
}


# Function: bs4MySidebarMenuItem ----
bs4MySidebarMenuItem <- function(..., condition = NULL, tabName = NULL, icon = NULL) {
  shiny::tags$li(class = "nav-item", shiny::tags$a(
    `data-display-if` = condition,
    class = "nav-link",
    id = paste0("tab-", tabName), href = paste0(
      "#shiny-tab-",
      tabName
    ), `data-toggle` = "tab", `data-value` = tabName,
    shiny::tags$i(class = paste0("nav-icon fa fa-", icon)),
    shiny::tags$p(...)
  ))
}
