# Function: bs4DashMySidebar ----

bs4DashMySidebar <- function(..., inputId = NULL, disable = FALSE,
                           title = NULL, skin = "dark", status = "primary",
                           brandColor = NULL, url = NULL, src = NULL,
                           elevation = 4, opacity = .8, expand_on_hover = TRUE,
                           brandElevation = 0, brandTextColor = "white") {

  # brand logo
  brandTag <- if (!is.null(title)) {
    shiny::tags$a(
      class = if (!is.null(brandColor)) paste0("brand-link bg-", brandColor) else "brand-link",
      href = url,
      shiny::tags$img(
        src = src,
        class = paste0("brand-image img-circle elevation-", brandElevation),
        style = paste0("opacity: ", opacity)
      ),
      shiny::tags$span(class = paste0("brand-text font-weight-light ", "text-", brandTextColor), title)
    )
  }

  # sidebar content
  contentTag <- shiny::tags$div(
    class = "sidebar",
    shiny::tags$nav(
      class = "mt-2",
      ...
    )
  )

  sidebarTag <- shiny::tags$aside(
    id = inputId,
    class = paste0(
      "main-sidebar sidebar-", skin, "-",
      status, " elevation-", elevation,
      if (expand_on_hover) NULL else " sidebar-no-expand"
    ),
    style = if (disable) "display: none;"
  )

  sidebarTag <- shiny::tagAppendChildren(sidebarTag, brandTag, contentTag)
  sidebarTag

  customCSS <- shiny::singleton(
    shiny::tags$style(
      ".content-wrapper, .main-footer, .main-header {
          margin-left: 0px;
       }
      "
    )
  )

  if (disable) shiny::tagList(customCSS, sidebarTag) else sidebarTag


  ## change sidebar width
  #shiny::tagList(
  #  shiny::singleton(
  #    shiny::tags$head(
  #      shiny::tags$style(
  #        shiny::HTML(
  #          paste0(
  #            ".main-sidebar, .main-sidebar:before {
  #              transition: margin-left 0.3s ease-in-out, width 0.3s ease-in-out;
  #              width: ", width, "px;
  #            }
  #            .sidebar-mini.sidebar-collapse .main-sidebar:hover {
  #                width: ", width, "px;
  #            }
  #            @media (min-width: 768px) {
  #              .content-wrapper,
  #              .main-footer,
  #              .main-header {
  #                transition: margin-left 0.3s ease-in-out;
  #                margin-left: ", width, "px;
  #                z-index: 3000;
  #              }
  #            }
  #            .nav-sidebar:hover {
  #              overflow: hidden;
  #            }
  #            "
  #          )
  #        )
  #      )
  #    )
  #  ),
  #  sidebarTag
  #)

}


# Function: bs4DashMyNavbar ----

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
    ... # ,
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

# Function: bs4MyCard ----

bs4MyCard <- function(..., inputId = NULL, title = NULL, footer = NULL, status = NULL, elevation = NULL,
                      solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                      width = 6, height = NULL, collapsible = TRUE, collapsed = FALSE,
                      closable = TRUE, maximizable = FALSE, labelStatus = NULL, labelText = NULL,
                      labelTooltip = NULL, dropdownMenu = NULL, dropdownIcon = "wrench",
                      overflow = FALSE, enable_sidebar = FALSE, sidebar_content = NULL,
                      sidebar_width = "25%", sidebar_background = "#333a40",
                      sidebar_start_open = FALSE, sidebar_icon = "cogs",
                      topButton = NULL, noPadding = FALSE) {

  if (!is.null(height) & overflow) {
    stop(
      "overlow and height are not compatible. Please choose only one property.
      When overflow is TRUE, the maximum height is 500px"
    )
  }

  cardCl <- if (!is.null(gradientColor)) {
    paste0("card bg-gradient-", gradientColor)
  } else {
    if (is.null(status)) {
      "card card-default"
    } else {
      if (isTRUE(solidHeader)) {
        paste0("card card-outline card-", status)
      } else {
        paste0("card card-", status)
      }
    }
  }

  if (enable_sidebar) {
    if (sidebar_start_open) {
      cardCl <- paste0(cardCl, " direct-chat direct-chat-contacts-open")
    } else {
      cardCl <- paste0(cardCl, " direct-chat")
    }
  }

  if (isTRUE(collapsible) & isTRUE(collapsed)) cardCl <- paste0(cardCl, " collapsed-card")
  if (!is.null(elevation)) cardCl <- paste0(cardCl, " elevation-", elevation)

  cardToolTag <- shiny::tags$div(
    class = "card-tools",

    # labels
    if (!is.null(labelText) || !is.null(labelStatus) || !is.null(labelTooltip)) {
      shiny::tags$span(
        class = paste0("badge bg-", labelStatus),
        title = if (!is.null(labelTooltip)) labelTooltip,
        `data-toggle` = "tooltip",
        labelText
      )
    },

    if (!is.null(dropdownMenu)) {
      shiny::tags$div(
        class = "btn-group",
        shiny::tags$button(
          type = "button",
          class = "btn btn-tool dropdown-toggle",
          `data-toggle` = "dropdown",
          shiny::icon(dropdownIcon)
        ),
        dropdownMenu
      )
    },

    if (!is.null(topButton)) {
      shiny::tags$div(
        class = "btn-group",
        style = "margin-top: -3px;",
        topButton
      )
    },

    # collapse
    if (isTRUE(collapsible)) {
      collapseIcon <- if (collapsed)
        "plus"
      else "minus"
      shiny::tags$button(
        type = "button",
        class = "btn btn-tool",
        `data-card-widget` = "collapse",
        shiny::icon(collapseIcon)
      )
    },

    # close
    if (isTRUE(closable)) {
      shiny::tags$button(
        type = "button",
        class = "btn btn-tool",
        `data-card-widget` = "remove",
        shiny::tags$i(class = "fa fa-times")
      )
    },

    # maximize
    if (maximizable) {
      shiny::tags$button(
        type = "button",
        class = "btn btn-tool",
        `data-card-widget` = "maximize",
        shiny::tags$i(class = "fa fa-expand")
      )
    },

    # sidebar
    if (enable_sidebar) {
      sidebarTag <- shiny::tags$button(
        class = "btn btn-tool",
        `data-widget` = "chat-pane-toggle",
        `data-toggle` = "tooltip",
        `data-original-title` = "More",
        title = NA,
        type = "button",
        shiny::icon(sidebar_icon)
      )
    }
  )

  # header
  if (is.null(title) & (isTRUE(maximizable) | isTRUE(closable) | isTRUE(collapsible))) title <- "\u200C"

  headerTag <- shiny::tags$div(
    class = if (isTRUE(headerBorder)) "card-header" else "card-header no-border",
    if (!is.null(title)) shiny::tags$h3(class = "card-title", title) else NULL
  )
  headerTag <- if (!is.null(title)) shiny::tagAppendChild(headerTag, cardToolTag)



  # body
  bodyTag <- shiny::tags$div(
    class = "card-body",
    style = if (!is.null(height)) {
      paste0("height: ", shiny::validateCssUnit(height))
    } else {
      if (overflow) "overflow-y: auto; max-height: 500px;" else NULL
    },
    ...,
    if (enable_sidebar) {
      shiny::tags$div(
        style = "z-index: 10000;",
        class = "direct-chat-contacts",
        shiny::tags$ul(
          class = "contacts-list",
          shiny::tags$li(
            style = paste0("width: ", sidebar_width, ";"),
            sidebar_content
          )
        )
      )
    }
  )

  footerTag <- if (!is.null(footer)) {
    shiny::tags$div(
      class = "card-footer",
      style = if (overflow) "overflow-y: auto; max-height: 500px;" else NULL,
      footer
    )
  }

  cardTag <- shiny::tags$div(
    class = cardCl,
    id = inputId
  )
  cardTag <- shiny::tagAppendChildren(cardTag, headerTag, bodyTag, footerTag)

  cardWrapper <- shiny::tags$div(
    class = if (!is.null(width)) {
      paste0("col-sm-", width)
    }, style = if (noPadding) {
      "padding: 0!important"
    }, cardTag
  )

  # for the sidebar
  translation_rate <- paste0("calc(100% - ", sidebar_width, ")")

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            paste0(
              ".direct-chat-contacts {
                 -webkit-transform: translate(100%, 0);
                 -ms-transform: translate(100%, 0);
                 -o-transform: translate(100%, 0);
                 transform: translate(100%, 0);
                 position: absolute;
                 top: 0;
                 bottom: 0;
                 height: 100%;
                 width: 100%;
                 background: ", sidebar_background, ";
                 color: #fff;
                 overflow: auto;
              }
              .direct-chat-contacts-open .direct-chat-contacts {
                -webkit-transform: translate(", translation_rate, ", 0);
                -ms-transform: translate(", translation_rate, ", 0);
                -o-transform: translate(", translation_rate, ", 0);
                transform: translate(", translation_rate, ", 0);
              }
              "
            )
          )
        )
      )
    ),
    cardWrapper
  )

}


# Function: bs4MyTabCard ----
bs4MyTabCard <- function(..., id, title = NULL, status = NULL, elevation = NULL,
                         solidHeader = FALSE, headerBorder = TRUE, gradientColor = NULL,
                         tabStatus = NULL, width = 6, height = NULL, collapsible = TRUE,
                         collapsed = FALSE, closable = TRUE, side = c("left", "right"),
                         topButton = NULL, noPadding = FALSE) {
  found_active <- FALSE
  side <- match.arg(side)
  tabCardCl <- if (!is.null(gradientColor)) {
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
    tabCardCl <- paste0(tabCardCl, " collapsed-card")
  }
  if (!is.null(elevation)) {
    tabCardCl <- paste0(tabCardCl, " elevation-", elevation)
  }
  if (isTRUE(closable) | isTRUE(collapsible)) {
    cardToolTag <- shiny::tags$div(
      class = "tools pt-3 pb-3 pr-2 mr-2",
      if (!is.null(topButton)) {
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
          type = "button", class = "btn btn-tool pb-0 pt-0",
          `data-widget` = "collapse", shiny::icon(collapseIcon)
        )
      }, if (isTRUE(closable)) {
        shiny::tags$button(
          type = "button", class = "btn btn-tool pb-0 pt-0",
          `data-widget` = "remove", shiny::tags$i(class = "fa fa-times")
        )
      }
    )
  }
  else {
    cardToolTag <- shiny::tags$div()
  }
  tabMenu <- bs4Dash::bs4TabSetPanel(..., id = id, side = side, tabStatus = tabStatus)[[2]]
  if (is.null(title) & (isTRUE(closable) | isTRUE(collapsible))) {
    title <- ""
  }
  headerTag <- shiny::tags$div(class = if (isTRUE(headerBorder)) {
    "card-header d-flex p-0"
  } else {
    "card-header d-flex p-0 no-border"
  }, if (side == "right") {
    shiny::tagList(if (!is.null(title)) {
      shiny::tags$h3(class = "card-title p-3", title)
    } else {
      NULL
    }, tabMenu)
  }
  else {
    shiny::tagList(tabMenu, if (!is.null(title)) {
      shiny::tags$h3(
        class = "card-title p-3 ml-auto",
        title
      )
    } else {
      NULL
    })
  })
  headerTag <- if (!is.null(title)) {
    shiny::tagAppendChild(headerTag, cardToolTag)
  }
  panelContent <- bs4Dash::bs4TabSetPanel(...,
                                          id = id, side = side,
                                          tabStatus = tabStatus
  )[c(1, 3)]
  bodyTag <- shiny::tags$div(
    class = "card-body", style = "overflow-y: auto;",
    panelContent
  )
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  tabCardTag <- shiny::tags$div(class = tabCardCl, style = if (!is.null(style)) {
    style
  })
  tabCardTag <- shiny::tagAppendChildren(
    tabCardTag, headerTag,
    bodyTag
  )
  # shiny::tags$div(class = paste0("col-sm-", width), tabCardTag)
  shiny::tags$div(class = if (!is.null(width)) {
    paste0("col-sm-", width)
  }, style = if (noPadding) {
    "padding: 0!important"
  }, tabCardTag)
}


# Function: bs4MyTabPanel ----
bs4MyTabPanel <- function(..., tabName, active = FALSE, noPadding = TRUE) {
  id <- tabName
  id <- gsub(x = id, pattern = "[[:punct:]]", replacement = "")
  id <- gsub(x = id, pattern = " ", replacement = "")

  tabPanelTag <- shiny::tags$div(
    class =
      if (active) {
        "tab-pane active"
      } else {
        "tab-pane"
      },
    style =
      if (noPadding) {
        "padding: 0px;"
      },
    id = id, ...
  )
  return(list(tabName, tabPanelTag))
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
      shiny::tags$div(class = "modal-body", list(...)) # ,
      # shiny::tags$div(class = "modal-footer", shiny::tags$button(
      #   type = "button",
      #   class = "btn btn-default", `data-dismiss` = "modal",
      #   "Close"
      # ))
    ))
  )
  # shinyBSDep <- htmltools::htmlDependency(
  #   "shinyBS", packageVersion("shinyBS"),
  #   src = c("href" = "sbs"),
  #   script = "shinyBS.js",
  #   stylesheet = "shinyBS.css"
  # )

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
