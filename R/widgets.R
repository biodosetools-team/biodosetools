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
  }, cardTag)
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


# Function: mySwitchInput ----
mySwitchInput <- function(inputId, label = NULL, value = FALSE, onLabel = "ON",
                          offLabel = "OFF", onStatus = NULL, offStatus = NULL, size = "default",
                          labelWidth = "auto", handleWidth = "auto", disabled = FALSE,
                          inline = FALSE, width = NULL, sideLabel = NULL) {
  dropNulls <- function (x) {
    x[!vapply(x, is.null, FUN.VALUE = logical(1))]
  }

  value <- shiny::restoreInput(id = inputId, default = value)
  size <- match.arg(arg = size, choices = c(
    "default", "mini",
    "small", "normal", "large"
  ))
  switchProps <- dropNulls(list(
    id = inputId, type = "checkbox",
    class = "sw-switchInput", `data-input-id` = inputId,
    `data-on-text` = onLabel, `data-off-text` = offLabel,
    `data-label-text` = label, `data-on-color` = onStatus,
    `data-off-color` = offStatus, `data-label-width` = labelWidth,
    `data-handle-width` = handleWidth, disabled = if (!disabled) NULL else disabled,
    `data-size` = if (size == "default") "" else size
  ))
  switchProps <- lapply(switchProps, function(x) {
    if (identical(x, TRUE)) {
      "true"
    } else if (identical(x, FALSE)) {
      "false"
    } else {
      x
    }
  })
  inputTag <- do.call(htmltools::tags$input, switchProps)
  if (!is.null(value) && value) {
    inputTag$attribs$checked <- "checked"
  }
  switchInputTag <- htmltools::tags$div(
    class = "form-group shiny-input-container",
    class = if (inline) {
      "shiny-input-container-inline"
    }, style = if (inline) {
      "display: inline-block;"
    }, style = if (!is.null(width)) {
      paste0(
        "width: ", htmltools::validateCssUnit(width),
        ";"
      )
    }, style = if (size == "mini") {
      "margin-bottom: 5px;"
    },
    inputTag,
    if (!is.null(sideLabel)) {
      htmltools::tags$div(
        style = "vertical-align: top; display: inline-block; padding-left: 4px; max-width: 70%;",
        p(sideLabel)
      )
    }
  )
  attachShinyWidgetsDep(switchInputTag, "bsswitch")
}


# Function: innerColumn ----
innerColumn <- function(width, ..., offset = 0) {
  if (!is.numeric(width)) {#|| (width < 1) || (width > 12)) {
    stop("column width must be between 1 and 12")
  }
  colClass <- paste0("col-inner-", width)
  if (offset > 0) {
    colClass <- paste0(colClass, " col-sm-offset-", offset)
  }
  div(class = colClass, ...)
}



# Function: attachShinyWidgetsDep ----
# Used in mySwitchInput
# Source: shinyWidgets

#' Attach shinyWidgets dependancies
#'
#' @param tag An object which has (or should have) HTML dependencies.
#' @param widget Name of a widget for particular dependancies
#'
#' @noRd
#' @importFrom utils packageVersion
#' @importFrom htmltools htmlDependency attachDependencies findDependencies
#' @importFrom shiny icon
#'
attachShinyWidgetsDep <- function(tag, widget = NULL) {
  version <- as.character(packageVersion("shinyWidgets")[[1]])
  dep <- htmltools::htmlDependency(
    name = "shinyWidgets", version = version,
    src = c(href = "shinyWidgets"),
    script = "shinyWidgets-bindings.min.js",
    stylesheet = "shinyWidgets.css"
  )
  if (!is.null(widget)) {
    if (widget == "picker") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "selectPicker",
          version = "1.13.3",
          src = c(href="shinyWidgets/selectPicker"),
          script = "js/bootstrap-select.min.js",
          stylesheet = "css/bootstrap-select.min.css"
        )
      )
    } else if (widget == "awesome") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "awesome-bootstrap",
          version = "0.2.0",
          src = c(href = "shinyWidgets/awesomeRadioCheckbox"),
          stylesheet = "css/awesome-bootstrap-checkbox-shiny.css"
        ),
        htmltools::findDependencies(shiny::icon("rebel"))[[1]]
      )
    } else if (widget == "bsswitch") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "bootstrap-switch",
          version = "3.3.4",
          src = c(href="shinyWidgets/switchInput/bootstrap-switch-3.3.4"),
          script = "bootstrap-switch.min.js",
          stylesheet = "bootstrap-switch.min.css"
        )
      )
    } else if (widget == "sweetalert") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "sweetAlert",
          version = "0.2.0",
          src = c(href="shinyWidgets/sweetAlert"),
          script = c("sweetalert.min.js", "sweetalert-bindings.js")
        )
      )
    } else if (widget == "multi") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "multi",
          version = "1.4.0",
          src = c(href="shinyWidgets/multi"),
          script = "multi.min.js",
          stylesheet = c("multi.min.css")
        )
      )
    } else if (widget == "jquery-knob") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "jquery-knob", version = "1.2.13",
          src = c(href = "shinyWidgets/jquery-knob"),
          script = c("jquery.knob.min.js",
                     "knob-input-binding.js")
        )
      )
    } else if (widget == "dropdown") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "dropdown-patch",
          version = version,
          src = c(href="shinyWidgets/dropdown"),
          script = "dropdown-click.js"
        )
      )
    } else if (widget == "sw-dropdown") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "sw-dropdown",
          version = version,
          src = c(href="shinyWidgets/sw-dropdown"),
          script = "sw-dropdown.js",
          stylesheet = "sw-dropdown.css"
        )
      )
    } else if (widget == "animate") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "animate",
          version = version,
          src = c(href="shinyWidgets/animate"),
          stylesheet = "animate.min.css"
        )
      )
    } else if (widget == "bttn") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "bttn",
          version = version,
          src = c(href="shinyWidgets/bttn"),
          stylesheet = "bttn.min.css"
        )
      )
    } else if (widget == "spectrum") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "spectrum",
          version = version,
          src = c(href="shinyWidgets/spectrum"),
          script = c("spectrum.min.js"),
          stylesheet = c("spectrum.min.css", "sw-spectrum.css")
        )
      )
    } else if (widget == "pretty") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "pretty",
          version = "3.0.3",
          src = c(href="shinyWidgets/pretty-checkbox"),
          stylesheet = "pretty-checkbox.min.css"
        )
      )
    } else if (widget == "nouislider") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "nouislider",
          version = "11.0.3",
          src = c(href="shinyWidgets/nouislider"),
          script = c("nouislider.min.js", "wNumb.js"),
          stylesheet = "nouislider.min.css"
        )
      )
    } else if (widget == "airdatepicker") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "air-datepicker",
          version = "2.2.3",
          src = c(href="shinyWidgets/air-datepicker"),
          script = c("datepicker.min.js", "datepicker-bindings.js"),
          stylesheet = "datepicker.min.css"
        )
      )
    }
  }
  htmltools::attachDependencies(tag, dep, append = TRUE)
}
