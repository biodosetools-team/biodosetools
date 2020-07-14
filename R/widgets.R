#' Help modal trigger button
#'
#' @param inputId id of the button
#' @param id_modal id of the modal dialogue
#'
#' @noRd
help_modal_button <- function(inputId, id_modal) {
  shinyBS::bsButton(
    class = "modal-help-button",
    inputId,
    label = "",
    icon = icon("question"),
    style = "default", size = "default"
  ) %>%
    bsplus::bs_attach_modal(id_modal)
}

# Function: mySwitchInput ----

mySwitchInput <- function(inputId, label = NULL, value = FALSE, onLabel = "ON",
                          offLabel = "OFF", onStatus = NULL, offStatus = NULL, size = "default",
                          labelWidth = "auto", handleWidth = "auto", disabled = FALSE,
                          inline = FALSE, width = NULL, sideLabel = NULL) {
  dropNulls <- function(x) {
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
  if (!is.numeric(width)) { #|| (width < 1) || (width > 12)) {
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
          src = c(href = "shinyWidgets/selectPicker"),
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
          src = c(href = "shinyWidgets/switchInput/bootstrap-switch-3.3.4"),
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
          src = c(href = "shinyWidgets/sweetAlert"),
          script = c("sweetalert.min.js", "sweetalert-bindings.js")
        )
      )
    } else if (widget == "multi") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "multi",
          version = "1.4.0",
          src = c(href = "shinyWidgets/multi"),
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
          script = c(
            "jquery.knob.min.js",
            "knob-input-binding.js"
          )
        )
      )
    } else if (widget == "dropdown") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "dropdown-patch",
          version = version,
          src = c(href = "shinyWidgets/dropdown"),
          script = "dropdown-click.js"
        )
      )
    } else if (widget == "sw-dropdown") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "sw-dropdown",
          version = version,
          src = c(href = "shinyWidgets/sw-dropdown"),
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
          src = c(href = "shinyWidgets/animate"),
          stylesheet = "animate.min.css"
        )
      )
    } else if (widget == "bttn") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "bttn",
          version = version,
          src = c(href = "shinyWidgets/bttn"),
          stylesheet = "bttn.min.css"
        )
      )
    } else if (widget == "spectrum") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "spectrum",
          version = version,
          src = c(href = "shinyWidgets/spectrum"),
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
          src = c(href = "shinyWidgets/pretty-checkbox"),
          stylesheet = "pretty-checkbox.min.css"
        )
      )
    } else if (widget == "nouislider") {
      dep <- list(
        dep,
        htmltools::htmlDependency(
          name = "nouislider",
          version = "11.0.3",
          src = c(href = "shinyWidgets/nouislider"),
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
          src = c(href = "shinyWidgets/air-datepicker"),
          script = c("datepicker.min.js", "datepicker-bindings.js"),
          stylesheet = "datepicker.min.css"
        )
      )
    }
  }
  htmltools::attachDependencies(tag, dep, append = TRUE)
}
