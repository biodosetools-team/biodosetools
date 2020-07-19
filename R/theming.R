#' Creates a custom theme object for a shinydashboard application
#'
#'
#' @return HTML code. Theme for a shinydashboard application.
#' @noRd
theme_button_status <- function(home_color, home_color_hover, home_color_border,
                                options_color, options_color_hover, options_color_border,
                                inputs_color, inputs_color_hover, inputs_color_border,
                                results_color, results_color_hover, results_color_border,
                                export_color, export_color_hover, export_color_border) {
  htmltools::tags$head(
    htmltools::tags$style(
      htmltools::HTML(
        paste0(
          "
          /* File selector widget */

          .btn.btn-file {
            border-top-right-radius: 0;
            border-bottom-right-radius: 0;
            border-style: solid none solid solid;
          }

          .progress {
            border-radius: 3px;
            height: 25px;
            margin-top: 5px;
            margin-bottom: 10px;
          }

          .progress-bar {
            font-size: 15px;
            line-height: 25px;
            background-color: ", options_color, ";
          }

          /* Home Button */

          .home-button {
            color: #fff;
            background-color: ", home_color, ";
            border-color:  ", home_color_border, ";
          }

          .home-button:focus {
            color: #fff;
            background-color: ", home_color, ";
            border-color:  ", home_color_border, ";
          }

          .home-button:hover {
            color: #fff;
            background-color: ", home_color_hover, ";
            border-color:  ", home_color_border, ";
          }

          /* Options Button */
          .options-button {
            color: #fff;
            background-color: ", options_color, ";
            border-color:  ", options_color_border, ";
          }

          .options-button:focus {
            color: #fff;
            background-color: ", options_color, ";
            border-color:  ", options_color_border, ";
          }

          .options-button:hover {
            color: #fff;
            background-color: ", options_color_hover, ";
            border-color:  ", options_color_border, ";
          }

          /* Inputs Button */
          .inputs-button {
            color: #fff;
            background-color: ", inputs_color, ";
            border-color:  ", inputs_color_border, ";
          }

          .inputs-button:focus {
            color: #fff;
            background-color: ", inputs_color, ";
            border-color:  ", inputs_color_border, ";
          }

          .inputs-button:hover {
            color: #fff;
            background-color: ", inputs_color_hover, ";
            border-color:  ", inputs_color_border, ";
          }

          /* Results Button */
          .results-button {
            color: #fff;
            background-color: ", results_color, ";
            border-color:  ", results_color_border, ";
          }

          .results-button:focus {
            color: #fff;
            background-color: ", results_color, ";
            border-color:  ", results_color_border, ";
          }

          .results-button:hover {
            color: #fff;
            background-color: ", results_color_hover, ";
            border-color:  ", results_color_border, ";
          }

          /* Export Button */
          .export-button {
            color: #fff;
            background-color: ", export_color, ";
            border-color:  ", export_color_border, ";
          }

          .export-button:focus {
            color: #fff;
            background-color: ", export_color, ";
            border-color:  ", export_color_border, ";
          }

          .export-button:hover {
            color: #fff;
            background-color: ", export_color_hover, ";
            border-color:  ", export_color_border, ";
          }

          "
        )
      )
    )
  )
}
