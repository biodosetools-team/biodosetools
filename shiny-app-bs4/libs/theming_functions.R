# Function: bs4DashSidebarColor ----------------------------------------
#' Creates a custom theme object for a shinydashboard application
#'
#'
#' @return HTML code. Theme for a shinydashboard application.

bs4DashSidebarColor <- function(back_color) {

  htmltools::tags$head(

    htmltools::tags$style(

      htmltools::HTML(

        paste0(

          '
          /* Sidebar Main Color */

          .alert-biodose-tools,
          .bg-biodose-tools,
          .label-biodose-tools {
            background-color:', back_color ,'!important
          }

          a.alert-biodose-tools:focus,
          a.alert-biodose-tools:hover,
          a.bg-biodose-tools:focus,
          a.bg-biodose-tools:hover,
          a.label-biodose-tools:focus,
          a.label-biodose-tools:hover,
          button.alert-biodose-tools:focus,
          button.alert-biodose-tools:hover,
          button.bg-biodose-tools:focus,
          button.bg-biodose-tools:hover,
          button.label-biodose-tools:focus,
          button.label-biodose-tools:hover {
            background-color:', back_color ,'!important
          }

          '

        )

      )

    )

  )

}


# Function: bs4DashCardsStatus ----------------------------------------
#' Creates a custom theme object for a shinydashboard application
#'
#'
#' @return HTML code. Theme for a shinydashboard application.

bs4DashCardsStatus <- function(options_color, inputs_color, results_color, export_color) {

  htmltools::tags$head(

    htmltools::tags$style(

      htmltools::HTML(

        paste0(

          '
          /* Options card colors */
          .card-options:not(.card-outline) .card-header {
          	background-color:', options_color, ';
          	border-bottom: 0
          }

          .card-options:not(.card-outline) .card-header,
          .card-options:not(.card-outline) .card-header a {
          	color: #fff
          }

          .card-options.card-outline {
          	border-top: 5px solid', options_color, ';
          }

          /* Inputs card colors */
          .card-inputs:not(.card-outline) .card-header {
          	background-color:', inputs_color, ';
          	border-bottom: 0
          }

          .card-inputs:not(.card-outline) .card-header,
          .card-inputs:not(.card-outline) .card-header a {
          	color: #fff
          }

          .card-inputs.card-outline {
          	border-top: 5px solid', inputs_color, ';
          }

          /* Results card colors */
          .card-results:not(.card-outline) .card-header {
          	background-color:', results_color, ';
          	border-bottom: 0
          }

          .card-results:not(.card-outline) .card-header,
          .card-results:not(.card-outline) .card-header a {
          	color: #fff
          }

          .card-results.card-outline {
          	border-top: 5px solid', results_color, ';
          }

          /* Export card colors */
          .card-export:not(.card-outline) .card-header {
          	background-color:', export_color, ';
          	border-bottom: 0
          }

          .card-export:not(.card-outline) .card-header,
          .card-export:not(.card-outline) .card-header a {
          	color: #fff
          }

          .card-export.card-outline {
          	border-top: 5px solid', export_color, ';
          }

          /* TaCards colors */

          .nav-pills .nav-link.active,
          .nav-pills .show>.nav-link {
              color: #fff;
              background-color:', results_color, ';
          }

          .nav-pills .nav-link:not(.active):hover {
              color:', results_color, ';
          }

          '

        )

      )

    )

  )

}



# Function: bs4DashButtonsStatus ----------------------------------------
#' Creates a custom theme object for a shinydashboard application
#'
#'
#' @return HTML code. Theme for a shinydashboard application.

bs4DashButtonsStatus <- function(home_color, home_color_hover, home_color_border,
                                        options_color, options_color_hover, options_color_border,
                                        inputs_color, inputs_color_hover, inputs_color_border,
                                        results_color, results_color_hover, results_color_border,
                                        export_color, export_color_hover, export_color_border) {

  htmltools::tags$head(

    htmltools::tags$style(

      htmltools::HTML(

        paste0(

          '
          /* Help buttons */
          .rightAlign{
            float:right;
          }

          /* Widgets */
          .side-widget {
            display: inline-block;
            vertical-align: top;
          }

          .side-widget-tall {
            display: inline-block;
            vertical-align: bottom;
            margin-bottom: -20px;
          }

          .widget-sep {
            display: inline-block;
            vertical-align: top;
            width: 20px;
          }

          /* File selector widget */

          .skin-blue .input-group-btn > .btn {
            color: #000;
            border-radius: 3px 0 0 3px;
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
          }

          /* Home Button */

          .home-button {
            color: #fff;
            background-color: ', home_color, ';
            border-color:  ', home_color_border, ';
          }

          .home-button:focus {
            color: #fff;
            background-color: ', home_color, ';
            border-color:  ', home_color_border, ';
          }

          .home-button:hover {
            color: #fff;
            background-color: ', home_color_hover, ';
            border-color:  ', home_color_border, ';
          }

          /* Options Button */
          .options-button {
            color: #fff;
            background-color: ', options_color, ';
            border-color:  ', options_color_border, ';
          }

          .options-button:focus {
            color: #fff;
            background-color: ', options_color, ';
            border-color:  ', options_color_border, ';
          }

          .options-button:hover {
            color: #fff;
            background-color: ', options_color_hover, ';
            border-color:  ', options_color_border, ';
          }

          /* Inputs Button */
          .inputs-button {
            color: #fff;
            background-color: ', inputs_color, ';
            border-color:  ', inputs_color_border, ';
          }

          .inputs-button:focus {
            color: #fff;
            background-color: ', inputs_color, ';
            border-color:  ', inputs_color_border, ';
          }

          .inputs-button:hover {
            color: #fff;
            background-color: ', inputs_color_hover, ';
            border-color:  ', inputs_color_border, ';
          }

          /* Results Button */
          .results-button {
            color: #fff;
            background-color: ', results_color, ';
            border-color:  ', results_color_border, ';
          }

          .results-button:focus {
            color: #fff;
            background-color: ', results_color, ';
            border-color:  ', results_color_border, ';
          }

          .results-button:hover {
            color: #fff;
            background-color: ', results_color_hover, ';
            border-color:  ', results_color_border, ';
          }

          /* Export Button */
          .export-button {
            color: #fff;
            background-color: ', export_color, ';
            border-color:  ', export_color_border, ';
          }

          .export-button:focus {
            color: #fff;
            background-color: ', export_color, ';
            border-color:  ', export_color_border, ';
          }

          .export-button:hover {
            color: #fff;
            background-color: ', export_color_hover, ';
            border-color:  ', export_color_border, ';
          }

          /* Custom shinyBS status colors */

          /* Options Checkbox */
          .checkbox-bs-warning input[type="checkbox"]:checked + label::before, .checkbox-bs-warning input[type="radio"]:checked + label::before {
            color: #fff;
            background-color: ', options_color, ';
            border-color:  ', options_color_border, ';
          }

          .checkbox-bs-options input[type="checkbox"]:checked + label::before, .checkbox-bs-options input[type="radio"]:checked + label::before {
            color: #fff;
            background-color: ', options_color, ';
            border-color:  ', options_color_border, ';
          }

          /* Inputs Checkbox */
          .checkbox-bs-primary input[type="checkbox"]:checked + label::before, .checkbox-bs-primary input[type="radio"]:checked + label::before {
            color: #fff;
            background-color: ', inputs_color, ';
            border-color:  ', inputs_color_border, ';
          }

          /* Results Checkbox */
          .checkbox-bs-success input[type="checkbox"]:checked + label::before, .checkbox-bs-success input[type="radio"]:checked + label::before {
            color: #fff;
            background-color: ', results_color, ';
            border-color:  ', results_color_border, ';
          }

          /* Export Checkbox */
          .checkbox-bs-danger input[type="checkbox"]:checked + label::before, .checkbox-bs-danger input[type="radio"]:checked + label::before {
            color: #fff;
            background-color: ', export_color, ';
            border-color:  ', export_color_border, ';
          }

          '

        )

      )

    )

  )

}
