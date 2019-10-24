# renderUI: Estimate results tabCard ----
output$estimate_results_ui <- renderUI({
  assessment <- input$assessment_select

  # Whole-body ----
  if (assessment == "whole-body") {
    bs4MyTabCard(
      id = "estimate_results_tabs",
      width = 12,
      noPadding = TRUE,
      side = "left",
      solidHeader = TRUE, closable = FALSE,

      topButton = div(
        # Help button
        bsButton(
          session$ns("help_dose_mixed_yields"),
          label = "",
          icon = icon("question"),
          style = "default", size = "default"
        ),
        # Help modal
        bs4MyModal(
          id = session$ns("help_dose_mixed_yields_dialog"),
          title = "Help: Heterogeneous exposures",
          trigger = session$ns("help_dose_mixed_yields"),
          size = "large",
          withMathJax(includeMarkdown("help/estimate/dose_mixed_yields.md"))
        )
      ),

      bs4MyTabPanel(
        tabName = "Whole-body",
        active = TRUE,
        h6("Whole-body exposure estimation"),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_yields_whole"))
        ),
        br(),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_doses_whole"))
        )#,

        # br(),
        # h6("Relative quality of the estimation"),
        # div(
        #   class = "hot-improved",
        #   rHandsontableOutput(session$ns("AIC_whole"))
        # )
      )
    )
  # Partial-body ----
  } else if (assessment == "partial-body") {
    bs4MyTabCard(
      id = "estimate_results_tabs",
      width = 12,
      noPadding = TRUE,
      side = "left",
      solidHeader = TRUE, closable = FALSE,

      topButton = div(
        # Help button
        bsButton(
          session$ns("help_dose_mixed_yields"),
          label = "",
          icon = icon("question"),
          style = "default", size = "default"
        ),
        # Help modal
        bs4MyModal(
          id = session$ns("help_dose_mixed_yields_dialog"),
          title = "Help: Heterogeneous exposures",
          trigger = session$ns("help_dose_mixed_yields"),
          size = "large",
          withMathJax(includeMarkdown("help/estimate/dose_mixed_yields.md"))
        )
      ),

      bs4MyTabPanel(
        tabName = "Whole-body",
        active = TRUE,
        h6("Whole-body exposure estimation"),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_yields_whole"))
        ),
        br(),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_doses_whole"))
        )#,

        # br(),
        # h6("Relative quality of the estimation"),
        # div(
        #   class = "hot-improved",
        #   rHandsontableOutput(session$ns("AIC_whole"))
        # )
      ),
      bs4MyTabPanel(
        tabName = "Partial-body",
        h6("Partial-body exposure estimation"),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_yields_partial"))
        ),
        br(),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_doses_partial"))
        ),

        br(),
        h6("Initial fraction of irradiated cells"),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_frac_partial"))
        )#,

        # br(),
        # h6("Relative quality of the estimation"),
        # div(
        #   class = "hot-improved",
        #   rHandsontableOutput(session$ns("AIC_partial"))
        # )
      )
    )
  # Heterogeneous ----
  } else if (assessment == "hetero") {
    bs4MyTabCard(
      id = "estimate_results_tabs",
      width = 12,
      noPadding = TRUE,
      side = "left",
      solidHeader = TRUE, closable = FALSE,

      topButton = div(
        # Help button
        bsButton(
          session$ns("help_dose_mixed_yields"),
          label = "",
          icon = icon("question"),
          style = "default", size = "default"
        ),
        # Help modal
        bs4MyModal(
          id = session$ns("help_dose_mixed_yields_dialog"),
          title = "Help: Heterogeneous exposures",
          trigger = session$ns("help_dose_mixed_yields"),
          size = "large",
          withMathJax(includeMarkdown("help/estimate/dose_mixed_yields.md"))
        )
      ),

      bs4MyTabPanel(
        tabName = "Whole-body",
        active = TRUE,
        h6("Whole-body exposure estimation"),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_yields_whole"))
        ),
        br(),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_doses_whole"))
        )#,

        # br(),
        # h6("Relative quality of the estimation"),
        # div(
        #   class = "hot-improved",
        #   rHandsontableOutput(session$ns("AIC_whole"))
        # )
      ),
      bs4MyTabPanel(
        tabName = "Heterogeneous",
        h6("Observed fraction of irradiated cells and its yield"),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_mixing_prop_hetero"))
        ),

        br(),
        h6("Heterogeneous exposure estimation"),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_yields_hetero"))
        ),
        br(),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_doses_hetero"))
        ),

        br(),
        h6("Initial fraction of irradiated cells"),
        div(
          class = "hot-improved",
          rHandsontableOutput(session$ns("est_frac_hetero"))
        )#,

        # br(),
        # h6("Relative quality of the estimation"),
        # div(
        #   class = "hot-improved",
        #   rHandsontableOutput(session$ns("AIC_hetero"))
        # )
      )
    )
  } else {
    return(NULL)
  }
})
