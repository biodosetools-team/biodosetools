# Libraries ------------------------------------------------

# Server Libs
library(rhandsontable)
library(shinyjs)
library(dplyr)
library(ggplot2)

# Server ---------------------------------------------------

server <- function(input, output) {


  # Fitting ----
  callModule(module = fittingTable, id = "fitting", reactive(input$button_fit))
  callModule(module = fittingResults, id = "fitting", reactive(input$button_fit))

  # Advanced Fitting ----
  callModule(module = fittingAdvHotTable, id = "adv_fitting")
  callModule(module = fittingAdvResults, id = "adv_fitting")

  # Estimate ----
  callModule(module = estimateHotTable, id = "estimate")
  callModule(module = estimateFittingCurve, id = "estimate")
  callModule(module = estimateResults, id = "estimate")



  output$plot <- renderPlot({
    hist(rnorm(input$obs))
  })

  output$distPlot <- renderPlot({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

    hist(dist(500))
  })

  output$plot2 <- renderPlotly({
    p <- plot_ly(df, x = ~x) %>%
      add_lines(y = ~y1, name = "A") %>%
      add_lines(y = ~y2, name = "B", visible = F) %>%
      layout(
        xaxis = list(domain = c(0.1, 1)),
        yaxis = list(title = "y"),
        updatemenus = list(
          list(
            y = 0.8,
            buttons = list(

              list(method = "restyle",
                   args = list("line.color", "blue"),
                   label = "Blue"),

              list(method = "restyle",
                   args = list("line.color", "red"),
                   label = "Red"))),

          list(
            y = 0.7,
            buttons = list(
              list(method = "restyle",
                   args = list("visible", list(TRUE, FALSE)),
                   label = "Sin"),

              list(method = "restyle",
                   args = list("visible", list(FALSE, TRUE)),
                   label = "Cos")))
        )
      )
  })

  output$plot3 <- renderPlotly({
    s <- subplot(
      plot_ly(x = x, type = "histogram"),
      plotly_empty(),
      plot_ly(x = x, y = y, type = "histogram2dcontour"),
      plot_ly(y = y, type = "histogram"),
      nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
      shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
    )
    p <- layout(s, showlegend = FALSE)
  })

}
