library(shiny)
library(markdown)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Biodose Tools"),

  # Contents
  fluidRow(
    column(12, includeHTML(here::here("dev/shiny-redirect/www/include.html")))
  )
)

# Define server
server <- function(input, output) {}

# Run the application
shinyApp(ui = ui, server = server)
