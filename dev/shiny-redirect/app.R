library(shiny)
library(markdown)
addResourcePath("www", here::here("dev/shiny-redirect/www"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Favicon
  tags$head(tags$link(rel = "shortcut icon", href = "www/favicon.png")),
  tags$link(rel = "stylesheet", type="text/css", href="www/style.css"),

  # Page title
  title = "Biodose Tools",

  # Application title
  # titlePanel("Biodose Tools"),

  # Contents
  fluidRow(
    column(12, includeHTML(here::here("dev/shiny-redirect/www/include.html")))
  )
)

# Define server
server <- function(input, output) {}

# Run the application
shinyApp(ui = ui, server = server)
