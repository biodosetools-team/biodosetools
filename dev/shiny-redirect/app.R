library(shiny)
library(markdown)
addResourcePath("www", here::here("dev/shiny-redirect/www"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Favicon
  tags$head(tags$meta(`http-equiv` = "refresh", content = "15;URL=https://biodosetools.reneb.bfs.de/")),
  tags$head(tags$link(rel = "shortcut icon", href = "www/favicon.png")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")),

  # Page title
  title = "Biodose Tools",

  # Contents
  fluidRow(
    column(
      12,
      div(
        class = "container",
        # Logo
        div(
          class = "header",
          img(src = "www/biodosetools_logo.svg", alt = "Biodose Tools Logo"),
          h1("Biodose Tools")
        ),
        # Test
        br(),
        div(
          class = "content",
          p(
            "Biodose Tools is now hosted by RENEB member",
            a(href = "https://www.bfs.de/EN/home/home_node.html", "Bundesamt für Strahlenschutz"),
            "."
          ),
          p(
            "You will be redirected in 10 seconds, if nothing happens, please follow this link",
            a(href = "https://biodosetools.reneb.bfs.de", "https://biodosetools.reneb.bfs.de"),
            "."
          )
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {}

# Run the application
shinyApp(ui = ui, server = server)
