# Fitting Modules ------------------------------------------

fittingUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabItem(tabName = label,
          h2("Dose-effect Fitting"),
          fluidRow(
            # Input and data boxes ----
            column(width = 4,
                   box(width = 12,
                       title = "Inputs",
                       status = "primary", solidHeader = F, collapsible = T,
                       # Inputs
                       textInput(inputId = ns("dose"),
                                 label = "Dose",
                                 value = "0,0.1,0.25,0.5,0.75,1,1.5,2,3,4,5"),
                       textInput(inputId = ns("aberr"),
                                 label = "Aberrations",
                                 value = "8,14,22,55,100,109,100,103,108,103,107"),
                       textInput(inputId = ns("cells"),
                                 label = "Cells",
                                 value = "5000,5002,2008,2002,1832,1168,562,332,193,103,59"),

                       # Tooltips
                       bsTooltip(ns("dose"), "List of doses",
                                 "right", options = list(container = "body")),
                       bsTooltip(ns("aberr"), "Aberrations count",
                                 "right", options = list(container = "body")),
                       bsTooltip(ns("cells"), "Cells count",
                                 "right", options = list(container = "body")),

                       # Button
                       actionButton(ns("button_fit"), "Calculate")
                   ),
                   box(width = 12,
                       title = "Data",
                       status = "primary", solidHeader = F, collapsible = T, collapsed = T,
                       tableOutput(outputId = ns("table"))
                   )
            )
            ,
            # Results tabBox ----
            column(width = 8,
                   tabBox(width = 12,
                          side = "left",
                          # height = "500px",
                          # selected = "Tab3",
                          tabPanel("Result of curve fit", verbatimTextOutput(ns("result"))),
                          tabPanel("Coefficients", verbatimTextOutput(ns("bstat"))),
                          tabPanel("Variance-covariance matrix", verbatimTextOutput(ns("vakoma"))),
                          tabPanel("Correlation matrix", verbatimTextOutput(ns("corma")))
                   )
            )

          )
  )
}

fittingTable <- function(input, output, session, stringsAsFactors) {

  table <- reactive({

    input$button_fit

    isolate({
      dose = as.numeric(unlist(strsplit(input$dose, ",")))
      aberr = as.numeric(unlist(strsplit(input$aberr, ",")))
      cell = as.numeric(unlist(strsplit(input$cells, ",")))
    })

    # output$ttt <- renderTable({
    data.frame(
      Dose = dose,
      Aberrations = aberr,
      Cells = cell
    )
    # })
  })

  # Output ----
  output$table <- renderTable({
    # data.frame(1,2,3)
    table()
  })
}


fittingResults <- function(input, output, session, stringsAsFactors) {

  # Calculations ----
  data <- reactive({

    input$button_fit

    isolate({
      dose = as.numeric(unlist(strsplit(input$dose, ",")))
      aberr = as.numeric(unlist(strsplit(input$aberr, ",")))
      cell = as.numeric(unlist(strsplit(input$cells, ",")))
    })

    x0 <- cell
    x1 <- cell * dose
    x2 <- cell * dose * dose
    model.data <- list(x0, x1, x2, aberr)

    # result <- glm(
    #   aberr ~  -1 + x1 + x2,
    #   family = poisson(link = "identity"),
    #   data = model.data
    # )

    result <- glm(
      aberr ~ -1 + x0 + x1 + x2,
      family = poisson(link = "identity"),
      data = model.data
    )

    smry <- summary(result, correlation = TRUE)
    smry$coefficients
    smry$correlation

    corma <- smry$correlation
    bstat <- smry$coefficients
    seb <- bstat[, 3]
    vakoma <- corma * outer(seb, seb)
    vakoma <- vcov(result)

    res_list <- list(result, bstat, vakoma, corma)

    return(res_list)
  })

  # Outputs ----
  output$result <- renderPrint({
    # "Result of curve fit 'result'"
    data()[[1]]
  })

  output$bstat <- renderPrint({
    # "Coefficients 'bstat'"
    data()[[2]]
  })

  output$vakoma <- renderPrint({
    # "variance-covariance matrix 'vakoma'"
    data()[[3]]
  })

  output$corma <- renderPrint({
    # "Correlation matrix 'corma'"
    data()[[4]]
  })

}


# Advanced Fitting Modules ---------------------------------

fittingAdvUI <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tabItem(tabName = label,
          h2("Dose-effect Fitting"),
          fluidRow(
            # Input box ----
            column(width = 12,
                   box(width = 12,
                       title = "Raw Inputs",
                       status = "primary", solidHeader = F, collapsible = T,
                       numericInput(ns("num.data"), "Dose", value = 1),
                       fluidRow(column(12, rHandsontableOutput(ns("hotable"))))
                   )
                   # box(width = 12,
                   #     title = "Data",
                   #     status = "primary", solidHeader = F, collapsible = T, collapsed = T,
                   #     tableOutput('table')
                   # )
            )#,
            # Main tabBox ----
            # column(width = 8,
            #        tabBox(width = 12,
            #               side = "left",
            #               # height = "500px",
            #               # selected = "Tab3",
            #               tabPanel("Result of curve fit", verbatimTextOutput("result")),
            #               tabPanel("Coefficients", verbatimTextOutput("bstat")),
            #               tabPanel("Variance-covariance matrix", verbatimTextOutput("vakoma")),
            #               tabPanel("Correlation matrix", verbatimTextOutput("corma"))
            #        )
            # )

          )
  )
}


fittingAdvTest <- function(input, output, session, stringsAsFactors) {

  # DF <- data.frame("0" = 1:10, "1" = 1:10, "2" = 1:10, "3" = 1:10, "4" = 1:10, "5" = 1:10, "6" = 1:10,
  #                  N = 1:10, X = 1:10,
  #                  stringsAsFactors = FALSE)
  # num.data <- 10

  previous <- reactive({
    num.data <- as.numeric(input$num.data)

    DF.base <- data.frame(matrix(0, nrow = num.data, ncol = 7))
    DF.calc <- data.frame(N = rep(0, num.data), X = rep(0, input$num.data))

    DF <- cbind(DF.base, DF.calc)
  })

  # numberofrows <- nrow(DF)

  # previous <- reactive({DF})

  MyChanges <- reactive({
    if(is.null(input$hotable1)){return(previous())}
    else if(!identical(previous(),input$hotable)){
      mytable <- as.data.frame(hot_to_r(input$hotable))

      mytable[,8] <- mytable[,1] * mytable[,2]
      mytable
    }
  })
  output$hotable <- renderRHandsontable({rhandsontable(MyChanges())})

  # Output ----
  # output$dose_num_value <- renderPrint({
  #   input$dose_num
  #   })
}
