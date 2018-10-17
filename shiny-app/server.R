# Libraries ------------------------------------------------

# Server Libs
# library(dplyr)
# library(xts)
library(shinyjs)
source("libs/dashboardthemes.R")

# Server ---------------------------------------------------

server <- function(input, output) {


  output$table <- renderTable({
    dose <- as.numeric(unlist(strsplit(input$dose,",")))
    ab <- as.numeric(unlist(strsplit(input$cells,",")))
    cells <- as.numeric(unlist(strsplit(input$ab,",")))

    data.frame(dose,ab,cells)
  })

  output$workbench<-renderPrint({

    dose <- as.numeric(unlist(strsplit(input$dose,",")))
    ab <- as.numeric(unlist(strsplit(input$cells,",")))
    cells <- as.numeric(unlist(strsplit(input$ab,",")))

    # kurvendaten<-data.frame(dose,ab,cells)
    # print(kurvendaten)

    x0<-cells
    x1<-cells*dose
    x2<-cells*dose*dose
    modelldaten<-list(x0,x1,x2,ab)
    # print(modelldaten)

    #result<-glm(ab ~  -1 + x1+x2,family=poisson(link = "identity"), data=modelldaten)
    # result <- glm(ab ~ -1 + x0+x1+x2, family = poisson(link = "identity"), data= modelldaten)
    result <- lm(ab ~ -1 + x0+x1+x2)
    smry<-summary(result,correlation=TRUE)
    smry$coefficients
    smry$correlation

    corma<-smry$correlation
    bstat<-smry$coefficients
    seb<-bstat[,3]
    vakoma<-corma*outer(seb,seb)
    vakoma<-vcov(result)

    cat("\n")
    print("Result of curve fit 'result'")
    # print("-------------------")
    print(result)

    cat("\n")
    print("Coefficients 'bstat'")
    print(bstat)

    cat("\n")
    print("variance-covariance matrix 'vakoma'")
    print(vakoma)

    cat("\n")
    print("correlation matrix 'corma'")
    print(corma)
  }
  )



  # Legacy Code ####

  source("libs/grind.R") # For phase portrait
  source("libs/cube.R")  # For 3D trajectories



  # Dynamic plot list
  # observeEvent(input$run_sims, {
  output$ui_plots <- renderUI({
    out <- list()
    if (length(input$`plots_checkbox`)==0){return(NULL)}

    for (i in input$`plots_checkbox`){
      # The name of the repl_* renderPlot()s must match the checkboxGroupInput()s
      out[[i]] <- plotOutput(outputId = paste0(input$sidebarmenu, "_", i))
    }
    return(out)
    # })
  })

  # TS Replication ####
  output$replication_time_series <- renderPlot({
    model <- function(t, state, parms) {
      with(as.list(c(state,parms)), {
        dR1 <- kappa * RNA1 * replicase * (1 - RNA1) - gamma * RNA1
        dp1 <-  alpha * RNA1 * (1 - replicase) - sigma * replicase
        return(list(c(dR1, dp1)))
      })
    }

    p <- c(kappa = input$slider_kappa,
           alpha = input$slider_alpha,
           gamma = input$slider_gamma,
           sigma = input$slider_sigma)


    s <- c(RNA1 = input$slider_r1,
           replicase = 0)

    run(tstep = 0.5, state = s, odes = model, parms = p)
  })

  # PP Replication ####
  output$replication_phase_port <- renderPlot({
    model <- function(t, state, parms) {
      with(as.list(c(state,parms)), {
        dR1 <- kappa * RNA1 * replicase * (1 - RNA1) - gamma * RNA1
        dp1 <-  alpha * RNA1 * (1 - replicase) - sigma * replicase
        return(list(c(dR1, dp1)))
      })
    }

    p <- c(kappa = input$slider_kappa,
           alpha = input$slider_alpha,
           gamma = input$slider_gamma,
           sigma = input$slider_sigma)


    s <- c(RNA1 = input$slider_r1,
           replicase = 0)

    plane(x = "RNA1", y = "replicase", tstep = 0.5, state = s, odes = model, parms = p, portrait = T)
  })

  # TS Bipartite ####
  output$bipartite_time_series <- renderPlot({
    model <- function(t, state, parms) {
      with(as.list(c(state,parms)), {
        dR1 <- kappa1 * RNA1 * replicase * (1 - RNA1- RNA2) - gamma1 * RNA1 - epsilon1 * RNA1 * coat
        dR2 <- kappa2 * RNA2 * replicase * (1 - RNA1- RNA2) - gamma2 * RNA2 - epsilon2 * RNA2 * coat
        dp1 <-  alpha * RNA1 * (1 - replicase - coat) - sigma1 * replicase
        ds1 <-  beta * RNA2 * (1 - replicase - coat) - sigma2 * coat
        dv1 <- epsilon1 * RNA1 * coat - delta1 * virion1
        dv2 <- epsilon2 * RNA2 * coat - delta2 * virion2
        return(list(c(dR1, dR2, dp1, ds1, dv1, dv2)))
      })
    }

    p <- c(
      kappa1   = input$slider_kappa_bi,   # default: 1
      kappa2   = input$slider_kappa_bi,
      alpha    = input$slider_alpha_bi,   # default: 1
      beta     = input$slider_alpha_bi,
      gamma1   = input$slider_gamma_bi,   # default: 0.1
      gamma2   = input$slider_gamma_bi,
      sigma1   = input$slider_sigma_bi,   # default: 0.1
      sigma2   = input$slider_sigma_bi,
      epsilon1 = input$slider_epsilon_bi, # default: 0.1
      epsilon2 = input$slider_epsilon_bi,
      delta1   = input$slider_delta_bi,   # default: 0.1
      delta2   = input$slider_delta_bi
    )

    s <- c(
      RNA1 = input$slider_r1_bi,
      RNA2 = input$slider_r2_bi,
      replicase = 0,
      coat = 0,
      virion1 = 0,
      virion2 = 0
    )

    run(tstep = 0.5, state = s, odes = model, parms = p)
  })

  # Traj Bipartite ####
  output$bipartite_3d_traj <- renderPlot({
    model <- function(t, state, parms) {
      with(as.list(c(state,parms)), {
        dR1 <- kappa1 * RNA1 * replicase * (1 - RNA1- RNA2) - gamma1 * RNA1 - epsilon1 * RNA1 * coat
        dR2 <- kappa2 * RNA2 * replicase * (1 - RNA1- RNA2) - gamma2 * RNA2 - epsilon2 * RNA2 * coat
        dp1 <-  alpha * RNA1 * (1 - replicase - coat) - sigma1 * replicase
        ds1 <-  beta * RNA2 * (1 - replicase - coat) - sigma2 * coat
        dv1 <- epsilon1 * RNA1 * coat - delta1 * virion1
        dv2 <- epsilon2 * RNA2 * coat - delta2 * virion2
        return(list(c(dR1, dR2, dp1, ds1, dv1, dv2)))
      })
    }

    p <- c(
      kappa1   = input$slider_kappa_bi,   # default: 1
      kappa2   = input$slider_kappa_bi,
      alpha    = input$slider_alpha_bi,   # default: 1
      beta     = input$slider_alpha_bi,
      gamma1   = input$slider_gamma_bi,   # default: 0.1
      gamma2   = input$slider_gamma_bi,
      sigma1   = input$slider_sigma_bi,   # default: 0.1
      sigma2   = input$slider_sigma_bi,
      epsilon1 = input$slider_epsilon_bi, # default: 0.1
      epsilon2 = input$slider_epsilon_bi,
      delta1   = input$slider_delta_bi,   # default: 0.1
      delta2   = input$slider_delta_bi
    )

    s <- c(
      RNA1 = input$slider_r1_bi,
      RNA2 = input$slider_r2_bi,
      replicase = 0,
      coat = 0,
      virion1 = 0,
      virion2 = 0
    )

    cube(x = 1, y = 2, z = 3, state = s)
  })

  # PP Bipartite ####
  output$bipartite_phase_port <- renderPlot({
    model <- function(t, state, parms) {
      with(as.list(c(state,parms)), {
        dR1 <- kappa1 * RNA1 * replicase * (1 - RNA1- RNA2) - gamma1 * RNA1 - epsilon1 * RNA1 * coat
        dR2 <- kappa2 * RNA2 * replicase * (1 - RNA1- RNA2) - gamma2 * RNA2 - epsilon2 * RNA2 * coat
        dp1 <-  alpha * RNA1 * (1 - replicase - coat) - sigma1 * replicase
        ds1 <-  beta * RNA2 * (1 - replicase - coat) - sigma2 * coat
        dv1 <- epsilon1 * RNA1 * coat - delta1 * virion1
        dv2 <- epsilon2 * RNA2 * coat - delta2 * virion2
        return(list(c(dR1, dR2, dp1, ds1, dv1, dv2)))
      })
    }

    p <- c(
      kappa1   = input$slider_kappa_bi,   # default: 1
      kappa2   = input$slider_kappa_bi,
      alpha    = input$slider_alpha_bi,   # default: 1
      beta     = input$slider_alpha_bi,
      gamma1   = input$slider_gamma_bi,   # default: 0.1
      gamma2   = input$slider_gamma_bi,
      sigma1   = input$slider_sigma_bi,   # default: 0.1
      sigma2   = input$slider_sigma_bi,
      epsilon1 = input$slider_epsilon_bi, # default: 0.1
      epsilon2 = input$slider_epsilon_bi,
      delta1   = input$slider_delta_bi,   # default: 0.1
      delta2   = input$slider_delta_bi
    )

    s <- c(
      RNA1 = input$slider_r1_bi,
      RNA2 = input$slider_r2_bi,
      replicase = 0,
      coat = 0,
      virion1 = 0,
      virion2 = 0
    )

    plane(x = "RNA1", y = "RNA2", tstep = 0.5, state = s, odes = model, parms = p, portrait = T)
  })

  # Debugger
  output$debugger <- renderText({
    paste(input$plots_checkbox)
  })

}
