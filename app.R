# # https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny

library(tidyverse)
library(shiny)

ui <- fluidPage(
  withMathJax(), 
  
  titlePanel("Shiny Sampling"), 
  
  sidebarLayout(
    sidebarPanel(
      navlistPanel(
        tabPanel("Population", 
                 fluidPage(
                   selectInput(inputId = "population", label = "", choices = c("Normal", "Uniform"), selected = "Normal"), 
                   
                   br(), 
                   
                   conditionalPanel("input.population == 'Normal'", 
                                    numericInput(inputId = "mean", label = "mean", value = 0), 
                                    numericInput(inputId = "sd", label = "sd", value = 1)), 
                   
                   conditionalPanel("input.population == 'Uniform'", 
                                    numericInput(inputId = "min", label = "min", value = 0), 
                                    numericInput(inputId = "max", label = "max", value = 1)), 
                   
                   br(), 
                   
                   fluidRow(
                     column(numericInput(inputId = "xmin", label = "from", value = -4), width = 6),
                     column(numericInput(inputId = "xmax", label = "to", value = 4), width = 6)
                   )
                 )
        ), 
        
        tabPanel("Sample", 
                 fluidPage(
                   numericInput(inputId = "n_1", label = '\\( n_1 \\)', value = 15, min = 1), 
                   br(), 
                   numericInput(inputId = "n_2", label = '\\( n_2 \\)', value = 15, min = 1)
                 )
        ), 
        
        tabPanel("Statistic", 
                 fluidPage(
                   selectInput(inputId = "T_1", label = "\\( T_1 \\)", choices = c("mean", "median"), selected = "mean"), 
                   conditionalPanel("input.T_1 == 'mean'", 
                                    numericInput(inputId = "T_1.trim", label = "trim", value = 0, min = 0, max = 0.5)), 
                   
                   br(), 
                   
                   selectInput(inputId = "T_2", label = "\\( T_2 \\)", choices = c("mean", "median"), selected = "mean"), 
                   conditionalPanel("input.T_2 == 'mean'", 
                                    numericInput(inputId = "T_2.trim", label = "trim", value = 0, min = 0, max = 0.5))
                 )
        ), 
        
        tabPanel("Resamples", 
                 fluidPage(
                   numericInput(inputId = "R_1", label = "\\( R_1 \\)", value = 10000, min = 1, max = 10000), 
                   
                   br(), 
                   
                   numericInput(inputId = "R_2", label = "\\( R_2 \\)", value = 10000, min = 1, max = 10000)
                 )
        )
      )
    ), 
    
    mainPanel(
      plotOutput(outputId = "population_plot",  height = 200), 
      plotOutput(outputId = "sample_plot",      height = 200), 
      plotOutput(outputId = "bootstrap_plot_1", height = 200), 
      plotOutput(outputId = "bootstrap_plot_2", height = 200)
    )
  )
)

server <- function(input, output, session) {
  
  output$population_plot <- renderPlot({
    if(input$population == "Normal") { 
      ddist <- function(x) dnorm(x, mean = input$mean, sd = input$sd)
    } else if(input$population == "Uniform") { 
      ddist <- function(x) dunif(x, min = input$min, max = input$max)
    }
    
    ggplot() + 
      stat_function(aes(x = input$xmin:input$xmax), 
                    n = (input$xmax - input$xmin) * 15, 
                    fun = ddist) + 
      labs(title = "Population", 
           x = "") + 
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank()) + 
      coord_cartesian(xlim = c(input$xmin, input$xmax))
  })
  
  output$sample_plot <- renderPlot({
    if(input$population == "Normal") {
      rdist <- function(n) rnorm(n, mean = input$mean, sd = input$sd)
    } else if(input$population == "Uniform") {
      rdist <- function(n) runif(n, min = input$min, max = input$max)
    }
    
    draws <- rdist(input$n_1)
    
    ggplot(tibble(draws), aes(x = draws)) + 
      geom_histogram() + 
      labs(title = "Sample", 
           x = "") + 
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank()) + 
      coord_cartesian(xlim = c(input$xmin, input$xmax))
  })
  
  output$bootstrap_plot_1 <- renderPlot({
    if(input$population == "Normal") {
      rdist <- function(n) rnorm(n, mean = input$mean, sd = input$sd)
    } else if(input$population == "Uniform") {
      rdist <- function(n) runif(n, min = input$min, max = input$max)
    }
    
    if(input$T_1 == "mean") {
      s <- function(x) mean(x, trim = input$T_1.trim)
    } else if(input$T_1 == "median") {
      s <- function(x) median(x)
    }
    
    replicate(input$R_1, {
      draws <- rdist(input$n_1)
      s(draws)
    }) -> simdat
    
    ggplot(tibble(simdat), aes(x = simdat)) + 
      geom_histogram() + 
      labs(title = "Bootstrap Distribution 1", 
           x = "") + 
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank()) + 
      coord_cartesian(xlim = c(input$xmin, input$xmax))
  })
  
  output$bootstrap_plot_2 <- renderPlot({
    if(input$population == "Normal") {
      rdist <- function(n) rnorm(n, mean = input$mean, sd = input$sd)
    } else if(input$population == "Uniform") {
      rdist <- function(n) runif(n, min = input$min, max = input$max)
    }
    
    if(input$T_2 == "mean") {
      s <- function(x) mean(x, trim = input$T_1.trim)
    } else if(input$T_2 == "median") {
      s <- function(x) median(x)
    }
    
    replicate(input$R_2, {
      draws <- rdist(input$n_2)
      s(draws)
    }) -> simdat
    
    ggplot(tibble(simdat), aes(x = simdat)) + 
      geom_histogram() + 
      labs(title = "Boostrap Distribution 2", 
           x = "") + 
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank()) + 
      coord_cartesian(xlim = c(input$xmin, input$xmax))
  })
}

shinyApp(ui, server)
