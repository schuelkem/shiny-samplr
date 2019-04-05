# https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
# withMathJax() label = HTML("$$ \\mu $$")

library(dplyr)
library(ggplot2)
library(rlang)
library(shiny)

ui <- fluidPage(
  withMathJax(), 
  
  titlePanel("Shiny Sampling"), 
  
  sidebarLayout(
    sidebarPanel(
      navlistPanel(
        tabPanel("Population", 
                 fluidPage(
                   selectInput(inputId = "population", label = "", choices = c("Normal", "Uniform", "Poisson", "Binomial", "Chi-Square", "Exponential", "Custom"), selected = "Normal"), 
                   
                   br(), 
                   
                   conditionalPanel("input.population == 'Normal'", 
                                    numericInput(inputId = "mean",      label = "mean",   value = 0), 
                                    numericInput(inputId = "sd",        label = "sd",     value = 1)), 
                   
                   conditionalPanel("input.population == 'Uniform'", 
                                    numericInput(inputId = "min",       label = "min",    value = 0), 
                                    numericInput(inputId = "max",       label = "max",    value = 1)), 
                   
                   conditionalPanel("input.population == 'Poisson'", 
                                    numericInput(inputId = "lambda",    label = "lambda", value = 1)), 
                   
                   conditionalPanel("input.population == 'Binomial'", 
                                    numericInput(inputId = "size",      label = "size",   value = 1), 
                                    numericInput(inputId = "prob",      label = "prob",   value = 0.5)), 
                   
                   conditionalPanel("input.population == 'Chi-Square'", 
                                    numericInput(inputId = "df",        label = "df",     value = 1)), 
                   
                   conditionalPanel("input.population == 'Exponential'", 
                                    numericInput(inputId = "rate",      label = "rate",   value = 1)), 
                   
                   conditionalPanel("input.population == 'Custom'", 
                                    textInput(inputId = "dcust",        label = "f(x)",   value = "0 <= x & x <= 1")), 
                   
                   br(), 
                   
                   fluidRow(
                     column(numericInput(inputId = "xmin", label = "from", value = -4), width = 6),
                     column(numericInput(inputId = "xmax", label = "to",   value =  4), width = 6)
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
  ddist <- reactive({
    switch(input$population, 
           "Normal" = function(x) dnorm(x, mean = input$mean, sd = input$sd), 
           "Uniform" = function(x) dunif(x, min = input$min, max = input$max), 
           "Poisson" = function(x) dpois(x, lambda = input$lambda), 
           "Binomial" = function(x) dbinom(x, size = input$size, prob = input$prob), 
           "Chi-Square" = function(x) dchisq(x, df = input$df), 
           "Exponential" = function(x) dexp(x, rate = input$rate), 
           "Custom" = function(x) eval_tidy(parse_expr(input$dcust))) # TODO: sanatize input
  })
  
  rdist <- reactive({
    switch(input$population, 
           "Normal" = function(n) rnorm(n, mean = input$mean, sd = input$sd), 
           "Uniform" = function(n) runif(n, min = input$min, max = input$max), 
           "Poisson" = function(n) rpois(n, lambda = input$lambda), 
           "Binomial" = function(n) rbinom(n, size = input$size, prob = input$prob), 
           "Chi-Square" = function(n) rchisq(n, df = input$df), 
           "Exponential" = function(n) rexp(n, rate = input$rate), 
           "Custom" = function(n) rnorm(n)) # TODO: sample from sanitized dcust() input (samplr::projectq3c())
  })
  
  output$population_plot <- renderPlot({
    ggplot() + 
      stat_function(aes(x = input$xmin:input$xmax), 
                    n = (input$xmax - input$xmin) * 15, 
                    fun = ddist()) + 
      labs(title = "Population", 
           x = "") + 
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank()) + 
      coord_cartesian(xlim = c(input$xmin, input$xmax))
  })
  
  output$sample_plot <- renderPlot({
    draws <- rdist()(input$n_1)
    
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
    statistic_1 <- switch(input$T_1, 
                          "mean" = function(x) mean(x, trim = input$T_1.trim), 
                          "median" = function(x) median(x))
    
    replicate(input$R_1, {
      draws <- rdist()(input$n_1)
      statistic_1(draws)
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
    statistic_2 <- switch(input$T_2, 
                          "mean" = function(x) mean(x, trim = input$T_2.trim), 
                          "median" = function(x) median(x))
    
    replicate(input$R_2, {
      draws <- rdist()(input$n_2)
      statistic_2(draws)
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
