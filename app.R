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
                   selectInput(inputId = "population", label = "", choices = c("normal", "uniform", "poisson", "binomial", "chi-square", "exponential", "custom"), selected = "normal"), 
                   
                   br(), 
                   
                   conditionalPanel("input.population == 'normal'", 
                                    numericInput(inputId = "mean",      label = "mean",   value = 0), 
                                    numericInput(inputId = "sd",        label = "sd",     value = 1)), 
                   
                   conditionalPanel("input.population == 'uniform'", 
                                    numericInput(inputId = "min",       label = "min",    value = 0), 
                                    numericInput(inputId = "max",       label = "max",    value = 1)), 
                   
                   conditionalPanel("input.population == 'poisson'", 
                                    numericInput(inputId = "lambda",    label = "lambda", value = 1)), 
                   
                   conditionalPanel("input.population == 'binomial'", 
                                    numericInput(inputId = "size",      label = "size",   value = 1), 
                                    numericInput(inputId = "prob",      label = "prob",   value = 0.5)), 
                   
                   conditionalPanel("input.population == 'chi-square'", 
                                    numericInput(inputId = "df",        label = "df",     value = 1)), 
                   
                   conditionalPanel("input.population == 'exponential'", 
                                    numericInput(inputId = "rate",      label = "rate",   value = 1)), 
                   
                   conditionalPanel("input.population == 'custom'", 
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
                   selectInput(inputId = "T_1", label = "\\( T_1 \\)", choices = c("mean", "median", "sd", "var", "iqr", "range", "order", "custom"), selected = "mean"), 
                   
                   conditionalPanel("input.T_1 == 'mean'", 
                                    numericInput(inputId = "T_1.trim", label = "trim", value = 0, min = 0, max = 0.5)), 
                   
                   conditionalPanel("input.T_1 == 'iqr'", 
                                    numericInput(inputId = "T_1.type", label = "type", value = 7, min = 1, max = 9)), 
                   
                   conditionalPanel("input.T_1 == 'order'", 
                                    uiOutput(outputId = "T_1.order")), 
                   
                   conditionalPanel("input.T_1 == 'custom'", 
                                    textInput(inputId = "T_1.custom", label = "f(x)", value = "sum(x) / length(x)")), 
                   
                   br(), 
                   
                   selectInput(inputId = "T_2", label = "\\( T_2 \\)", choices = c("mean", "median", "sd", "var", "iqr", "range", "order", "custom"), selected = "mean"), 
                   
                   conditionalPanel("input.T_2 == 'mean'", 
                                    numericInput(inputId = "T_2.trim", label = "trim", value = 0, min = 0, max = 0.5)), 
                   
                   conditionalPanel("input.T_2 == 'iqr'", 
                                   numericInput(inputId = "T_2.type", label = "type", value = 7, min = 1, max = 9)), 
                   
                   conditionalPanel("input.T_2 == 'order'", 
                                    uiOutput(outputId = "T_2.order")), 
                   
                   conditionalPanel("input.T_2 == 'custom'", 
                                    textInput(inputId = "T_2.custom", label = "f(x)", value = "sum(x) / length(x)"))
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
           "normal" = function(x) dnorm(x, mean = input$mean, sd = input$sd), 
           "uniform" = function(x) dunif(x, min = input$min, max = input$max), 
           "poisson" = function(x) dpois(x, lambda = input$lambda), 
           "binomial" = function(x) dbinom(x, size = input$size, prob = input$prob), 
           "chi-square" = function(x) dchisq(x, df = input$df), 
           "exponential" = function(x) dexp(x, rate = input$rate), 
           "custom" = function(x) eval_tidy(parse_expr(input$dcust))) # TODO: sanatize custom population input
  })
  
  rdist <- reactive({
    switch(input$population, 
           "normal" = function(n) rnorm(n, mean = input$mean, sd = input$sd), 
           "uniform" = function(n) runif(n, min = input$min, max = input$max), 
           "poisson" = function(n) rpois(n, lambda = input$lambda), 
           "binomial" = function(n) rbinom(n, size = input$size, prob = input$prob), 
           "chi-square" = function(n) rchisq(n, df = input$df), 
           "exponential" = function(n) rexp(n, rate = input$rate), 
           "custom" = function(n) rnorm(n)) # TODO: sample from sanitized dcust() input (samplr::projectq3c())
  })
  
  output$T_1.order <- renderUI({
    numericInput(inputId = "T_1.order", label = "order", value = 1, min = 1, max = input$n_1)
  })
  
  output$T_2.order <- renderUI({
    numericInput(inputId = "T_2.order", label = "order", value = 1, min = 1, max = input$n_2)
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
                          "median" = function(x) median(x), 
                          "sd" = function(x) sd(x), 
                          "var" = function(x) var(x), 
                          "iqr" = function(x) IQR(x, type = input$T_1.type), 
                          "range" = function(x) diff(range(x)), 
                          "order" = function(x) sort(x)[input$T_1.order], 
                          "custom" = function(x) eval_tidy(parse_expr(input$T_1.custom))) # TODO: sanitize T1 custom input
    
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
                          "median" = function(x) median(x), 
                          "sd" = function(x) sd(x), 
                          "var" = function(x) var(x), 
                          "iqr" = function(x) IQR(x, type = input$T_2.type), 
                          "range" = function(x) diff(range(x)), 
                          "order" = function(x) sort(x)[input$T_2.order], 
                          "custom" = function(x) eval_tidy(parse_expr(input$T_2.custom))) # TODO: sanitize T2 custom input
    
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
