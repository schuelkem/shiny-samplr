# https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
# withMathJax() label = HTML("$$ \\mu $$")

##### IMPORTS #####

library(dplyr)
library(ggplot2)
library(rlang)
library(shiny)



##### FUNCTIONS #####

skew <- function(x) {
  n <- length(x)
  x <- scale(x, scale = FALSE)
  sqrt(n) * sum(x^3) / (sum(x^2)^(3/2))
}

kurtosis <- function(x) {
  n <- length(x)
  x <- scale(x, scale = FALSE)
  r <- n * sum(x^4) / (sum(x^2)^2) - 3
}

descriptives <- function(x) {
  c(mean(x), median(x), sd(x), var(x), skew(x), kurtosis(x))
}

theme_common <- function() {
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
}



##### COMMON OBJECTS #####

population_choices <- c("normal", "uniform", "poisson", "binomial", "chi-square", "exponential", "custom")
statistic_choices <- c("mean", "median", "sd", "var", "var*", "iqr", "range", "order", "t", "mad", "custom")
descriptive_labels <- c("mean", "median", "sd", "var", "skew", "kurtosis")






ui <- fluidPage(
  withMathJax(), 
  
  titlePanel("shiny-samplr"), 
  
  sidebarLayout(
    sidebarPanel(
      navlistPanel(
        tabPanel("Population", 
                 fluidPage(
                   selectInput(inputId = "population", label = "", choices = population_choices, selected = "normal"), 
                   
                   br(), 
                   
                   conditionalPanel("input.population == 'normal'", 
                                    numericInput(inputId = "mean", label = "mean", value = 0), 
                                    numericInput(inputId = "sd", label = "sd", value = 1)), 
                   
                   conditionalPanel("input.population == 'uniform'", 
                                    numericInput(inputId = "min", label = "min", value = 0), 
                                    numericInput(inputId = "max", label = "max", value = 1)), 
                   
                   conditionalPanel("input.population == 'poisson'", 
                                    numericInput(inputId = "lambda", label = "lambda", value = 1)), 
                   
                   conditionalPanel("input.population == 'binomial'", 
                                    numericInput(inputId = "size", label = "size", value = 1), 
                                    numericInput(inputId = "prob", label = "prob", value = 0.5)), 
                   
                   conditionalPanel("input.population == 'chi-square'", 
                                    numericInput(inputId = "df", label = "df", value = 1)), 
                   
                   conditionalPanel("input.population == 'exponential'", 
                                    numericInput(inputId = "rate", label = "rate", value = 1)), 
                   
                   conditionalPanel("input.population == 'custom' & window.location.hostname == '127.0.0.1'", 
                                    textInput(inputId = "dcust", label = "f(x)", value = "0 <= x & x <= 1")), 
                   
                   br(), 
                   
                   fluidRow(
                     column(numericInput(inputId = "xmin", label = "from", value = -4), width = 6),
                     column(numericInput(inputId = "xmax", label = "to", value =  4), width = 6)
                   )
                 )
        ), 
        
        tabPanel("Sample", 
                 fluidPage(
                   numericInput(inputId = "n_1", label = HTML("$$ n_1 $$"), value = 15, min = 1), 
                   br(), 
                   numericInput(inputId = "n_2", label = HTML("$$ n_2 $$"), value = 15, min = 1)
                 )
        ), 
        
        tabPanel("Statistic", 
                 fluidPage(
                   selectInput(inputId = "T_1", label = HTML("$$ T_1 $$"), choices = statistic_choices, selected = "mean"), 
                   
                   conditionalPanel("input.T_1 == 'mean'", 
                                    numericInput(inputId = "T_1.trim", label = "trim", value = 0, min = 0, max = 0.5)), 
                   
                   conditionalPanel("input.T_1 == 'iqr'", 
                                    numericInput(inputId = "T_1.type", label = "type", value = 7, min = 1, max = 9, step = 1)), 
                   
                   conditionalPanel("input.T_1 == 'order'", 
                                    numericInput(inputId = "T_1.order", label = "order", value = 1, min = 1, max = 15, step = 1)), 
                   
                   conditionalPanel("input.T_1 == 'custom'", 
                                    textInput(inputId = "T_1.custom", label = "f(x)", value = "sum(x) / length(x)")), 
                   
                   br(), 
                   
                   selectInput(inputId = "T_2", label = HTML("$$ T_2 $$"), choices = statistic_choices, selected = "mean"), 
                   
                   conditionalPanel("input.T_2 == 'mean'", 
                                    numericInput(inputId = "T_2.trim", label = "trim", value = 0, min = 0, max = 0.5)), 
                   
                   conditionalPanel("input.T_2 == 'iqr'", 
                                   numericInput(inputId = "T_2.type", label = "type", value = 7, min = 1, max = 9, step = 1)), 
                   
                   conditionalPanel("input.T_2 == 'order'", 
                                    numericInput(inputId = "T_2.order", label = "order", value = 1, min = 1, max = 15, step = 1)), 
                   
                   conditionalPanel("input.T_2 == 'custom'", 
                                    textInput(inputId = "T_2.custom", label = "f(x)", value = "sum(x) / length(x)"))
                 )
        ), 
        
        tabPanel("Resamples", 
                 fluidPage(
                   numericInput(inputId = "R_1", label = HTML("$$ R_1 $$"), value = 10000, min = 1, max = 10000), 
                   
                   br(), 
                   
                   numericInput(inputId = "R_2", label = HTML("$$ R_2 $$"), value = 10000, min = 1, max = 10000)
                 )
        )
      )
    ), 
    
    mainPanel(
      fluidPage(
        fluidRow(
          column(plotOutput(outputId = "population_plot", height = 200), width = 10), 
          column(
            br(), 
            verbatimTextOutput(outputId = "population_descriptives"), width = 2
          )
        ), 
        
        fluidRow(
          column(plotOutput(outputId = "sample_plot", height = 200), width = 10), 
          br(), 
          column(verbatimTextOutput(outputId = "sample_descriptives"), width = 2)
        ), 
        
        fluidRow(
          column(plotOutput(outputId = "bootstrap_1_plot", height = 200), width = 10), 
          br(), 
          column(verbatimTextOutput(outputId = "bootstrap_1_descriptives"), width = 2)
        ), 
        
        fluidRow(
          column(plotOutput(outputId = "bootstrap_2_plot", height = 200), width = 10), 
          br(), 
          column(verbatimTextOutput(outputId = "bootstrap_2_descriptives"), width = 2)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ##### UPDATE INPUTS #####
  
  # if not on localhost remove "custom" option for population and both statistics
  observe({
    if(session$clientData$url_hostname != "127.0.0.1") {
      updateSelectInput(session, "population", "", population_choices[-length(population_choices)], "normal")
      updateSelectInput(session, "T_1", "", statistic_choices[-length(statistic_choices)], "mean")
      updateSelectInput(session, "T_2", "", statistic_choices[-length(statistic_choices)], "mean")
    }
  })
  
  # update max order stats based on sample sizes
  ## retain old T1 order if still valid; otherwise reset to min
  observeEvent(input$n_1, {
    old_value <- input$T_1.order
    new_max <- input$n_1
    new_value <- ifelse(old_value < new_max, old_value, 1)
    updateNumericInput(session, "T_1.order", "order", value = new_value, min = 1, max = new_max, step = 1) 
  })
  ## retain old T2 order if still valid; otherwise reset to min
  observeEvent(input$n_2, {
    old_value <- input$T_2.order
    new_max <- input$n_2
    new_value <- ifelse(old_value < new_max, old_value, 1)
    updateNumericInput(session, "T_2.order", "order", value = new_value, min = 1, max = new_max, step = 1)
  })
  
  
  
  ##### DISTRIBUTION FUNCTIONS #####
  
  ddist <- reactive({
    switch(input$population, 
           "normal" =      function(x) dnorm(x, mean = input$mean, sd = input$sd), 
           "uniform" =     function(x) dunif(x, min = input$min, max = input$max), 
           "poisson" =     function(x) dpois(x, lambda = input$lambda), 
           "binomial" =    function(x) dbinom(x, size = input$size, prob = input$prob), 
           "chi-square" =  function(x) dchisq(x, df = input$df), 
           "exponential" = function(x) dexp(x, rate = input$rate), 
           "custom" =      function(x) eval_tidy(parse_expr(input$dcust)))
  })
  
  rdist <- reactive({
    switch(input$population, 
           "normal" =      function(n) rnorm(n, mean = input$mean, sd = input$sd), 
           "uniform" =     function(n) runif(n, min = input$min, max = input$max), 
           "poisson" =     function(n) rpois(n, lambda = input$lambda), 
           "binomial" =    function(n) rbinom(n, size = input$size, prob = input$prob), 
           "chi-square" =  function(n) rchisq(n, df = input$df), 
           "exponential" = function(n) rexp(n, rate = input$rate), 
           "custom" =      function(n) rnorm(n)) # TODO: sample from dcust() input (samplr::projectq3c())
  })
  
  
  
  ##### POPULATION #####
  
  output$population_plot <- renderPlot({
    ggplot() + 
      switch(input$population, 
             "poisson" = geom_step(aes(x = x, y = y), 
                                   tibble(x = seq(input$xmin - 1, input$xmax + 1, 1) - 0.5, 
                                          y = dpois(x + 0.5, lambda = input$lambda)), 
                                   color = "#337ab7"), 
             "binomial" = geom_step(aes(x = x, y = y), 
                                    tibble(x = seq(input$xmin - 1, input$xmax + 1, 1) - 0.5, 
                                           y = dbinom(x + 0.5, size = input$size, input$prob)), 
                                    color = "#337ab7"), 
             stat_function(aes(x = input$xmin:input$xmax), 
                           n = (input$xmax - input$xmin) * 15, 
                           fun = ddist(), 
                           color = "#337ab7")
      ) + 
      labs(title = "Population", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$xmin, input$xmax))
  })
  
  # calculate population descriptive statistics such as mean, median, and standard deviation
  # these are reported as well as used to construct some bootstrap samples (e.g., mad or t)
  population_descriptives <- reactive({
    switch(input$population, 
           "normal" =      c(input$mean, 
                             input$mean, 
                             input$sd, 
                             input$sd^2, 
                             0, 
                             0), 
           
           "uniform" =     c((input$min + input$max) / 2, 
                             (input$min + input$max) / 2, 
                             sqrt((1 / 12) * (input$max - input$min)^2), 
                             (1 / 12) * (input$max - input$min)^2, 
                             0, 
                             -6 / 5), 
           
           "poisson" =     c(input$rate, 
                             input$rate + 1 / 3 - 0.02 / input$rate, 
                             sqrt(input$rate), 
                             input$rate, 
                             input$rate^(-1 / 2), 
                             1 / input$rate), 
           
           "binomial" =    c(input$size * input$prob, 
                             input$size * input$prob, 
                             sqrt(input$size * input$prob * (1 - input$prob)), 
                             input$size * input$prob * (1 - input$prob), 
                             (1 - 2 * input$prob) / (sqrt(input$size * input$prob * (1 - input$prob))), 
                             (1 - 6 * input$prob * (1 - input$prob)) / (input$size * input$prob * (1 - input$prob))), 
           
           "chi-square" =  c(input$df, 
                             input$df * (1 - 2 / (9 * input$df))^3, 
                             sqrt(2 * input$df), 
                             2 * input$df, 
                             sqrt(8 / input$df), 
                             12 / input$df), 
           
           "exponential" = c(1 / input$rate, 
                             input$rate^(-1) * log(2), 
                             sqrt(input$rate^(-2)), 
                             input$rate^(-2), 
                             2, 
                             6), 
           
           "custom" =      c(NA, 
                             NA, 
                             NA, 
                             NA, 
                             NA, 
                             NA))
  })
  
  output$population_descriptives <- renderPrint({
    vectxt <- paste(descriptive_labels, "=", format(round(population_descriptives(), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })
  
  
  
  ##### SAMPLE #####
  
  sample_draws <- reactive({
    rdist()(input$n_1)
  })
  
  output$sample_plot <- renderPlot({
    ggplot(tibble(sample_draws()), aes(x = sample_draws())) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Sample", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$xmin, input$xmax))
  })
  
  output$sample_descriptives <- renderPrint({
    cat(paste(paste(descriptive_labels, "=", format(round(descriptives(sample_draws()), 2), nsmall = 2)), collapse = "\n"))
  })
  
  
  
  ##### BOOTSTRAP 1 #####
  
  bootstrap_1_draws <- reactive({
    statistic_1 <- switch(input$T_1, 
                          "mean" =   function(x) mean(x, trim = input$T_1.trim), 
                          "median" = function(x) median(x), 
                          "sd" =     function(x) sd(x), 
                          "var" =    function(x) var(x), 
                          "var*" =   function(x) (length(x) - 1) / length(x) * var(x), 
                          "iqr" =    function(x) IQR(x, type = input$T_1.type), 
                          "range" =  function(x) diff(range(x)), 
                          "order" =  function(x) sort(x)[input$T_1.order], 
                          "t" =      function(x) t.test(x, mu = population_descriptives()[1])$statistic, 
                          "mad" =    function(x) mad(x, center = population_descriptives()[1]), 
                          "custom" = function(x) eval_tidy(parse_expr(input$T_1.custom)))
    
    replicate(input$R_1, {
      draws <- rdist()(input$n_1)
      statistic_1(draws)
    })
  })
  
  output$bootstrap_1_plot <- renderPlot({
    ggplot(tibble(bootstrap_1_draws()), aes(x = bootstrap_1_draws())) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Bootstrap Distribution 1", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$xmin, input$xmax))
  })
  
  output$bootstrap_1_descriptives <- renderPrint({
    vectxt <- paste(descriptive_labels, "=", format(round(descriptives(bootstrap_1_draws()), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })
  
  
  
  ##### BOOTSTRAP 2 #####
  
  bootstrap_2_draws <- reactive({
    statistic_2 <- switch(input$T_2, 
                          "mean" =   function(x) mean(x, trim = input$T_2.trim), 
                          "median" = function(x) median(x), 
                          "sd" =     function(x) sd(x), 
                          "var" =    function(x) var(x), 
                          "var*" =   function(x) (length(x) - 1) / length(x) * var(x), 
                          "iqr" =    function(x) IQR(x, type = input$T_2.type), 
                          "range" =  function(x) diff(range(x)), 
                          "order" =  function(x) sort(x)[input$T_2.order], 
                          "t" =      function(x) t.test(x, mu = population_descriptives()[1])$statistic, 
                          "mad" =    function(x) mad(x, center = population_descriptives()[1]), 
                          "custom" = function(x) eval_tidy(parse_expr(input$T_2.custom)))
    
    replicate(input$R_2, {
      draws <- rdist()(input$n_2)
      statistic_2(draws)
    })
  })
  
  output$bootstrap_2_plot <- renderPlot({
    ggplot(tibble(bootstrap_2_draws()), aes(x = bootstrap_2_draws())) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Boostrap Distribution 2", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$xmin, input$xmax))
  })
  
  output$bootstrap_2_descriptives <- renderPrint({
    vectxt <- paste(descriptive_labels, "=", format(round(descriptives(bootstrap_2_draws()), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })
}

shinyApp(ui, server)

# TODO: sample from custom population
# TODO: vertical lines
# TODO: outliers
# TODO: annimation
