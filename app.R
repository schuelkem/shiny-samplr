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

population_choices <- c("beta", "binomial", "chi-square", "exponential", "gamma", "normal", "poisson", "t", "uniform")
statistic_choices <- c("mean", "median", "sd", "var", "var*", "iqr", "range", "order", "t", "mad", "custom")
descriptive_labels <- c("mean", "median", "sd", "var", "skew", "kurtosis")






ui <- fluidPage(
  withMathJax(), 
  
  titlePanel("shiny-samplr"), 
  
  sidebarLayout(
    sidebarPanel(
      fluidPage(
        navlistPanel(
          tabPanel("Population", 
                   fluidPage(
                     selectInput(inputId = "population", label = "", choices = population_choices, selected = "normal"), 
                     
                     br(), 
                     
                     conditionalPanel("input.population == 'beta'", 
                                      numericInput(inputId = "shape1", label = "shape1", value = 1), 
                                      numericInput(inputId = "shape2", label = "shape2", value = 1)), 
                     
                     conditionalPanel("input.population == 'binomial'", 
                                      numericInput(inputId = "size", label = "size", value = 1), 
                                      numericInput(inputId = "prob", label = "prob", value = 0.5)), 
                     
                     conditionalPanel("input.population == 'chi-square'", 
                                      numericInput(inputId = "df", label = "df", value = 1)), 
                     
                     conditionalPanel("input.population == 'exponential'", 
                                      numericInput(inputId = "rate", label = "rate", value = 1)), 
                     
                     conditionalPanel("input.population == 'gamma'", 
                                      numericInput(inputId = "shape", label = "shape", value = 1), 
                                      numericInput(inputId = "rate", label = "rate", value = 1)), 
                     
                     conditionalPanel("input.population == 'normal'", 
                                      numericInput(inputId = "mean", label = "mean", value = 0), 
                                      numericInput(inputId = "sd", label = "sd", value = 1)), 
                     
                     conditionalPanel("input.population == 'poisson'", 
                                      numericInput(inputId = "lambda", label = "lambda", value = 1)), 
                     
                     conditionalPanel("input.population == 't'", 
                                      numericInput(inputId = "t_df", label = "df", value = 1)), 
                     
                     conditionalPanel("input.population == 'uniform'", 
                                      numericInput(inputId = "min", label = "min", value = 0), 
                                      numericInput(inputId = "max", label = "max", value = 1))
                   )
          ), 
          
          tabPanel("Sample", 
                   fluidPage(
                     numericInput(inputId = "n_1", label = HTML("$$ n_1 $$"), value = 15, min = 1), 
                     
                     br(), br(), 
                     
                     numericInput(inputId = "n_2", label = HTML("$$ n_2 $$"), value = 15, min = 1)
                   )
          ), 
          
          tabPanel("Outliers", 
                   fluidPage(
                     numericInput(inputId = "out_prop_1", label = "Proportion of Outliers 1", value = 0, min = 0, max = 1), 
                     
                     selectInput(inputId = "out_side_1", label = "Side", choices = c("Both", "Lower", "Upper")), 
                     
                     br(), 
                     
                     numericInput(inputId = "out_single_value_1", label = "Single Outlier Value 1", value = 1000), 
                     
                     numericInput(inputId = "out_single_prob_1", label = "Probability of Inclusion", value = 0, min = 0, max = 1), 
                     
                     br(), br(), 
                     
                     numericInput(inputId = "out_prop_2", label = "Proportion of Outliers 2", value = 0, min = 0, max = 1), 
                     
                     selectInput(inputId = "out_side_2", label = "Side", choices = c("Both", "Lower", "Upper")), 
                     
                     br(), 
                     
                     numericInput(inputId = "out_single_value_2", label = "Single Outlier Value 2", value = 1000), 
                     
                     numericInput(inputId = "out_single_prob_2", label = "Probability of Inclusion", value = 0, min = 0, max = 1)
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
                     
                     br(), br(), 
                     
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
                     numericInput(inputId = "R_1", label = HTML("$$ R_1 $$"), value = 1000, min = 1, max = 10000), 
                     
                     br(), br(), 
                     
                     numericInput(inputId = "R_2", label = HTML("$$ R_2 $$"), value = 1000, min = 1, max = 10000)
                   )
          ), 
          
          tabPanel("Plot", 
                   fluidPage(
                     fluidRow(
                       column(numericInput(inputId = "plot.xmin", label = "From", value = -4), width = 6),
                       column(numericInput(inputId = "plot.xmax", label = "To", value =  4), width = 6)
                     ), 
                     
                     br(), 
                     
                     selectInput(inputId = "n_vlines", label = "Number of Vertical Lines", choices = 0:3, selected = 0), 
                     
                     conditionalPanel("input.n_vlines > 0", 
                                      numericInput(inputId = "vline1", label = "vline 1", value = 0)), 
                     
                     conditionalPanel("input.n_vlines > 1", 
                                      numericInput(inputId = "vline2", label = "vline 2", value = 0)), 
                     
                     conditionalPanel("input.n_vlines > 2", 
                                      numericInput(inputId = "vline3", label = "vline 3", value = 0))
                   )
          )
        )
      ), width = 3
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
      ), width = 9
    )
  )
)

server <- function(input, output, session) {
  
  ##### UPDATE INPUTS #####
  
  # if not on localhost remove "custom" option for both statistics
  observe({
    if(session$clientData$url_hostname != "127.0.0.1") {
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
  
  ## vlines
  vlines <- reactive({
    switch(input$n_vlines, 
           "0" = geom_blank(), 
           "1" = geom_vline(xintercept = input$vline1), 
           "2" = geom_vline(xintercept = c(input$vline1, input$vline2)), 
           "3" = geom_vline(xintercept = c(input$vline1, input$vline2, input$vline3)))
  })
  
  
  
  ##### DISTRIBUTION FUNCTIONS #####
  
  ddist <- reactive({
    switch(input$population, 
           "beta" =        function(x) dbeta(x, shape1 = input$shape1, shape2 = input$shape2), 
           "binomial" =    function(x) dbinom(x, size = input$size, prob = input$prob), 
           "chi-square" =  function(x) dchisq(x, df = input$df), 
           "exponential" = function(x) dexp(x, rate = input$rate), 
           "gamma" =       function(x) dgamma(x, shape = input$shape, rate = input$rate), 
           "normal" =      function(x) dnorm(x, mean = input$mean, sd = input$sd), 
           "poisson" =     function(x) dpois(x, lambda = input$lambda), 
           "t" =           function(x) dt(x, df = input$t_df), 
           "uniform" =     function(x) dunif(x, min = input$min, max = input$max))
  })
  
  pdist <- reactive({
    switch(input$population, 
           "beta" =        function(q) pbeta(q, shape1 = input$shape1, shape2 = input$shape2), 
           "binomial" =    function(q) pbinom(q, size = input$size, prob = input$prob), 
           "chi-square" =  function(q) pchisq(q, df = input$df), 
           "exponential" = function(q) pexp(q, rate = input$rate), 
           "gamma" =       function(q) pgamma(q, shape = input$shape, rate = input$rate), 
           "normal" =      function(q) pnorm(q, mean = input$mean, sd = input$sd), 
           "poisson" =     function(q) ppois(q, lambda = input$lambda), 
           "t" =           function(q) pt(q, df = input$t_df), 
           "uniform" =     function(q) punif(q, min = input$min, max = input$max))
  })
  
  qdist <- reactive({
    switch(input$population, 
           "beta" =        function(p) qbeta(p, shape1 = input$shape1, shape2 = input$shape2), 
           "binomial" =    function(p) qbinom(p, size = input$size, prob = input$prob), 
           "chi-square" =  function(p) qchisq(p, df = input$df), 
           "exponential" = function(p) qexp(p, rate = input$rate), 
           "gamma" =       function(p) qgamma(p, shape = input$shape, rate = input$rate), 
           "normal" =      function(p) qnorm(p, mean = input$mean, sd = input$sd), 
           "poisson" =     function(p) qpois(p, lambda = input$lambda), 
           "t" =           function(p) qt(p, df = input$t_df), 
           "uniform" =     function(p) qunif(p, min = input$min, max = input$max))
  })
  
  rdist <- reactive({
    switch(input$population, 
           "beta" =        function(n) rbeta(n, shape1 = input$shape1, shape2 = input$shape2), 
           "binomial" =    function(n) rbinom(n, size = input$size, prob = input$prob), 
           "chi-square" =  function(n) rchisq(n, df = input$df), 
           "exponential" = function(n) rexp(n, rate = input$rate), 
           "gamma" =       function(n) rgamma(n, shape = input$shape, rate = input$rate), 
           "normal" =      function(n) rnorm(n, mean = input$mean, sd = input$sd), 
           "poisson" =     function(n) rpois(n, lambda = input$lambda), 
           "t" =           function(n) rt(n, df = input$t_df), 
           "uniform" =     function(n) runif(n, min = input$min, max = input$max))
  })
  
  
  
  ##### POPULATION #####
  
  output$population_plot <- renderPlot({
    ggplot() + 
      switch(input$population, 
             "poisson" = geom_step(aes(x = x, y = y), 
                                   tibble(x = seq(input$plot.xmin - 1, input$plot.xmax + 1, 1) - 0.5, 
                                          y = dpois(x + 0.5, lambda = input$lambda)), 
                                   color = "#337ab7"), 
             "binomial" = geom_step(aes(x = x, y = y), 
                                    tibble(x = seq(input$plot.xmin - 1, input$plot.xmax + 1, 1) - 0.5, 
                                           y = dbinom(x + 0.5, size = input$size, input$prob)), 
                                    color = "#337ab7"), 
             stat_function(aes(x = input$plot.xmin:input$plot.xmax), 
                           n = (input$plot.xmax - input$plot.xmin) * 15, 
                           fun = ddist(), 
                           color = "#337ab7")
      ) + 
      labs(title = "Population", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
  })
  
  # calculate population descriptive statistics such as mean, median, and standard deviation
  # these are reported as well as used to construct some bootstrap samples (e.g., mad or t)
  population_descriptives <- reactive({
    switch(input$population, 
           "beta" =        { 
                             a <- input$shape1
                             b <- input$shape2
                             var_x <- (a * b) / ((a + b)^2 * (a + b + 1))
                             
                             c(a / (a + b), 
                               qdist()(0.5), 
                               sqrt(var_x), 
                               var_x, 
                               (2 * (b - a) * sqrt(a + b + 1)) / ((a + b + 2) * sqrt(a * b)), 
                               (6 * ((a - b)^2 * (a + b + 1) - a * b * (a + b + 2))) / (a * b * (a + b + 2) * (a + b + 3))
                              ) 
                           }, 
           
           "binomial" =    {
                             n <- input$size
                             p <- input$prob
                             var_x <- n * p * (1 - p)
                               
                             c(n * p, 
                               n * p, 
                               sqrt(var_x), 
                               var_x, 
                               (1 - 2 * input$prob) / (sqrt(input$size * input$prob * (1 - input$prob))), 
                               (1 - 6 * input$prob * (1 - input$prob)) / (input$size * input$prob * (1 - input$prob))
                              )
                           }, 
           
           "chi-square" =  {
                             df <- input$df
                             var_x <- 2 * df
                             
                             c(df, 
                               df * (1 - 2 / (9 * df))^3, 
                               sqrt(var_x), 
                               var_x, 
                               sqrt(8 / df), 
                               12 / df
                             )
                           }, 
           
           "exponential" = {
                             rate <- input$rate
                             var_x <- rate^(-2)
                             
                             c(1 / rate, 
                               rate^(-1) * log(2), 
                               sqrt(var_x), 
                               var_x, 
                               2, 
                               6
                             )
                           }, 
           
           "gamma" =       {
                             a <- input$shape
                             b <- input$rate
                             var_x <- a / b^2
                             
                             c(a / b, 
                               qdist()(0.5), 
                               sqrt(var_x), 
                               var_x, 
                               2 / sqrt(a), 
                               6 / a
                             )
                           }, 
           
           "normal" =      {
                             mu <- input$mean
                             var_x <- input$sd^2
                             
                             c(mu, 
                               mu, 
                               sqrt(var_x), 
                               var_x, 
                               0, 
                               0
                             )
                           }, 
           
           "poisson" =     {
                             rate <- input$rate
                             var_x <- rate
                             
                             c(rate, 
                               rate + 1 / 3 - 0.02 / rate, 
                               sqrt(var_x), 
                               var_x, 
                               rate^(-1 / 2), 
                               1 / rate
                             )
                           }, 
           
           "t" =           {
                             v <- input$t_df
                             var_x <- ifelse(v > 2, 
                                             v / (v - 2), 
                                             ifelse(v > 1, Inf, NA))
                             
                             c(ifelse(v > 1, 0, NA), 
                               0, 
                               sqrt(var_x), 
                               var_x, 
                               ifelse(v > 3, 0, NA), 
                               ifelse(v > 4, 6 / (v - 4), ifelse(v > 2, Inf, NA))
                             )
           }, 
           
           "uniform" =     {
                             a <- input$min
                             b <- input$max
                             var_x <- (1 / 12) * (input$max - input$min)^2
                             
                             c((a + b) / 2, 
                               (a + b) / 2, 
                               sqrt(var_x), 
                               var_x, 
                               0, 
                               -6 / 5
                             )
                           }
          )
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
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
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
      draws <- if(input$out_prop_1 == 0) {
        rdist()(input$n_1)
      } else {
        switch(input$out_side_1, 
               "Both" = {
                 n_out <- 2 * round(input$out_prop_1 * input$n_1 / 2)
                 n_in <- input$n_1 - n_out
                 draws_in <- rdist()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_lwr <- runif(n_out / 2, 
                                        min = q[1] - 4.5 * (q[3] - q[1]),
                                        max = q[1] - 1.5 * (q[3] - q[1]))
                 draws_out_upr <- runif(n_out / 2, 
                                        min = q[3] + 1.5 * (q[3] - q[1]), 
                                        max = q[3] + 4.5 * (q[3] - q[1]))
                 c(draws_out_lwr, draws_in, draws_out_upr)
               }, 
               "Lower" = {
                 n_out <- round(input$out_prop_1 * input$n_1)
                 n_in <- input$n_1 - n_out
                 draws_in <- rdist()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_lwr <- runif(n_out / 2, 
                                        min = q[1] - 4.5 * (q[3] - q[1]),
                                        max = q[1] - 1.5 * (q[3] - q[1]))
                 c(draws_out_lwr, draws_in)
               }, 
               "Upper" = {
                 n_out <- round(input$out_prop_1 * input$n_1)
                 n_in <- input$n_1 - n_out
                 draws_in <- rdist()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_upr <- runif(n_out / 2, 
                                        min = q[3] + 1.5 * (q[3] - q[1]), 
                                        max = q[3] + 4.5 * (q[3] - q[1]))
                 c(draws_in, draws_out_upr)
               }
               )
      }
      
      if(runif(1) < input$out_single_prob_1)
        draws[sample(length(draws), 1)] <- input$out_single_value_1
      
      statistic_1(draws)
    })
  })
  
  output$bootstrap_1_plot <- renderPlot({
    ggplot(tibble(bootstrap_1_draws()), aes(x = bootstrap_1_draws())) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Bootstrap Distribution 1", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
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
      draws <- if(input$out_prop_2 == 0) {
        rdist()(input$n_2)
      } else {
        switch(input$out_side_2, 
               "Both" = {
                 n_out <- 2 * round(input$out_prop_2 * input$n_2 / 2)
                 n_in <- input$n_2 - n_out
                 draws_in <- rdist()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_lwr <- runif(n_out / 2, 
                                        min = q[1] - 4.5 * (q[3] - q[1]),
                                        max = q[1] - 1.5 * (q[3] - q[1]))
                 draws_out_upr <- runif(n_out / 2, 
                                        min = q[3] + 1.5 * (q[3] - q[1]), 
                                        max = q[3] + 4.5 * (q[3] - q[1]))
                 c(draws_out_lwr, draws_in, draws_out_upr)
               }, 
               "Lower" = {
                 n_out <- round(input$out_prop_2 * input$n_2)
                 n_in <- input$n_2 - n_out
                 draws_in <- rdist()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_lwr <- runif(n_out / 2, 
                                        min = q[1] - 4.5 * (q[3] - q[1]),
                                        max = q[1] - 1.5 * (q[3] - q[1]))
                 c(draws_out_lwr, draws_in)
               }, 
               "Upper" = {
                 n_out <- round(input$out_prop_2 * input$n_2)
                 n_in <- input$n_2 - n_out
                 draws_in <- rdist()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_upr <- runif(n_out / 2, 
                                        min = q[3] + 1.5 * (q[3] - q[1]), 
                                        max = q[3] + 4.5 * (q[3] - q[1]))
                 c(draws_in, draws_out_upr)
               }
        )
      }
      
      if(runif(1) < input$out_single_prob_2)
        draws[sample(length(draws), 1)] <- input$out_single_value_2
      
      statistic_2(draws)
    })
  })
  
  output$bootstrap_2_plot <- renderPlot({
    ggplot(tibble(bootstrap_2_draws()), aes(x = bootstrap_2_draws())) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Boostrap Distribution 2", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
  })
  
  output$bootstrap_2_descriptives <- renderPrint({
    vectxt <- paste(descriptive_labels, "=", format(round(descriptives(bootstrap_2_draws()), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })
}

shinyApp(ui, server)
