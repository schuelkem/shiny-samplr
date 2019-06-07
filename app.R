##### IMPORTS #####

library(dplyr)
library(ggplot2)
library(rlang)
library(shiny)

source("sdist.R")
source("ssamp.R")



##### COMMON OBJECTS #####

pop_choices <- c("beta", 
                 "binomial", 
                 "chi-square", 
                 "exponential", 
                 "gamma", 
                 "log-normal", 
                 "normal", 
                 "poisson", 
                 "t", 
                 "uniform")

statistic_choices <- c("mean", 
                       "median", 
                       "sd", 
                       "var", 
                       "var*", 
                       "iqr", 
                       "range", 
                       "order", 
                       "t", 
                       "mad", 
                       "custom")

theme_common <- function() {
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())
}






ui <- fluidPage(
  
  withMathJax(), 
  
  titlePanel("shiny-samplr"), 
  
  sidebarLayout(
    
    sidebarPanel(
      
      fluidPage(
        
        navlistPanel(
          
          tabPanel("Population", 
                   
                   fluidPage(
                     
                     selectInput(inputId = "n_pop", label = "Number of Examples", choices = c("one", "two"), selected = "two"), 
                     
                     br(), br(), 
                     
                     selectInput(inputId = "pop_1", label = "Population 1", choices = pop_choices, selected = "normal"), 
                     
                     conditionalPanel("input.pop_1 == 'beta'", 
                                      numericInput(inputId = "pop_1_beta_shape1", label = "shape1", value = 1), 
                                      numericInput(inputId = "pop_1_beta_shape2", label = "shape2", value = 1)), 
                     
                     conditionalPanel("input.pop_1 == 'binomial'", 
                                      numericInput(inputId = "pop_1_binom_size", label = "size", value = 1), 
                                      numericInput(inputId = "pop_1_binom_prob", label = "prob", value = 0.5)), 
                     
                     conditionalPanel("input.pop_1 == 'chi-square'", 
                                      numericInput(inputId = "pop_1_chisq_df", label = "df", value = 1)), 
                     
                     conditionalPanel("input.pop_1 == 'exponential'", 
                                      numericInput(inputId = "pop_1_exp_rate", label = "rate", value = 1)), 
                     
                     conditionalPanel("input.pop_1 == 'gamma'", 
                                      numericInput(inputId = "pop_1_gamma_shape", label = "shape", value = 1), 
                                      numericInput(inputId = "pop_1_gamma_rate", label = "rate", value = 1)), 
                     
                     conditionalPanel("input.pop_1 == 'log-normal'", 
                                      numericInput(inputId = "pop_1_lnorm_meanlog", label = "meanlog", value = 0), 
                                      numericInput(inputId = "pop_1_lnorm_sdlog", label = "sdlog", value = 1)), 
                     
                     conditionalPanel("input.pop_1 == 'normal'", 
                                      numericInput(inputId = "pop_1_norm_mean", label = "mean", value = 0), 
                                      numericInput(inputId = "pop_1_norm_sd", label = "sd", value = 1)), 
                     
                     conditionalPanel("input.pop_1 == 'poisson'", 
                                      numericInput(inputId = "pop_1_pois_lambda", label = "lambda", value = 1)), 
                     
                     conditionalPanel("input.pop_1 == 't'", 
                                      numericInput(inputId = "pop_1_t_df", label = "df", value = 1)), 
                     
                     conditionalPanel("input.pop_1 == 'uniform'", 
                                      numericInput(inputId = "pop_1_unif_min", label = "min", value = 0), 
                                      numericInput(inputId = "pop_1_unif_max", label = "max", value = 1)), 
                     
                     conditionalPanel("input.n_pop == 'two'", 
                                      
                                      br(), br(), 
                                      
                                      selectInput(inputId = "pop_2", label = "Population 2", choices = pop_choices, selected = "normal"), 
                                      
                                      conditionalPanel("input.pop_2 == 'beta'", 
                                                       numericInput(inputId = "pop_2_beta_shape1", label = "shape1", value = 1), 
                                                       numericInput(inputId = "pop_2_beta_shape2", label = "shape2", value = 1)), 
                                      
                                      conditionalPanel("input.pop_2 == 'binomial'", 
                                                       numericInput(inputId = "pop_2_binom_size", label = "size", value = 1), 
                                                       numericInput(inputId = "pop_2_binom_prob", label = "prob", value = 0.5)), 
                                      
                                      conditionalPanel("input.pop_2 == 'chi-square'", 
                                                       numericInput(inputId = "pop_2_chisq_df", label = "df", value = 1)), 
                                      
                                      conditionalPanel("input.pop_2 == 'exponential'", 
                                                       numericInput(inputId = "pop_2_exp_rate", label = "rate", value = 1)), 
                                      
                                      conditionalPanel("input.pop_2 == 'gamma'", 
                                                       numericInput(inputId = "pop_2_gamma_shape", label = "shape", value = 1), 
                                                       numericInput(inputId = "pop_2_gamma_rate", label = "rate", value = 1)), 
                                      
                                      conditionalPanel("input.pop_2 == 'log-normal'", 
                                                       numericInput(inputId = "pop_2_lnorm_meanlog", label = "meanlog", value = 0), 
                                                       numericInput(inputId = "pop_2_lnorm_sdlog", label = "sdlog", value = 1)), 
                                      
                                      conditionalPanel("input.pop_2 == 'normal'", 
                                                       numericInput(inputId = "pop_2_norm_mean", label = "mean", value = 0), 
                                                       numericInput(inputId = "pop_2_norm_sd", label = "sd", value = 1)), 
                                      
                                      conditionalPanel("input.pop_2 == 'poisson'", 
                                                       numericInput(inputId = "pop_2_pois_lambda", label = "lambda", value = 1)), 
                                      
                                      conditionalPanel("input.pop_2 == 't'", 
                                                       numericInput(inputId = "pop_2_t_df", label = "df", value = 1)), 
                                      
                                      conditionalPanel("input.pop_2 == 'uniform'", 
                                                       numericInput(inputId = "pop_2_unif_min", label = "min", value = 0), 
                                                       numericInput(inputId = "pop_2_unif_max", label = "max", value = 1))
                     )
                     
                   )
                   
          ), 
          
          tabPanel("Sample", 
                   
                   fluidPage(
                     
                     numericInput(inputId = "n_1", label = HTML("$$ n_1 $$"), value = 15, min = 1), 
                     
                     conditionalPanel("input.n_pop == 'two'", 
                                      br(), br(), 
                                      numericInput(inputId = "n_2", label = HTML("$$ n_2 $$"), value = 15, min = 1)
                     )
                     
                   )
                   
          ), 
          
          tabPanel("Outliers", 
                   
                   fluidPage(
                     
                     numericInput(inputId = "out_prop_1", label = "Proportion of Outliers 1", value = 0, min = 0, max = 1), 
                     
                     selectInput(inputId = "out_side_1", label = "Side", choices = c("Both", "Lower", "Upper")), 
                     
                     br(), 
                     
                     numericInput(inputId = "out_single_value_1", label = "Single Outlier Magnitude 1", value = 1000, min = 0), 
                     
                     numericInput(inputId = "out_single_prob_1", label = "Probability of Inclusion", value = 0, min = 0, max = 1), 
                     
                     conditionalPanel("input.n_pop == 'two'", 
                                      
                                      br(), br(), 
                                      
                                      numericInput(inputId = "out_prop_2", label = "Proportion of Outliers 2", value = 0, min = 0, max = 1), 
                                      
                                      selectInput(inputId = "out_side_2", label = "Side", choices = c("Both", "Lower", "Upper")), 
                                      
                                      br(), 
                                      
                                      numericInput(inputId = "out_single_value_2", label = "Single Outlier Magnitude 2", value = 1000, min = 0), 
                                      
                                      numericInput(inputId = "out_single_prob_2", label = "Probability of Inclusion", value = 0, min = 0, max = 1)
                     )
                     
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
                     
                     conditionalPanel("input.n_pop == 'two'", 
                                      
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
                     
                   )
                   
          ), 
          
          tabPanel("Resamples", 
                   
                   fluidPage(
                     
                     numericInput(inputId = "R_1", label = HTML("$$ R_1 $$"), value = 1000, min = 1, max = 10000), 
                     
                     conditionalPanel("input.n_pop == 'two'", 
                                      br(), br(), 
                                      numericInput(inputId = "R_2", label = HTML("$$ R_2 $$"), value = 1000, min = 1, max = 10000)
                     )
                     
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
        
      ), width = 4
      
    ), 
    
    mainPanel(uiOutput(outputId = "plots"), width = 8)
    
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
  
  # vlines
  vlines <- reactive({
    switch(input$n_vlines, 
           "0" = geom_blank(), 
           "1" = geom_vline(xintercept = input$vline1), 
           "2" = geom_vline(xintercept = c(input$vline1, input$vline2)), 
           "3" = geom_vline(xintercept = c(input$vline1, input$vline2, input$vline3)))
  })
  
  
  
  
  
  
  ##### DISTRIBUTION FUNCTIONS #####
  
  # population 1
  ddist_1 <- reactive({
    switch(input$pop_1, 
           "beta" =        function(x) dbeta(x, shape1 = input$pop_1_beta_shape1, shape2 = input$pop_1_beta_shape2), 
           "binomial" =    function(x) dbinom(x, size = input$pop_1_binom_size, prob = input$pop_1_binom_prob), 
           "chi-square" =  function(x) dchisq(x, df = input$pop_1_chisq_df), 
           "exponential" = function(x) dexp(x, rate = input$pop_1_exp_rate), 
           "gamma" =       function(x) dgamma(x, shape = input$pop_1_gamma_shape, rate = input$pop_1_gamma_rate), 
           "log-normal" =  function(x) dlnorm(x, meanlog = input$pop_1_lnorm_meanlog, sdlog = input$pop_1_lnorm_sdlog), 
           "normal" =      function(x) dnorm(x, mean = input$pop_1_norm_mean, sd = input$pop_1_norm_sd), 
           "poisson" =     function(x) dpois(x, lambda = input$pop_1_pois_lambda), 
           "t" =           function(x) dt(x, df = input$pop_1_t_df), 
           "uniform" =     function(x) dunif(x, min = input$pop_1_unif_min, max = input$pop_1_unif_max))
  })
  
  qdist_1 <- reactive({
    switch(input$pop_1, 
           "beta" =        function(p) qbeta(p, shape1 = input$pop_1_beta_shape1, shape2 = input$pop_1_beta_shape2), 
           "binomial" =    function(p) qbinom(p, size = input$pop_1_binom_size, prob = input$pop_1_binom_prob), 
           "chi-square" =  function(p) qchisq(p, df = input$pop_1_chisq_df), 
           "exponential" = function(p) qexp(p, rate = input$pop_1_exp_rate), 
           "gamma" =       function(p) qgamma(p, shape = input$pop_1_gamma_shape, rate = input$pop_1_gamma_rate), 
           "log-normal" =  function(p) qlnorm(p, meanlog = input$pop_1_lnorm_meanlog, sdlog = input$pop_1_lnorm_sdlog), 
           "normal" =      function(p) qnorm(p, mean = input$pop_1_norm_mean, sd = input$pop_1_norm_sd), 
           "poisson" =     function(p) qpois(p, lambda = input$pop_1_pois_lambda), 
           "t" =           function(p) qt(p, df = input$pop_1_t_df), 
           "uniform" =     function(p) qunif(p, min = input$pop_1_unif_min, max = input$pop_1_unif_max))
  })
  
  rdist_1 <- reactive({
    switch(input$pop_1, 
           "beta" =        function(n) rbeta(n, shape1 = input$pop_1_beta_shape1, shape2 = input$pop_1_beta_shape2), 
           "binomial" =    function(n) rbinom(n, size = input$pop_1_binom_size, prob = input$pop_1_binom_prob), 
           "chi-square" =  function(n) rchisq(n, df = input$pop_1_chisq_df), 
           "exponential" = function(n) rexp(n, rate = input$pop_1_exp_rate), 
           "gamma" =       function(n) rgamma(n, shape = input$pop_1_gamma_shape, rate = input$pop_1_gamma_rate), 
           "log-normal" =  function(n) rlnorm(n, meanlog = input$pop_1_lnorm_meanlog, sdlog = input$pop_1_lnorm_sdlog), 
           "normal" =      function(n) rnorm(n, mean = input$pop_1_norm_mean, sd = input$pop_1_norm_sd), 
           "poisson" =     function(n) rpois(n, lambda = input$pop_1_pois_lambda), 
           "t" =           function(n) rt(n, df = input$pop_1_t_df), 
           "uniform" =     function(n) runif(n, min = input$pop_1_unif_min, max = input$pop_1_unif_max))
  })

  # population 2
  ddist_2 <- reactive({
    switch(input$pop_2, 
           "beta" =        function(x) dbeta(x, shape1 = input$pop_2_beta_shape1, shape2 = input$pop_2_beta_shape2), 
           "binomial" =    function(x) dbinom(x, size = input$pop_2_binom_size, prob = input$pop_2_binom_prob), 
           "chi-square" =  function(x) dchisq(x, df = input$pop_2_chisq_df), 
           "exponential" = function(x) dexp(x, rate = input$pop_2_exp_rate), 
           "gamma" =       function(x) dgamma(x, shape = input$pop_2_gamma_shape, rate = input$pop_2_gamma_rate), 
           "log-normal" =  function(x) dlnorm(x, meanlog = input$pop_2_lnorm_meanlog, sdlog = input$pop_2_lnorm_sdlog), 
           "normal" =      function(x) dnorm(x, mean = input$pop_2_norm_mean, sd = input$pop_2_norm_sd), 
           "poisson" =     function(x) dpois(x, lambda = input$pop_2_pois_lambda), 
           "t" =           function(x) dt(x, df = input$pop_2_t_df), 
           "uniform" =     function(x) dunif(x, min = input$pop_2_unif_min, max = input$pop_2_unif_max))
  })
  
  qdist_2 <- reactive({
    switch(input$pop_2, 
           "beta" =        function(p) qbeta(p, shape1 = input$pop_2_beta_shape1, shape2 = input$pop_2_beta_shape2), 
           "binomial" =    function(p) qbinom(p, size = input$pop_2_binom_size, prob = input$pop_2_binom_prob), 
           "chi-square" =  function(p) qchisq(p, df = input$pop_2_chisq_df), 
           "exponential" = function(p) qexp(p, rate = input$pop_2_exp_rate), 
           "gamma" =       function(p) qgamma(p, shape = input$pop_2_gamma_shape, rate = input$pop_2_gamma_rate), 
           "log-normal" =  function(p) qlnorm(p, meanlog = input$pop_2_lnorm_meanlog, sdlog = input$pop_2_lnorm_sdlog), 
           "normal" =      function(p) qnorm(p, mean = input$pop_2_norm_mean, sd = input$pop_2_norm_sd), 
           "poisson" =     function(p) qpois(p, lambda = input$pop_2_pois_lambda), 
           "t" =           function(p) qt(p, df = input$pop_2_t_df), 
           "uniform" =     function(p) qunif(p, min = input$pop_2_unif_min, max = input$pop_2_unif_max))
  })
  
  rdist_2 <- reactive({
    switch(input$pop_2, 
           "beta" =        function(n) rbeta(n, shape1 = input$pop_2_beta_shape1, shape2 = input$pop_2_beta_shape2), 
           "binomial" =    function(n) rbinom(n, size = input$pop_2_binom_size, prob = input$pop_2_binom_prob), 
           "chi-square" =  function(n) rchisq(n, df = input$pop_2_chisq_df), 
           "exponential" = function(n) rexp(n, rate = input$pop_2_exp_rate), 
           "gamma" =       function(n) rgamma(n, shape = input$pop_2_gamma_shape, rate = input$pop_2_gamma_rate), 
           "log-normal" =  function(n) rlnorm(n, meanlog = input$pop_2_lnorm_meanlog, sdlog = input$pop_2_lnorm_sdlog), 
           "normal" =      function(n) rnorm(n, mean = input$pop_2_norm_mean, sd = input$pop_2_norm_sd), 
           "poisson" =     function(n) rpois(n, lambda = input$pop_2_pois_lambda), 
           "t" =           function(n) rt(n, df = input$pop_2_t_df), 
           "uniform" =     function(n) runif(n, min = input$pop_2_unif_min, max = input$pop_2_unif_max))
  })



  
  
  
  ##### POPULATION #####
  
  # popuation 1
  output$pop_1_plot <- renderPlot({
    input$n_pop
    
    ggplot() + 
      switch(input$pop_1, 
             "poisson" = geom_step(aes(x = x, y = y), 
                                   tibble(x = seq(input$plot.xmin - 1, input$plot.xmax + 1, 1) - 0.5, 
                                          y = dpois(x + 0.5, lambda = input$pop_1_pois_lambda)), 
                                   color = "#337ab7"), 
             "binomial" = geom_step(aes(x = x, y = y), 
                                    tibble(x = seq(input$plot.xmin - 1, input$plot.xmax + 1, 1) - 0.5, 
                                           y = dbinom(x + 0.5, size = input$pop_1_binom_size, input$pop_1_binom_prob)), 
                                    color = "#337ab7"), 
             stat_function(aes(x = input$plot.xmin:input$plot.xmax), 
                           n = (input$plot.xmax - input$plot.xmin) * 15, 
                           fun = ddist_1(), 
                           color = "#337ab7")
      ) + 
      labs(title = "Population 1", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
  })

  ## calculate population descriptive statistics such as mean, median, and standard deviation
  ## these are reported as well as used to construct some bootstrap samples (e.g., mad or t)
  pop_1_descriptives <- reactive({
    switch(input$pop_1, 
           "beta" =        sbeta(input$pop_1_beta_shape1, input$pop_1_beta_shape2), 
           "binomial" =    sbinom(input$pop_1_binom_size, input$pop_1_binom_prob), 
           "chi-square" =  schisq(input$pop_1_chisq_df), 
           "exponential" = sexp(input$pop_1_exp_rate), 
           "gamma" =       sgamma(input$pop_1_gamma_shape, input$pop_1_gamma_rate), 
           "log-normal" =  slnorm(input$pop_1_lnorm_meanlog, input$pop_1_lnorm_sdlog), 
           "normal" =      snorm(input$pop_1_norm_mean, input$pop_1_norm_sd), 
           "poisson" =     spois(input$pop_1_pois_lambda), 
           "t" =           st(input$pop_1_t_df), 
           "uniform" =     sunif(input$pop_1_unif_min, input$pop_1_unif_max)
    )
  })
  
  output$pop_1_descriptives <- renderPrint({
    input$n_pop
    vectxt <- paste(names(pop_1_descriptives()), "=", format(round(pop_1_descriptives(), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })

  # population 2
  output$pop_2_plot <- renderPlot({
    input$n_pop
    
    ggplot() + 
      switch(input$pop_2, 
             "poisson" = geom_step(aes(x = x, y = y), 
                                   tibble(x = seq(input$plot.xmin - 1, input$plot.xmax + 1, 1) - 0.5, 
                                          y = dpois(x + 0.5, lambda = input$pop_2_pois_lambda)), 
                                   color = "#337ab7"), 
             "binomial" = geom_step(aes(x = x, y = y), 
                                    tibble(x = seq(input$plot.xmin - 1, input$plot.xmax + 1, 1) - 0.5, 
                                           y = dbinom(x + 0.5, size = input$pop_2_binom_size, input$pop_2_binom_prob)), 
                                    color = "#337ab7"), 
             stat_function(aes(x = input$plot.xmin:input$plot.xmax), 
                           n = (input$plot.xmax - input$plot.xmin) * 15, 
                           fun = ddist_2(), 
                           color = "#337ab7")
      ) + 
      labs(title = "Population 2", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
  })
  
  ## calculate population descriptive statistics such as mean, median, and standard deviation
  ## these are reported as well as used to construct some bootstrap samples (e.g., mad or t)
  pop_2_descriptives <- reactive({
    switch(input$pop_2, 
           "beta" =        sbeta(input$pop_2_beta_shape1, input$pop_2_beta_shape2), 
           "binomial" =    sbinom(input$pop_2_binom_size, input$pop_2_binom_prob), 
           "chi-square" =  schisq(input$pop_2_chisq_df), 
           "exponential" = sexp(input$pop_2_exp_rate), 
           "gamma" =       sgamma(input$pop_2_gamma_shape, input$pop_2_gamma_rate), 
           "log-normal" =  slnorm(input$pop_2_lnorm_meanlog, input$pop_2_lnorm_sdlog), 
           "normal" =      snorm(input$pop_2_norm_mean, input$pop_2_norm_sd), 
           "poisson" =     spois(input$pop_2_pois_lambda), 
           "t" =           st(input$pop_2_t_df), 
           "uniform" =     sunif(input$pop_2_unif_min, input$pop_2_unif_max)
    )
  })
  
  output$pop_2_descriptives <- renderPrint({
    input$n_pop
    vectxt <- paste(names(pop_2_descriptives()), "=", format(round(pop_2_descriptives(), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })

  
  

  
    
  ##### SAMPLE #####
  
  # sample 1
  sample_1_draws <- reactive({
    rdist_1()(input$n_1)
  })
  
  output$sample_1_plot <- renderPlot({
    input$n_pop
    
    ggplot(tibble(sample_1_draws()), aes(x = sample_1_draws())) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Sample 1", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
  })
  
  sample_1_descriptives <- reactive({
    ssamp(sample_1_draws())
  })
  
  output$sample_1_descriptives <- renderPrint({
    input$n_pop
    vectxt <- paste(names(sample_1_descriptives()), "=", format(round(sample_1_descriptives(), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })
  
  # sample 2
  sample_2_draws <- reactive({
    rdist_2()(input$n_2)
  })
  
  output$sample_2_plot <- renderPlot({
    input$n_pop
    
    ggplot(tibble(sample_2_draws()), aes(x = sample_2_draws())) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Sample 2", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
  })
  
  sample_2_descriptives <- reactive({
    ssamp(sample_2_draws())
  })
  
  output$sample_2_descriptives <- renderPrint({
    input$n_pop
    vectxt <- paste(names(sample_2_descriptives()), "=", format(round(sample_2_descriptives(), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })
  
  
  
  
  
  
  ##### BOOTSTRAP #####
  
  # bootstrap 1
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
                          "t" =      function(x) t.test(x, mu = pop_descriptives()[1])$statistic, 
                          "mad" =    function(x) mad(x, center = pop_descriptives()[1]), 
                          "custom" = function(x) eval_tidy(parse_expr(input$T_1.custom)))
    
    replicate(input$R_1, {
      draws <- if(input$out_prop_1 == 0) {
        rdist_1()(input$n_1)
      } else {
        switch(input$out_side_1, 
               "Both" = {
                 n_out <- 2 * round(input$out_prop_1 * input$n_1 / 2)
                 n_in <- input$n_1 - n_out
                 draws_in <- rdist_1()(n_in)
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
                 draws_in <- rdist_1()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_lwr <- runif(n_out / 2, 
                                        min = q[1] - 4.5 * (q[3] - q[1]),
                                        max = q[1] - 1.5 * (q[3] - q[1]))
                 c(draws_out_lwr, draws_in)
               }, 
               "Upper" = {
                 n_out <- round(input$out_prop_1 * input$n_1)
                 n_in <- input$n_1 - n_out
                 draws_in <- rdist_1()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_upr <- runif(n_out / 2, 
                                        min = q[3] + 1.5 * (q[3] - q[1]), 
                                        max = q[3] + 4.5 * (q[3] - q[1]))
                 c(draws_in, draws_out_upr)
               }
               )
      }
      
      if(runif(1) < input$out_single_prob_1)
        draws[sample(length(draws), 1)] <- sample(c(-1,1), 1) * input$out_single_value_1
      
      statistic_1(draws)
    })
  })
  
  output$bootstrap_1_plot <- renderPlot({
    input$n_pop
    
    ggplot(tibble(bootstrap_1_draws()), aes(x = bootstrap_1_draws())) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Bootstrap Sampling Distribution 1", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
  })
  
  bootstrap_1_descriptives <- reactive({
    ssamp(bootstrap_1_draws())
  })
  
  output$bootstrap_1_descriptives <- renderPrint({
    input$n_pop
    vectxt <- paste(names(bootstrap_1_descriptives()), "=", format(round(bootstrap_1_descriptives(), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })
  
  
  
  # bootstrap 2
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
                          "t" =      function(x) t.test(x, mu = pop_descriptives()[1])$statistic, 
                          "mad" =    function(x) mad(x, center = pop_descriptives()[1]), 
                          "custom" = function(x) eval_tidy(parse_expr(input$T_2.custom)))
    
    replicate(input$R_2, {
      draws <- if(input$out_prop_2 == 0) {
        rdist_2()(input$n_2)
      } else {
        switch(input$out_side_2, 
               "Both" = {
                 n_out <- 2 * round(input$out_prop_2 * input$n_2 / 2)
                 n_in <- input$n_2 - n_out
                 draws_in <- rdist_2()(n_in)
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
                 draws_in <- rdist_2()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_lwr <- runif(n_out / 2, 
                                        min = q[1] - 4.5 * (q[3] - q[1]),
                                        max = q[1] - 1.5 * (q[3] - q[1]))
                 c(draws_out_lwr, draws_in)
               }, 
               "Upper" = {
                 n_out <- round(input$out_prop_2 * input$n_2)
                 n_in <- input$n_2 - n_out
                 draws_in <- rdist_2()(n_in)
                 q <- quantile(draws_in, probs = seq(0.25, 1, 0.25))
                 draws_out_upr <- runif(n_out / 2, 
                                        min = q[3] + 1.5 * (q[3] - q[1]), 
                                        max = q[3] + 4.5 * (q[3] - q[1]))
                 c(draws_in, draws_out_upr)
               }
        )
      }
      
      if(runif(1) < input$out_single_prob_2)
        draws[sample(length(draws), 1)] <- sample(c(-1,1), 1) * input$out_single_value_2
      
      statistic_2(draws)
    })
  })
  
  output$bootstrap_2_plot <- renderPlot({
    input$n_pop
    
    ggplot(tibble(bootstrap_2_draws()), aes(x = bootstrap_2_draws())) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Boostrap Sampling Distribution 2", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(input$plot.xmin, input$plot.xmax)) + 
      vlines()
  })
  
  bootstrap_2_descriptives <- reactive({
    ssamp(bootstrap_2_draws())
  })
  
  output$bootstrap_2_descriptives <- renderPrint({
    input$n_pop
    vectxt <- paste(names(bootstrap_2_descriptives()), "=", format(round(bootstrap_2_descriptives(), 2), nsmall = 2))
    cat(paste(vectxt, collapse = "\n"))
  })
  
  
  
  
  
  ##### PLOT OUTPUT #####
  output$plots <- renderUI({
    if (input$n_pop == "one") {
      fluidRow(
        column(12,
          fluidRow(
            column(8, plotOutput(outputId = "pop_1_plot", height = 200)),
            column(4, br(), verbatimTextOutput(outputId = "pop_1_descriptives"))
          ),
          
          fluidRow(
            column(8, plotOutput(outputId = "sample_1_plot", height = 200)),
            column(4, br(), verbatimTextOutput(outputId = "sample_1_descriptives"))
          ),
          
          fluidRow(
            column(8, plotOutput(outputId = "bootstrap_1_plot", height = 200)),
            column(4, br(), verbatimTextOutput(outputId = "bootstrap_1_descriptives"))
          )
          
        )
      )
    } else {
      fluidRow(
        column(6,
               fluidRow(
                 column(8, plotOutput(outputId = "pop_1_plot", height = 200)),
                 column(4, br(), verbatimTextOutput(outputId = "pop_1_descriptives"))
               ),
               
               fluidRow(
                 column(8, plotOutput(outputId = "sample_1_plot", height = 200)),
                 column(4, br(), verbatimTextOutput(outputId = "sample_1_descriptives"))
               ),
               
               fluidRow(
                 column(8, plotOutput(outputId = "bootstrap_1_plot", height = 200)),
                 column(4, br(), verbatimTextOutput(outputId = "bootstrap_1_descriptives"))
               )
               
        ), 
        
        column(6,
               fluidRow(
                 column(8, plotOutput(outputId = "pop_2_plot", height = 200)),
                 column(4, br(), verbatimTextOutput(outputId = "pop_2_descriptives"))
               ),
               
               fluidRow(
                 column(8, plotOutput(outputId = "sample_2_plot", height = 200)),
                 column(4, br(), verbatimTextOutput(outputId = "sample_2_descriptives"))
               ),
               
               fluidRow(
                 column(8, plotOutput(outputId = "bootstrap_2_plot", height = 200)),
                 column(4, br(), verbatimTextOutput(outputId = "bootstrap_2_descriptives"))
               )
               
        )
        
      )
    }
  })
}






shinyApp(ui, server)
