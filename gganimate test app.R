library(gganimate)
library(ggplot2)
library(shiny)

theme_common <- function() {
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())
}

ui <- fluidPage(
  uiOutput("plots")
)

server <- function(input, output) {
  resample_size <- 5
  sample_size <- 10
  
  df_population <- data.frame(frame = numeric(0), 
                              x = numeric(0), 
                              y = numeric(0), 
                              xend = numeric(0), 
                              yend = numeric(0))
  
  df_sample <- data.frame(frame = numeric(0), 
                          x = numeric(0))
  
  df_resamples <- data.frame(frame = numeric(0), 
                             x = numeric(0))
  
  for(r in 1:resample_size) {
    for(s in 1:sample_size) {
      frame = (r - 1) * sample_size + s
      next_obs <- rnorm(1)
      
      df_population <- rbind(df_population, 
                             data.frame(frame = frame, 
                                        x = next_obs, y = 0, 
                                        xend = next_obs, yend = dnorm(next_obs)))
      
      if(s == 1) {
        df_sample <- rbind(df_sample, data.frame(frame = frame, x = next_obs))
      } else {
        last_frame_sample <- df_sample[df_sample$frame == frame - 1, "x"]
        df_sample <- rbind(df_sample, data.frame(frame = frame, x = c(last_frame_sample, next_obs)))
      }
      
      if(r == 1) {
        if(s != sample_size)
          df_resamples <- rbind(df_resamples, data.frame(frame = frame, x = NA))
      } else {
        last_frame_resamples <- df_resamples[df_resamples$frame == frame - 1, "x"]
        df_resamples <- rbind(df_resamples, data.frame(frame = frame, x = last_frame_resamples))
      }
      
      if(s == sample_size) {
        this_frame_sample <- df_sample[df_sample$frame == frame, "x"]
        next_stat <- mean(this_frame_sample)
        df_resamples <- rbind(df_resamples, data.frame(frame = frame, x = next_stat))
      }
    }
  }
  
  output$pop_plot <- renderImage({
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making population annimation", value = 0)
    
    outfile <- tempfile(fileext='.gif')
    
    p = ggplot() + 
      stat_function(aes(x = (-4):4), 
                    n = (4 - (-4)) * 15, 
                    fun = dnorm, 
                    color = "#337ab7") + 
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = df_population) + 
      labs(title = "Population 1", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(-4, 4)) + 
      transition_manual(frame)
    
    anim_save("outfile.gif", animate(p))
    
    list(src = "outfile.gif",
         contentType = 'image/gif', 
         # width = 400,
         height = 200
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$samp_plot <- renderImage({
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making sample annimation", value = 0)
    
    outfile <- tempfile(fileext='.gif')
    
    p =  ggplot(df_sample, aes(x = x)) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Sample 1", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(-4, 4)) + 
      transition_manual(frame)
    
    anim_save("outfile.gif", animate(p))
    
    list(src = "outfile.gif",
         contentType = 'image/gif', 
         # width = 400,
         height = 200
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$resamp_plot <- renderImage({
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making resample annimation", value = 0)
    
    outfile <- tempfile(fileext='.gif')
    
    p =  ggplot(df_resamples, aes(x = x)) + 
      geom_histogram(fill = "#337ab7") + 
      labs(title = "Bootstrap Sampling Distribution 1", x = "") + 
      theme_common() + 
      coord_cartesian(xlim = c(-4, 4)) + 
      transition_manual(frame)
    
    anim_save("outfile.gif", animate(p))
    
    list(src = "outfile.gif",
         contentType = 'image/gif',
         # width = 400,
         height = 200
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$plots <- renderUI({
    fluidRow(
      fluidRow(column(12, imageOutput("pop_plot", height = 200))), 
      
      fluidRow(column(12, imageOutput("samp_plot", height = 200))), 
      
      fluidRow(column(12, imageOutput("resamp_plot", height = 200)))
    )
  })
}

shinyApp(ui, server)
