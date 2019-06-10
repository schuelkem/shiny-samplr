library(ggplot2)
library(gganimate)

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



ggplot() + 
  stat_function(aes(x = (-4):4), 
                n = (4 - (-4)) * 15, 
                fun = dnorm, 
                color = "#337ab7") + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = df_population) + 
  transition_manual(frame)

ggplot(df_sample, aes(x = x)) + 
  geom_histogram() + 
  transition_manual(frame)

ggplot(df_resamples, aes(x = x)) + 
  geom_histogram() + 
  transition_manual(frame)
