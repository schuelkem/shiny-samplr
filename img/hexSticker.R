library(dplyr)
library(ggplot2)
library(hexSticker)

ggplot() + 
  geom_area(aes(x = x, y = y), tibble(x = seq(-4, -3, .01), y = dnorm(x)), fill = "#ffffff") + 
  geom_area(aes(x = x, y = y), tibble(x = seq(-3, -2, .01), y = dnorm(x)), fill = "#8fd6bd") + 
  geom_area(aes(x = x, y = y), tibble(x = seq(-2, -1, .01), y = dnorm(x)), fill = "#5bc2e7") + 
  geom_area(aes(x = x, y = y), tibble(x = seq(-1,  1, .01), y = dnorm(x)), fill = "#003da5") + 
  geom_area(aes(x = x, y = y), tibble(x = seq( 1,  2, .01), y = dnorm(x)), fill = "#5bc2e7") + 
  geom_area(aes(x = x, y = y), tibble(x = seq( 2,  3, .01), y = dnorm(x)), fill = "#8fd6bd") + 
  geom_area(aes(x = x, y = y), tibble(x = seq( 3,  4, .01), y = dnorm(x)), fill = "#ffffff") + 
  geom_line(aes(x = x, y = y), tibble(x = seq(-4,  4, .01), y = dnorm(x))) + 
  theme_void() -> p

sysfonts::font_add("Brandon Grotesque", "C:/Windows/Fonts/HVD Fonts - BrandonGrotesque-Regular_0.otf")
sysfonts::font_add_google("Crimson Text", "Crimson Text")
showtext_auto()

sticker(p, 
        package = "Shiny Samplr",
        p_size = 18,
        p_y = 0.7, 
        p_color = "#003da5", 
        p_family = "Brandon Grotesque", 
        s_x = 1,
        s_y = 1.3,
        s_width = 1.1,
        s_height = 0.8,
        filename = "img/hexSticker.png",
        h_fill = NA,
        h_color = "#003da5",
        url = "github.com/schuelkem/shiny-samplr",
        u_color = "#003da5",
        u_size = 4.75, 
        u_family = "Crimson Text")
