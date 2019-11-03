library(tidyverse)
library(ggplot2)
library(ggforce)
library(gganimate)
library(RColorBrewer)

cols <- brewer.pal(8, "Set3")

t <- seq(pi, pi * 3, by = 0.1)

df <- data.frame(
  index = t,
  x1 = sin(t),
  y1 = 0,
  x2 = 0,
  y2 = cos(t),
  x3 = sin(45 * pi/180) * sin(t + 1 * pi/4),
  y3 = cos(45 * pi/180) * sin(t + 1 * pi/4),
  x4 = -sin(45 * pi/180) * sin(t + 3 * pi/4),
  y4 = cos(45 * pi/180) * sin(t + 3 * pi/4),
  x5 = sin(22.5 * pi/180) * sin(t + 1.5 * pi / 4),
  y5 = cos(22.5 * pi/180) * sin(t+ 1.5 * pi / 4),
  x6 = -sin(22.5 * pi/180) * sin(t + 2.5 * pi/4),
  y6 = cos(22.5 * pi/180) * sin(t + 2.5 * pi/4),
  x7 = sin(67.5 * pi/180) * sin(t + 0.5 * pi/4),
  y7 = cos(67.5 * pi/180) * sin(t + 0.5 * pi/4),
  x8 = -sin(67.5 * pi/180) * sin(t + 3.5 * pi/4),
  y8 = cos(67.5 * pi/180) * sin(t + 3.5 * pi/4)
)

p <- df %>% 
  ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), fill = "#666666") +
  annotate("line", x = df$x1, y = df$y1, col = cols[1]) +
  annotate("line", x = df$x2, y = df$y2, col = cols[2]) +
  annotate("line", x = df$x3, y = df$y3, col = cols[3]) +
  annotate("line", x = df$x4, y = df$y4, col = cols[4]) +
  annotate("line", x = df$x5, y = df$y5, col = cols[5]) +
  annotate("line", x = df$x6, y = df$y6, col = cols[6]) +
  annotate("line", x = df$x7, y = df$y7, col = cols[7]) +
  annotate("line", x = df$x8, y = df$y8, col = cols[8]) +
  geom_point(aes(x = x1, y = y1), size = 8, col = cols[1]) +
  geom_point(aes(x = x2, y = y2), size = 8, col = cols[2]) +
  geom_point(aes(x = x3, y = y3), size = 8, col = cols[3]) +
  geom_point(aes(x = x4, y = y4), size = 8, col = cols[4]) +
  geom_point(aes(x = x5, y = y5), size = 8, col = cols[5]) +
  geom_point(aes(x = x6, y = y6), size = 8, col = cols[6]) +
  geom_point(aes(x = x7, y = y7), size = 8, col = cols[7]) +
  geom_point(aes(x = x8, y = y8), size = 8, col = cols[8]) +
  labs(x = NULL, y = NULL) +
  transition_layers(from_blank = FALSE) +
  enter_fade() + enter_grow() +
  transition_time(index) +
  ease_aes() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#4d4d4d"))

animate(p, fps = 20, nframes = nrow(df))