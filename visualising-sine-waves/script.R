library(tidyverse)
library(gganimate)
library(scales)
library(grid)
library(latex2exp)
library(RColorBrewer)

source("theme.R")

x <- seq(-3 * pi, 3 * pi, 0.01)
sin_x <- sin(x)
cos_x <- cos(x)

df <- data.frame(x, sin_x, cos_x) %>% 
  gather(key, value, -x)

cols <- brewer.pal(3, "Set3")

x_breaks <- seq(from = -3 * pi, to = 3 * pi, by = pi/2)
x_labels <- expression(-3 * pi, -5 * pi /2, -2 * pi, -3 * pi /2, -pi, -pi /2,
                       0, pi /2, pi, 3 * pi /2, 2 * pi, 5 * pi /2, 3 * pi)

animation <- df %>% 
  ggplot(aes(x, value, col = key)) +
  geom_point() +
  labs(title = "Visualising sine and cosine waves using gganimate",
       subtitle = "A cosine wave is a sine wave with a phase-shift of π/2 radians. Because of this head start, it is often said that\nthe cosine function leads the sine function or that the sine lags the cosine.",
       x = "x",
       y = NULL,
       caption = "Graphic: Joshua Feldman") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  scale_color_brewer(palette = "Set3", labels = c("Cosine wave: cos(x)", "Sine wave: sin(x)")) +
  geom_hline(yintercept = 0, color = "white", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "white", linetype = "dashed") +
  # annotate("text", label = "Sine wave: sin(x)", col = cols[2], x = -3 * pi, y = 1,
  #          family = "Raleway", size = 4.5, fontface = "bold", hjust = 0) +
  # annotate("text", label = "Cosine wave: cos(x)", col = cols[1], x = -3 * pi, y = 0.9,
  #          family = "Raleway", size = 4.5, fontface = "bold", hjust = 0) +
  theme(legend.position = "top",
        plot.margin = unit(c(4, 1, 1, 1), "lines"),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.spacing.x = unit(0.3, 'cm')) +
  guides(colour = guide_legend(override.aes = list(size=8))) +
  transition_time(x) +
  ease_aes("linear") +
  shadow_mark(alpha = 0.5)

animate(animation, height = 800, width = 1000)
