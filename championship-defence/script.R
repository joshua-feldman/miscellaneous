library(tidyverse)
library(ggtext)
library(glue)

main_color <- "#2e2e2e"

th <- theme_minimal(base_size = 18) +
  theme(text = element_text(family = "Roboto", color = "white"),
        panel.background = element_rect(fill = main_color, color = main_color),
        plot.title = element_text(face = "bold", color = "white"),
        plot.background = element_rect(fill = main_color, color = main_color),
        strip.text = element_text(face = "bold", color = "white"),
        axis.text = element_text(color = "white"),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        legend.position = 'top',
        plot.margin = margin(25, 25, 10, 25),
        panel.grid.major = element_line(color = "#595959"),
        panel.grid.minor = element_line(color = "#595959"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), color = "white"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), color = "white"))

theme_set(th)

setwd("~/GitHub/football")

df <- read_csv("prem-tables.csv")

df_mod <- df %>% 
  mutate(champions = coalesce(ifelse(str_detect(Team, "\\(C\\)"), 1, 0), 0)) %>% 
  mutate(Team = str_remove_all(Team, " \\(C\\)")) %>% 
  group_by(Team) %>% 
  arrange(Season, by_group = TRUE) %>% 
  mutate(champions_prev = coalesce(ifelse(lag(champions) == 1, 1, 0), 0)) %>% 
  filter(champions == 1 | champions_prev == 1) %>% 
  select(Season, Team, Pts, champions_prev) %>% 
  mutate(Pts_Prev = lag(Pts)) %>% 
  mutate(Season_Prev = Season - 1) %>% 
  filter(champions_prev == 1) %>% 
  mutate(diff = Pts - Pts_Prev) %>% 
  mutate(Season_Team = paste0(Season, ": ", Team)) %>% 
  mutate(color = ifelse(diff > 0, "green", ifelse(diff < 0, "red", NA))) %>% 
  mutate(color_text = ifelse(diff > 0, "green", ifelse(diff < 0, "red", "white"))) %>% 
  mutate(linetype = ifelse(Team == "Liverpool", "normal", "dashed")) %>% 
  mutate(label_left = paste0(Season_Prev, "/", str_sub(Season, -2, -1))) %>% 
  mutate(label_right = paste0(Season, "/", str_sub(Season + 1, -2, -1)))

df_mod %>% 
  ggplot(aes(color = color)) +
  geom_segment(aes(xend = Pts, x = Pts_Prev, y = Season_Team, yend = Season_Team),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"), lwd = 1) +
  geom_point(aes(x = Pts_Prev, y = Season_Team), color = "white", size = 2) +
  geom_text(aes(x = Pts_Prev, y = Season_Team, label = label_left), color = "white",
            hjust = ifelse(df_mod$Pts >= df_mod$Pts_Prev, 1, 0),
            nudge_x = ifelse(df_mod$Pts >= df_mod$Pts_Prev, -0.5, 0.5)) +
  geom_text(aes(x = Pts, y = Season_Team, label = label_right, color = color_text),
            hjust = ifelse(df_mod$Pts >= df_mod$Pts_Prev, 0, 1),
            nudge_x = ifelse(df_mod$Pts >= df_mod$Pts_Prev, 0.5, -0.5)) +
  labs(x = "Points",
       y = "Championship-winning team") +
  scale_color_manual(values = c("#71CA97", "#ff7f7f", "grey")) +
  guides(color = FALSE, linetype = FALSE) +
  scale_x_continuous(limits = c(min(c(df_mod$Pts, df_mod$Pts_Prev)) - 5, max(c(df_mod$Pts, df_mod$Pts_Prev)) + 5)) +
  labs(title = "Points from championship-winning and championship-defending seasons",
       subtitle = str_wrap("Liverpool earned 30 fewer points in their championship-defending season – the third steepest decline in Premier League history. Only Chelsea (–37 points, 2015/16) and Leicester (–37 points, 2016/17) have fared worse.", 130),
       x = "Points",
       y = "Championship-winning team",
       caption = "Graphic: Joshua Feldman") +
  theme(plot.subtitle = element_text(margin = margin(0, 0, 20, 0)))
