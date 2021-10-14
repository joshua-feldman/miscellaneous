library(tidyverse)
library(rvest)

main_color <- "#2e2e2e"

th <- theme_minimal(base_size = 18) +
  theme(text = element_text(family = "Raleway", color = "white"),
        panel.background = element_rect(fill = main_color, color = main_color),
        plot.title = element_text(face = "bold", color = "white"),
        plot.background = element_rect(fill = main_color, color = main_color),
        strip.text = element_text(face = "bold", color = "white"),
        axis.text = element_text(color = "white"),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        legend.position = 'top',
        plot.margin = margin(20, 20, 20, 20),
        plot.subtitle = element_text(size = 16),
        panel.grid.major = element_line(color = "#595959"),
        panel.grid.minor = element_line(color = "#595959"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), color = "white"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), color = "white"))

theme_set(th)

url <- "https://en.wikipedia.org/wiki/List_of_James_Bond_films"
html <- read_html(url)

df <- html %>% 
  html_table() %>% 
  .[[1]]

df <- df[2:26, c(1:3, 8)]

# Ratings retrieved from here on 13 Oct 2021: https://www.imdb.com/list/ls006405458/
df$ratings <- c(7.2, 7.4, 7.7, 7, 6.9, 6.7, 6.6, 6.8, 6.7, 7.1, 6.3, 6.7, 6.5, 6.3, 6.7, 6.6, 7.2, 6.5, 6.4, 6.1, 8, 6.6, 7.8, 6.8, 7.6)

mean_ratings <- df %>% 
  group_by(`Bond actor`) %>% 
  summarise(mean_ratings = mean(ratings)) %>% 
  ungroup()

df_new <- df %>% 
  left_join(mean_ratings) %>% 
  mutate(index = row_number()) %>% 
  mutate(pos = ifelse(ratings > mean_ratings, Title, NA)) %>% 
  mutate(neg = ifelse(ratings < mean_ratings, Title, NA)) %>% 
  mutate(neu = ifelse(ratings == mean_ratings, Title, NA)) %>%
  group_by(`Bond actor`) %>% 
  mutate(index_mean = mean(index)) %>% 
  mutate(actor_label = ifelse(Year == min(Year), `Bond actor`, NA)) %>% 
  ungroup() %>% 
  mutate(char = 1 / nchar(Title))

df_new %>% 
  ggplot() +
  # geom_hline(yintercept = median(df_new$ratings), linetype = "dashed") +
  geom_point(aes(index, ratings, color = `Bond actor`), size = 2) +
  geom_line(aes(index, mean_ratings, color = `Bond actor`), lwd = 1) +
  geom_segment(aes(x = index, xend = index, y = mean_ratings, yend = ratings, color = `Bond actor`)) +
  geom_text(aes(index, ratings, label = pos, color = `Bond actor`, size = char), family = "Roboto", vjust = -1) +
  geom_text(aes(index, ratings, label = neg, color = `Bond actor`, size = char), family = "Roboto", vjust = 2) +
  geom_text(aes(index, ratings, label = neu, color = `Bond actor`, size = char), family = "Roboto", vjust = 2) +
  geom_label(aes(index_mean, mean_ratings, label = toupper(actor_label), color = `Bond actor`),
            label.size = NA, fontface = "bold", size = 5, vjust = -0.1, fill = "white", alpha = 0.1, family = "Raleway") +
  labs(title = "IMDb ratings of James Bond films by lead actor",
       subtitle = "On average, <span style='color:#66C2A5'>Bond films featuring Daniel Craig</span> are more critically acclaimed than those featuring any other actor, thanks to successful<br>iterations like *No Time to Die*, *Skyfall* and *Casino Royale* – seemingly the best Bond film of all time.",
       y = "IMDb rating",
       caption = "Graphic: Joshua Feldman") +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(clip = "off") +
  guides(color = FALSE, size = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.subtitle = ggtext::element_markdown(),
        legend.position = "bottom") +
  scale_size(range = c(1.75, 4))

