library(tidyverse)
library(rvest)
library(extrafont)
library(ggtext)
library(glue)

main_color <- "#2e2e2e"
palette <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")

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
        plot.margin = margin(20, 20, 20, 20),
        plot.subtitle = element_text(size = 16, margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(margin = margin(20, 0, 0, 0)),
        panel.grid.major = element_line(color = "#595959"),
        panel.grid.minor = element_line(color = "#595959"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), color = "white"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), color = "white"))

theme_set(th)

seinfeld <- "https://en.wikipedia.org/wiki/Seinfeld"
friends <- "https://en.wikipedia.org/wiki/Friends"
frasier <- "https://en.wikipedia.org/wiki/Frasier"

seinfeld_df <- read_html(seinfeld) %>% rvest::html_table() %>% pluck(3)
friends_df <- read_html(friends) %>% rvest::html_table() %>% pluck(2)
frasier_df <- read_html(frasier) %>% rvest::html_table() %>% pluck(2)

# Prepare Seinfeld data
colnames(seinfeld_df) <- seinfeld_df[1,]
seinfeld_df_new <- seinfeld_df[2:nrow(seinfeld_df), 2:ncol(seinfeld_df)] %>% 
  select(season = Season,
         first_aired = `First aired`,
         last_aired = `Last aired`,
         viewers = `Viewers (millions)`) %>% 
  mutate(first_aired = as.Date(str_extract(first_aired, "(?<=\\().*(?=\\))"))) %>% 
  mutate(last_aired = as.Date(str_extract(last_aired, "(?<=\\().*(?=\\))"))) %>% 
  mutate(season = as.numeric(season)) %>% 
  mutate(viewers = as.numeric(viewers)) %>% 
  mutate(show = "Seinfeld")

# Prepare Friends data
friends_df_new <- friends_df[2:nrow(friends_df), ] %>% 
  select(season = Season,
         first_aired = `Season premiere`,
         last_aired = `Season finale`,
         viewers = `Viewers(in millions)`) %>% 
  mutate(viewers = str_remove(viewers, "\\[105\\]")) %>% 
  mutate(first_aired = as.Date(first_aired, format = "%B %d, %Y")) %>% 
  mutate(last_aired = as.Date(last_aired, format = "%B %d, %Y")) %>% 
  mutate(season = as.numeric(season)) %>% 
  mutate(viewers = as.numeric(viewers)) %>% 
  mutate(show = "Friends")

# Prepare Frasier data
colnames(frasier_df) <- frasier_df[1,]
frasier_df_new <- frasier_df[2:nrow(frasier_df), 2:ncol(frasier_df)] %>% 
  select(season = Season,
         first_aired = `First aired`,
         last_aired = `Last aired`,
         viewers = `Viewers(millions)`) %>% 
  mutate(first_aired = as.Date(str_extract(first_aired, "(?<=\\().*(?=\\))"))) %>% 
  mutate(last_aired = as.Date(str_extract(last_aired, "(?<=\\().*(?=\\))"))) %>% 
  mutate(season = as.numeric(season)) %>% 
  mutate(viewers = as.numeric(viewers)) %>% 
  mutate(show = "Frasier")

# Combine datasets
df <- seinfeld_df_new %>% 
  rbind(friends_df_new) %>% 
  rbind(frasier_df_new) %>% 
  mutate(x_joining1 = ifelse(season %% 2 == 0, first_aired, last_aired)) %>% 
  mutate(x_joining1 = as.Date(x_joining1, origin = "1970-01-01")) %>% 
  mutate(x_joining2 = ifelse(season %% 2 == 0, last_aired, first_aired)) %>% 
  mutate(x_joining2 = as.Date(x_joining2, origin = "1970-01-01")) %>% 
  group_by(show) %>% 
  mutate(group1 = rep(season, length.out = n(), each = 2)) %>% 
  mutate(group1 = ifelse(show == "Friends", group1 + 10,
                 ifelse(show == "Frasier", group1 + 20, group1))) %>%
  mutate(group2 = ifelse(season %% 2 == 0, group1, group1 - 1)) %>% 
  ungroup() %>% 
  mutate(midway = as.Date((as.numeric(last_aired) + as.numeric(first_aired)) / 2, origin = '1970-01-01')) %>% 
  mutate(label = paste0("S", season)) %>% 
  mutate(label = ifelse(show == "Friends" & label == "S5", NA, label))

# Make labels
subtitle <- "<span style='color:#bc5090'>**Seinfeld**</span> became increasingly popular during its original run, with the final season attracting an average of 35.5M viewers per episode – up from 19.2M<br>in Season 1. <span style='color:#ff6361'>**Friends**</span> peaked in Season 2, while <span style='color:#ffa600'>**Frasier**</span> never bettered its first season's ratings."
label1 <- "<span style='color:#bc5090;'>**Seinfeld**</span> surged in popularity between Seasons<br>4 and 5 following a number of classic episodes<br>like The Contest, in which the characters compete<br>to see who can go the longest without masturbating."
label2 <- "<span style='color:#ff6361;'>**Friends**</span> peaked in Season 2, when Ross<br>and Rachel began perhaps the most famous<br>romance in sitcom history."
label3 <- "<span style='color:#ffa600;'>**Frasier**</span> surpassed Friends in Seasons 6 and 7<br>after taking Seinfeld's time slot. However, the<br>show's ratings then slipped, falling below 15M<br>in the final two seasons."

# Plot
df %>%
  ggplot(aes(first_aired, viewers, color = show, label = label)) +
  geom_errorbarh(aes(xmin = first_aired, xmax = last_aired), lwd = 1) +
  geom_line(aes(x = x_joining1, group = group1), lwd = 1) +
  geom_line(aes(x = x_joining2, group = group2), lwd = 1) +
  geom_text(aes(x = midway), vjust = -0.5, family = "Roboto", fontface = "bold", size = 4) +
  geom_curve(aes(x = as.Date("1993-07-01"), xend = as.Date("1991-07-01"),
                 y = 28, yend = 26.5),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             color = "white",
             ncp = 10) +
  geom_curve(aes(x = as.Date("1999-01-21"), xend = as.Date("1999-01-21"),
                 y = 22, yend = 17),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             color = "white",
             curvature = -0.1,
             ncp = 10) +
  geom_curve(aes(x = as.Date("1996-01-181"), xend = as.Date("1996-12-18"),
                 y = 31, yend = 30.5),
             arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
             color = "white",
             curvature = -0.5,
             ncp = 10) +
  scale_color_manual(values = palette[c(5, 4, 3)]) +
  annotate("richtext",  x = as.Date("1989-03-01"), y = 24.5, fill = NA, label.color = NA,
           label = label1, color = "white", family = "Roboto Light", hjust = 0) +
  annotate("richtext",  x = as.Date("1996-10-01"), y = 29, fill = NA, label.color = NA,
           label = label2, color = "white", family = "Roboto Light", hjust = 0) +
  annotate("richtext",  x = as.Date("1997-01-01"), y = 15, fill = NA, label.color = NA,
           label = label3, color = "white", family = "Roboto Light", hjust = 0) +
  annotate("text", label = "S5", x = as.Date("1999-01-21"), y = 23, color = palette[4],
           family = "Roboto", fontface = "bold", size = 4) +
  labs(title = "TV ratings of American 90s sitcoms",
       subtitle = subtitle,
       x = "Date",
       y = "Viewers",
       caption = "Graphic: Joshua Feldman\nSource: Nielsen Ratings (via Wikipedia)") +
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank(),
        plot.subtitle = element_markdown(lineheight = 1.2))
        
