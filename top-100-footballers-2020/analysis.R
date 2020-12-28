library(tidyverse)
library(rvest)
library(waffle)
library(hrbrthemes)
library(countrycode)

th <- theme_dark(base_size = 18) +
  theme(axis.text = element_text(color = "white", family = "Raleway"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), face = "bold"),
        legend.background = element_rect(fill = "#4d4d4d", color = "#4d4d4d"),
        legend.key = element_rect(fill = "#4d4d4d", color = "#4d4d4d"),
        legend.title = element_text(face = "bold"),
        panel.background = element_rect(fill = "#4d4d4d", color = "#4d4d4d"),
        panel.grid.major = element_line(color = "#666666", size = rel(0.5)),
        panel.grid.minor = element_line(color = "#666666", size = rel(0.25)),
        plot.background = element_rect(fill = "#4d4d4d", color = "#4d4d4d"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        text = element_text(color = "white", family = "Raleway"))

theme_set(th)

df <- read_csv("2020-top-100-male.csv")

df$nationality_continent <- countrycode(sourcevar = df[, "Nationality"],
                                                origin = "country.name",
                                                destination = "continent")

df <- df %>% 
  mutate(nationality_continent = ifelse(is.na(nationality_continent), "Europe", nationality_continent)) %>% 
  mutate(Position.modifier = factor(Position.modifier, levels = c("RIGHT", "CENTRE", "LEFT"))) %>% 
  mutate(Position = factor(Position, levels = c("Goalkeeper", "Defender", "Midfielder", "Winger", "Forward")))

# Age distribution

wingers <- df %>% filter(Position == "Winger")
goalkeepers <- df %>% filter(Position == "Goalkeeper")

ggplot() +
  geom_density(data = df, aes(Age.on.20.Dec.2020), lwd = 1, color = brewer.pal(3, "Set2")[1]) +
  geom_density(data = wingers, aes(Age.on.20.Dec.2020), lwd = 1, color = brewer.pal(3, "Set2")[2])

df %>% 
  ggplot(aes(Age.on.20.Dec.2020, color = Position)) +
  geom_density() +
  labs(title = "Age distribution of footballers in the Guardian's Top 100",
       subtitle = "The average age is 27.25 years old. The oldest footballer to be included\nis Zlatan Ibrahimovic (39 years old).",
       x = "Age",
       y = "n") +
  scale_color_brewer(palette = "Set2")

df %>% 
  count(Position, Age.on.20.Dec.2020) %>% 
  ggplot(aes(Age.on.20.Dec.2020, n, fill = Position)) +
  geom_col(position = "stack") +
  labs(title = "Age distribution of footballers in the Guardian's Top 100",
       subtitle = "The average age is 27.25 years old. The oldest footballer to be included\nis Zlatan Ibrahimovic (39 years old).",
       x = "Age",
       y = "n") +
  scale_y_continuous(breaks=c(2,4,6,8, 10, 12)) +
  scale_fill_brewer(palette = "Set2")

# Nationality distribution
df %>% 
  count(Position, nationality_continent) %>% 
  ggplot(aes(fill = Position, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~nationality_continent, nrow = 1, strip.position = "bottom") +
  scale_x_discrete()


# Nationality distribution [Waffle chart of flags]  

df %>% 
  count(nationality_continent) %>% 
  ggplot(aes(label = nationality_continent, values = n))  +
  geom_pictogram(aes(color = nationality_continent), n_rows = 10, flip = TRUE, make_proportional = TRUE) +
  scale_color_manual(
    name = NULL,
    values = RColorBrewer::brewer.pal(4, "Set2"),
    labels = c("Africa", "Americas", "Asia", "Europe")
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("globe-africa", "globe-americas", "globe-asia", "globe-europe"),
    labels = c("Africa", "Americas", "Asia", "Europe")
  ) +
  labs(title = "Nationality distribution",
       subtitle = "Two thirds of the top 100 consists of players from Europe. There is only one player\nfrom Asia (Son Heung-min, Tottenham Hotspur).") +
  coord_fixed() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_blank())

# Position distribution [Beeswarm]

sort(table(df$Position))

set.seed(1234)

df %>% 
  mutate(label = ifelse(Rank <= 10, Name, NA)) %>% 
  ggplot(aes(Position, Position.modifier, col = Position, size = 100 - Rank)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.5) +
  geom_text(aes(label = label), position=position_jitter(width=0.3, height=0.3, seed=1234)) +
  labs(title = "Position of footballers in the Guardian's Top 100",
       subtitle = "Out of 100 footballers, there are 34 Forwards, 25 Midfielders, 18 Wingers, 16 Defenders and 7 Goalkeepers.") +
    guides(col = FALSE, size = FALSE)
