library(tidyverse)
library(ggtext)

th <- theme_dark(base_size = 18) +
  theme(axis.text = element_text(color = "white", family = "Raleway"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), face = "bold"),
        legend.background = element_rect(fill = "#262626", color = "#262626"),
        legend.key = element_rect(fill = "#262626", color = "#262626"),
        panel.background = element_rect(fill = "#262626", color = "#262626"),
        panel.grid.major = element_line(color = "#404040", size = rel(0.5)),
        panel.grid.minor = element_line(color = "#404040", size = rel(0.25)),
        plot.background = element_rect(fill = "#262626", color = "#262626"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#1a1a1a"),
        text = element_text(color = "white", family = "Raleway"))

theme_set(th)

color_hex <- RColorBrewer::brewer.pal(4, "Set3")

df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

df_new <- df %>%
  filter(!state %in% c("Guam", "Virgin Islands", "Northern Mariana Islands", "Puerto Rico")) %>% 
  group_by(state) %>% 
  arrange(date) %>% 
  mutate(new_cases = cases - lag(cases)) %>% 
  mutate(new_cases_roll7 = zoo::rollmean(new_cases, 7, na.pad=TRUE, align = "right")) %>% 
  ungroup() %>% 
  mutate(color = ifelse(state == "Florida", color_hex[1],
                 ifelse(state == "California", color_hex[2],
                 ifelse(state == "Texas", color_hex[3],
                 ifelse(state == "New York", color_hex[4], "grey")))))
 
df_new %>% 
  filter(date >= '2020-03-15') %>% 
  mutate(flag = ifelse(state %in% c("Florida", "California", "Texas", "New York"),
                       state, "z_other")) %>% 
  mutate(flag_binary = ifelse(state %in% c("Florida", "California", "Texas", "New York"),
                       1, 0)) %>% 
  ggplot(aes(date, new_cases_roll7, col = flag, group = state, alpha = flag_binary)) +
  geom_line(lwd = 0.8) +
  labs(title = "New cases of COVID-19 in the United States",
       subtitle = "Three states are driving the surge: <b style='color:#FFFFB3'>Florida</b>, <b style='color:#FB8072'>Texas</b> and <b style='color:#8DD3C7'>California</b>. Meanwhile, <b style='color:#BEBADA'>New York</b> – once<br>the epicentre of the virus – is now registering fewer than 1k cases per day.",
       x = "Date",
       y = "New cases (7-day rolling average)",
       caption = "Graphic: @JoshuaFeldman") +
  scale_color_manual(values = c(color_hex, "grey")) +
  theme(legend.position = "top",
        plot.subtitle = element_markdown()) +
  scale_y_continuous(labels = scales::comma) +
  guides(alpha = FALSE, col = FALSE) +
  annotate("text", x = as.Date("2020-07-16"), y = 11865.286, label = "Florida", color = "#FFFFB3",
           fontface = "bold", family = "Raleway", size = 4, vjust = -0.25) +
  annotate("text", x = as.Date("2020-07-16"), y = 10213.000, label = "Texas", color = "#FB8072",
           fontface = "bold", family = "Raleway", size = 4, vjust = -0.25) +
  annotate("text", x = as.Date("2020-07-16"), y = 8749.286, label = "California", color = "#8DD3C7",
           fontface = "bold", family = "Raleway", size = 4, vjust = -0.25) +
  annotate("text", x = as.Date("2020-04-10"), y = 10250, label = "New York", color = "#BEBADA",
           fontface = "bold", family = "Raleway", size = 4)