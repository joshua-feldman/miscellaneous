library(tidyverse)
library(lubridate)
library(ggrepel)

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
        plot.subtitle = element_text(size = 14),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#1a1a1a"),
        text = element_text(color = "white", family = "Raleway"))

theme_set(th)

df <- read.csv("https://assets.datacamp.com/production/repositories/1934/datasets/1955897055a9136a66544aa93fbb0442bd5afcf2/gallup_approval_polls.csv")

df_new <- df %>% 
  select(Date, President, Approve, Days) %>% 
  mutate(Date = mdy(Date)) %>% 
  arrange(Date) %>% 
  group_by(President) %>% 
  filter(Days == min(Days) | Days == max(Days)) %>% 
  mutate(Term = row_number()) %>% 
  ungroup() %>% 
  mutate(label = paste0(President, " (", str_sub(Date, 1, 4), ")"))

df_new <- df_new[2:(nrow(df_new)-1),]

old_label <- df_new$label[df_new$Term==2]
old <- df_new$Approve[df_new$Term==2]

new_label <- df_new$label[df_new$Term==1]
new <- df_new$Approve[df_new$Term==1]

final <- data.frame(old_label, old, new_label, new) %>% 
  mutate(increase = ifelse(new > old, 1, 0)) %>% 
  mutate(gap = new - old) %>% 
  mutate(old_type = ifelse(str_detect(old_label, "Bush|Eisenhower|Ford|Nixon|Reagan"), "Republican", "Democratic")) %>% 
  mutate(new_type = ifelse(str_detect(new_label, "Bush|Eisenhower|Ford|Nixon|Reagan"), "Republican", "Democratic")) %>% 
  mutate(type = paste0(old_type, " to ", new_type))

final %>% 
  ggplot() +
  geom_segment(aes(x = 1, xend = 2, y = old, yend = new, col = as.factor(increase)),
               size = .75, show.legend = F) +
  geom_text_repel(x = rep(1, nrow(final)), y = final$old, label = final$old_label, hjust = 1.1, size = 3.8,
                  col = "white", family = "Raleway") +
  geom_text_repel(x = rep(2, nrow(final)), y = final$new, label = final$new_label, hjust = -0.1, size = 3.8,
                  col = "white", family = "Raleway") +
  # geom_text(label="OUTGOING\nPRESIDENT", x=0.6, y=mean(final1$old), size=6,
  #           col = "white", family = "Raleway") +
  # geom_text(label="INCOMING\nPRESIDENT", x=2.4, y=mean(final1$new), size=6,
  #           col = "white", family = "Raleway") +
  labs(title = "Presidential approval ratings between transitions of power",
       subtitle = "Since the 1940s, only three Presidents have begun their tenure with a lower approval rating than their predecessors –\nGeorge H. W. Bush (1989), George W. Bush (2001) and Donald Trump (2017). There has never been a decline in approval\nrating when going from a Republican to a Democrat.",
       y = "Approval rating",
       caption = "Source: Gallup Approval Polls\nGraphic: @JoshuaFeldman") +
  xlim(.5, 2.5) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.subtitle = element_text(margin = margin(0, 0, 15, 0))) +
  facet_wrap(~type) +
  scale_color_manual(values = c("#dd0808", "#0add08"))
