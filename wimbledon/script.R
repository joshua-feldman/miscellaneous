library(tidyverse)
library(rvest)
library(countrycode)
library(ggflags)
library(ggtext)
library(patchwork)
library(glue)

url_men <- "https://en.wikipedia.org/wiki/List_of_Wimbledon_gentlemen%27s_singles_champions"
url_women <- "https://en.wikipedia.org/wiki/List_of_Wimbledon_ladies%27_singles_champions"

html_men <- read_html(url_men)
html_women <- read_html(url_women)

df_men_amateur <- html_table(html_men)[[3]][,1:3]
df_women_amateur <- html_table(html_women)[[3]][,1:3]

df_men <- html_table(html_men)[[4]][,1:3]
df_women <- html_table(html_women)[[4]][,1:3]

df_men_new <- df_men %>%
  union_all(df_men_amateur) %>% 
  mutate(Country = str_sub(Country, 1, 3)) %>% 
  mutate(country = tolower(countrycode(Country, origin = "wb", destination = "iso2c"))) %>% 
  mutate(country = ifelse(Country == "TCH", "cz",
                   ifelse(Country == "FRG", "de",
                   ifelse(Country == "GER", "de",
                   ifelse(Country == "NED", "nl",
                   ifelse(Country == "SUI", "ch", 
                   ifelse(Country == "CRO", "hr", 
                   ifelse(Country == "NO ", "no",
                   ifelse(Country == "BRI", "gb", country))))))))) %>% 
  filter(`Year[d]` >= 1884 & `Year[d]` < 2020) %>% 
  mutate(decade = paste0(`Year[d]` - `Year[d]` %% 10, "s")) %>% 
  mutate(y = str_sub(`Year[d]`, 4, 4)) %>% 
  mutate(x = as.numeric(factor(decade))) %>% 
  mutate(y = as.numeric(y)) %>%
  select(-Country)

colnames(df_men_new) <- c("year", "champion", "country", "decade", "y", "x")
colnames(df_men_new) <- paste0(colnames(df_men_new), "_men")

df_women_new <- df_women %>%
  union_all(df_women_amateur) %>% 
  mutate(Country = str_sub(Country, 1, 3)) %>% 
  mutate(country = tolower(countrycode(Country, origin = "wb", destination = "iso2c"))) %>% 
  mutate(country = ifelse(Country == "TCH", "cz",
                          ifelse(Country == "FRG", "de",
                                 ifelse(Country == "GER", "de",
                                        ifelse(Country == "NED", "nl",
                                               ifelse(Country == "SUI", "ch", 
                                                      ifelse(Country == "CRO", "hr", 
                                                             ifelse(Country == "NO ", "no",
                                                                    ifelse(Country == "BRI", "gb", country))))))))) %>% 
  filter(`Year[d]` >= 1884 & `Year[d]` < 2020) %>% 
  mutate(decade = paste0(`Year[d]` - `Year[d]` %% 10, "s")) %>% 
  mutate(y = str_sub(`Year[d]`, 4, 4)) %>% 
  mutate(x = as.numeric(factor(decade))) %>% 
  mutate(y = as.numeric(y)) %>%
  select(-Country)

colnames(df_women_new) <- c("year", "champion", "country", "decade", "y", "x")
colnames(df_women_new) <- paste0(colnames(df_women_new), "_women")

df_new <- df_men_new %>% 
  left_join(df_women_new, by = c("year_men" = "year_women"))

background_color <- alpha("#2e2e2e", 1)

legend_df <- data.frame(x = c(-1, 1), y = c(0, 0), country = c("rs", "ro"))

legend_plot <- legend_df %>% 
  ggplot(aes(x, y, country = country)) +
  # geom_point(shape = 21, size = 30, color = "white", stroke = 3) +
  geom_flag(size = 16) +
  geom_richtext(x = -7, y = 0, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), color = "white", family = "Roboto",
                label = "The flag on the <b>LEFT</b> represents the home<br>country of the <b>GENTLEMEN'S</b> singles champion<br>in a given year, e.g. Serbia (Djokovic) in 2019") +
  geom_richtext(x = 7, y = 0, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt"), color = "white", family = "Roboto",
                label = "The flag on the <b>RIGHT</b> represents the home<br>country of the <b>LADIES'</b> singles champion<br>in a given year, e.g. Romania (Halep) in 2019") +
  # annotate("text", label = glue("The flag on the <b>LEFT</b> represents the home\ncountry of the <b>GENTLEMEN'S</b> singles champion\nin a given year, e.g. Serbia in 2019"),
  #          x = -6, y = 0, color = "white", family = "Roboto Light") + 
  # annotate("text", label = glue("The flag on the <b>RIGHT</b> represents the home\ncountry of the <b>LADIES'</b> singles champion\nin a given year, e.g. Romania in 2019"),
  #          x = 6, y = 0, color = "white", family = "Roboto Light") + 
  annotate("text", label = "2019",
           x = 0, y = -0.7, color = "white", family = "Luminari", size = 4.8) + 
  scale_x_continuous(limits = c(-20, 20)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_void() +
  theme(panel.background = element_rect(fill = background_color, color = background_color))

main_plot <- df_new %>% 
  mutate(label = ifelse(!is.na(country_men), year_men, NA)) %>% 
  ggplot(aes(x = x_men, y = y_men, label = label)) +
  
  # SHADE THE AMATEUR ERA
  annotate("rect", xmin = 0.5, xmax = 8.5, ymin = -0.5, ymax = 10, fill = "#404040") +
  annotate("rect", xmin = 8.5, xmax = 9.5, ymin = -0.5, ymax = 7.5, fill = "#404040") +
  
  # SHADE THE OPEN ERA
  annotate("rect", xmin = 8.5, xmax = 9.5, ymin = 7.5, ymax = 10, fill = "#595959") +
  annotate("rect", xmin = 9.5, xmax = 14.5, ymin = -0.5, ymax = 10, fill = "#595959") +
  
  geom_flag(data = df_men_new, aes(x = x_men, y = y_men, country = country_men), size = 10, inherit.aes = FALSE,
            position = position_nudge(x = -0.15)) + 
  geom_flag(data = df_women_new, aes(x = x_women, y = y_women, country = country_women), size = 10, inherit.aes = FALSE,
            position = position_nudge(x = 0.15)) + 
  
  geom_text(color = "white", family = "Luminari", size = 3, vjust = 3.2) +
  annotate("line", y = 0, ymax = 7, x = 7, xmax = 7, color = "white") +
  labs(title = "THE HOME COUNTRY OF WIMBLEDON CHAMPIONS",
       # subtitle = str_wrap("The USA have dominated the Open Era, taking home 15 Gentlemen's singles championships and 29 Ladies' singles championships since 1968 - thanks to superstar American players like Martina Navratilova (8 championships), Serena Williams (7 championships) and Pete Sampras (7 championships).", 160),
       caption = "Graphic by Joshua Feldman") +
  scale_country() +
  scale_x_continuous(labels = levels(as.factor(df_new$decade_men)), 
                     breaks = 1:14,
                     position = "top") +
  scale_y_reverse(expand = c(0, 0)) +
  annotate("text", label = "WORLD WAR I", angle = 90, color = "white", family = "Montserrat", x = 4, y = 6.5, size = 6) +
  annotate("text", label = "WORLD WAR II", angle = 90, color = "white", family = "Montserrat", x = 7, y = 2.5, size = 6) +
  annotate("text", label = "Amateur Era", color = "white", family = "Times New Roman", fontface = "italic", x = 0.6, y = 9.75, size = 8, hjust = 0) +
  annotate("text", label = "Open Era", color = "white", family = "Times New Roman", fontface = "italic", x = 8.6, y = 9.75, size = 8, hjust = 0) +
  # annotation_custom(plot1, xmin = 10, xmax = 20, ymin = 20, ymax = 40) +
  guides(country = "none") +
  theme_minimal(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        # axis.ticks.x = element_line(color = "white"),
        axis.text.x = element_text(face = "bold", color = "white", family = "Montserrat", size = 14),
        text = element_text(color = "white", family = "Roboto"),
        plot.title = element_text(face = "bold", family = "Montserrat", hjust = 0.5, size = 30, margin = margin(10, 0, 100, 0)),
        # plot.subtitle = element_text(family = "Roboto", hjust = 0.5, margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(family = "Montserrat", hjust = 0.5, margin = margin(20, 0, 00, 0)),
        plot.background = element_rect(fill = background_color, color = NA))

main_plot + inset_element(legend_plot, left = 0.1, bottom = 0.83, right = 0.9, top = 0.95, align_to = 'full')
