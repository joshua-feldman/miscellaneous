library(tidyverse)
library(wikipediatrend)

setwd("~/GitHub/wikipedia")

greek_letters <- c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa",
                   "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", "sigma", "tau", "upsilon", "phi",
                   "chi", "psi", "omega")

retrieve_avg_views <- function() {
  
  views <- vector()
  
  for(i in 1:length(greek_letters)) {
    
    df <- wikipediatrend::wp_trend(page = greek_letters[i])
    views <- append(views, median(df$views))
    
  }
  
  return(views)
  
}

avg_views <- retrieve_avg_views()

retrieve_greek_lettering <- function() {
  
  lc_letters <- vector()
  uc_letters <- vector()
  
  for(i in 1:length(greek_letters)) {
    
    lc_letter <- greekLetters::greeks(greek_letters[i])
    lc_letters <- append(lc_letters, lc_letter)
    
    uc_letter <- greekLetters::greeks(stringr::str_to_title(greek_letters[i]))
    uc_letters <- append(uc_letters, uc_letter)
    
  }
  
  lc_letters <<- lc_letters
  uc_letters <<- uc_letters
  
}

retrieve_greek_lettering()

df <- data.frame(greek_letters, avg_views) %>% 
  mutate(x_index = rep(1:6, 4),
         y_index = c(rep(4, 6), rep(3, 6), rep(2, 6), rep(1, 6)),
         uc = uc_letters,
         lc = lc_letters,
         greek_label = paste0(uc, " ", lc),
         page_views = paste0("Avg views: ", scales::comma(avg_views, accuracy = 1)),
         pi = as.factor(ifelse(greek_letters == "pi", 1, 0)))

df %>% 
  ggplot(aes(x_index, y_index)) +
  geom_tile(aes(fill = avg_views), width = 0.85, height = 0.85) +
  geom_text(aes(col = pi), label = str_to_upper(greek_letters), vjust = -3, family = "GFS Didot", size = 4) +
  geom_text(aes(label = greek_label, col = pi), size = 9, family = "GFS Didot") +
  geom_text(aes(label = page_views, col = pi), vjust = 4, size = 3.9, family = "GFS Didot") +
  theme_minimal(base_size  = 16) +
  labs(title = "The Alpha to Omega of Wikipedia",
       subtitle = "This graphic shows the daily median of Wikipedia page views for each Greek letter since\n10 December 2007. Pi is the most searched-for letter, with an average of 6,148 views per day.",
       caption = "Graphic: Joshua Feldman") +
  guides(fill = FALSE, color = FALSE) +
  scale_color_manual(values = c("black", "white")) +
  scale_fill_gradient(low = alpha("#0d5eaf", 0), high = alpha("#0d5eaf", 1)) +
  theme(plot.background = element_rect(fill = alpha("#0d5eaf", 0.05), color = alpha("#0d5eaf", 0.05)),
        text = element_text(family = "GFS Neohellenic"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 32, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(hjust = 0.5, margin = margin(10, 0, 0, 0)),
        plot.caption = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

ggsave("graphic.png", width = 10, height = 10)

