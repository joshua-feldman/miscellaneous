library(rvest)
library(tidyverse)

bbc_women <- read_html("https://www.bbc.com/news/world-59514598")

# Save even and odd indices for data extraction later

odd_index <- seq(1,200,2)
even_index <- seq(2,200,2)

# Extract name

name <- bbc_women %>% 
  html_nodes("article h4") %>% 
  html_text()

# Extract image

img <- bbc_women %>% 
  html_nodes(".card__header") %>% 
  html_nodes("img") %>% 
  html_attr("src")

img <- img[odd_index]

# Extract category

category <- bbc_women %>% 
  html_nodes("article .card") %>% 
  str_extract("card category--[A-Z][a-z].*data-id") %>% 
  str_remove_all("card category--") %>% 
  str_remove_all("Afghanistan,") %>% 
  str_remove_all("data-id") %>% 
  str_replace_all("&amp;", "&") %>% 
  str_remove_all("\" ")

# Extract country & role

country_role <- bbc_women %>% 
  html_nodes(".card__header__strapline__location") %>% 
  html_text()

country <- country_role[odd_index]
role <- country_role[even_index]

country <- ifelse(str_detect(country, "Iran"), "Iran", country)

# Extract description

description <- bbc_women %>% 
  html_nodes(".first_paragraph") %>% 
  html_text()

# Finalise data frame

df <- data.frame(
  name,
  img,
  category,
  country,
  role,
  description
)

# Export

write.csv(df, "2021-data.csv", row.names=FALSE)