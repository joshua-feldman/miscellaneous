# Load packages

library(rvest)
library(tidyverse)

card__header__strapline__instagram

# Load web page

bbc_women <- html("https://www.bbc.co.uk/news/world-55042935")

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
  str_extract("card category--[A-Z][a-z]+") %>% 
  str_remove_all("card category--")

# Extract country & role

country_role <- bbc_women %>% 
  html_nodes(".card__header__strapline__location") %>% 
  html_text()

country <- country_role[odd_index]
role <- country_role[even_index]

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

write.csv(df, "data.csv", row.names=FALSE)
