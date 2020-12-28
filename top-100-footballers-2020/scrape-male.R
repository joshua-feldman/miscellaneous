library(tidyverse)
library(rvest)

url <- "https://interactive.guim.co.uk/docsdata/1vK8-O66EQDxE6yyQM0NEt530ZuADIIAabGur5Ee9_Rs.json"

df <- jsonlite::fromJSON(url) %>% as.data.frame()
colnames(df) <- str_remove(colnames(df), "sheets.?(players|furniture).")

df_reduced <- df[,c(1:10, 19:20)]

df_reduced <- df_reduced %>% 
  mutate(Rank = as.numeric(Rank),
         Age.on.20.Dec.2020 = as.numeric(Age.on.20.Dec.2020))

write.csv(df_reduced, "2020-top-100-male.csv", row.names=FALSE)
