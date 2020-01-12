library(tidyverse)
library(gganimate)

m <- matrix(nrow = 16, ncol = 16)
df <- as.data.frame(m)

for(x in 0:15) {
  for(y in 0:15) {
    df[x + 1, y + 1] <- bitwXor(x, y)
  }
}

colnames(df) <- 1:16

df_long <- df %>% 
  rownames_to_column() %>% 
  gather(key, value, -rowname) %>% 
  mutate(rowname = as.numeric(rowname)) %>% 
  mutate(key = as.numeric(key))

anim <- df_long %>% 
  ggplot(aes(rowname, key, fill = value)) +
  geom_tile() +
  guides(fill = FALSE) +
  scale_fill_gradient(high = "#006622", low = "#99ffbb") +
  theme_void() +
  theme(panel.background = element_rect(fill ="#4d4d4d")) +
  transition_filter(
    transition_length = 10,
    filter1 = value < 1,
    filter2 = value < 2,
    filter3 = value < 3,
    filter4 = value < 4,
    filter5 = value < 5,
    filter6 = value < 6,
    filter7 = value < 7,
    filter8 = value < 8,
    filter9 = value < 9,
    filter10 = value < 10,
    filter11 = value < 11,
    filter12 = value < 12,
    filter13 = value < 13,
    filter14 = value < 14,
    filter15 = value < 15,
    filter16 = value < 16
  )

animate(anim, fps = 50)
