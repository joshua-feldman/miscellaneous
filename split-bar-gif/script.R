# THIS CODE IS HEAVILY BASED ON THAT OF GEORGIOS KARAMANIS (@geokaramanis)
# ORIGINAL CONCEPT BY ELANA LEVIN SCHTULBERG (@ElanaEllesce)

library(ggplot2)
library(dplyr)
library(magick)
library(gganimate)

setwd("~/Github/miscellaneous/split-bar-gif")

col_fill <- "black"
col_bg <- "#93CB56"

gif <- image_read("snoop.gif") %>%
  image_convert(colorspace = "gray")

gif_w <- image_info(gif)$width
gif_h <- image_info(gif)$height

if (gif_w[1] >= gif_h[1]) {
  gif <- image_resize(gif, "80")
} else {
  gif <- image_resize(gif, ("x80"))
}

# Loop through frames of GIF
for(i in 1:length(gif)) {

  # Create array for each frame
  assign(paste0("gif_array", i), drop(as.integer(gif[[i]])))
  
  # Create data table for each frame
  assign(paste0("gif_df", i), as.data.frame.table(eval(as.name(paste0("gif_array", i)))) %>% 
    `colnames<-`(c("y", "x", "b")) %>% 
    mutate(
      across(everything(), as.numeric),
      bf = 1 - b / 255
    ))
  
  # Allocate file path for saving plots to disk
  fp <- file.path("frames", paste0("frame", i, ".png"))
  
  # Create plot for each frame
  p <- ggplot(eval(as.name(paste0("gif_df", i)))) +
    geom_rect(aes(xmin = x, xmax = x + bf * 0.9, ymin = y, ymax = y + 0.85), fill = col_fill, color = NA) +
    scale_y_reverse() +
    coord_fixed(expand = FALSE) +
    theme_void() +
    theme(legend.position = "none", plot.background = element_rect(fill = col_bg, color = NA))
  
  # Save each plot to disk
  ggsave(plot = p, 
         filename = fp, 
         device = "png")
  
}

## List file names and read in
imgs <- list.files("frames", full.names = TRUE)
imgs <- gtools::mixedsort(sort(imgs))
img_list <- lapply(imgs, image_read)

## Join images together
img_joined <- image_join(img_list)

## Animate (Note: you may need to adjust the FPS parameter according to the frame rate of your GIF)
img_animated <- image_animate(img_joined, fps = 10)

## View animated images
img_animated

## Save to disk
image_write(image = img_animated, path = "snoop_new.gif")