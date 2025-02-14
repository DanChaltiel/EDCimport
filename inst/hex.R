
library(tidyverse)

p =
  tibble(angle = 2*pi/6*seq(0,6),
         y_arrow = c(0.2,0.35,0.35, 0.2,0.35,0.35, 0.2)) %>%
  ggplot() +
  aes(x=angle, y=1) +
  ggimage::geom_image(image="inst/figures/table.png", size=0.2) +
  geom_segment(aes(x=angle, xend=angle, y=y_arrow, yend=0.7), 
               arrow=arrow(type="open", length=unit(0.2, "cm")), 
               linewidth=0.5, color="black") +
  ylim(0, NA) +
  coord_polar(theta = "x") +
  theme_void()


hexSticker::sticker(
  #package name
  package="EDCimport",
  p_size=20,
  p_x=1, p_y=1,
  #hexagon
  h_fill = "#404040",
  h_color = "#e85811",
  h_size = 1.2,
  #subplot
  subplot= p,
  s_x=1, s_y=1,
  s_width=1.75,
  s_height=5000,
  #output
  filename="man/figures/logo.png"
)

