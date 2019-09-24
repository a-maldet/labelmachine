# create the poodle logo of the styledTables package
# install.packages("hexSticker")
hexSticker::sticker(
  "various/labelmachine.png",
  package = c("label       ","     machine"), 
  p_size = 18,
  p_y = 0.7,
  p_x = c(0.60, 1.15),
  s_x=1,
  s_y=1.15,
  h_fill = "#17d2e1",
  p_color = c("#a635d0", "#ffffff"),
  h_color = "#a635d0",
  s_width=0.7,
  s_height=0.7,
  filename="man/figures/logo.png"
)
