# create the poodle logo of the styledTables package
# install.packages("hexSticker")
hexSticker::sticker(
    "man/figures/labelmachine.png",
    package = "labelmachine", 
    p_size = 16,
    p_y = 0.8,
    s_x=1,
    s_y=1.3,
    h_fill = "#17d2e1",
    p_color = "#ffffff",
    h_color = "#a635d0",
    s_width=0.7,
    s_height=0.7,
    filename="man/figures/logo.png"
  )
