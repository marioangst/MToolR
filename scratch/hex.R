library(hexSticker)
library(here)

imgurl <- here("scratch/mtoollogo.jpeg")
sticker(imgurl, package="MToolR", p_size=20, s_x=1, s_y=1.2, s_width=0.5,
        p_y = 0.7,h_color = "black",h_fill = "#5a9bd5",
        filename="inst/figures/mtoolr_logo.png")
