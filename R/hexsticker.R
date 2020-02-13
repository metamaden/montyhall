#!/usr/bin/env R

library(hexSticker)

# Make the montyhall package hex sticker

sticker("goat.jpg", package = "montyhall", 
        s_x = 1, s_y = 1.2, s_width = 0.55,
        h_fill = "white", p_color = "black", 
        p_size = 28, p_y = 0.65,
        url = "github.com/metamaden/montyhall", 
        u_color = "black", u_size = 4.1, 
        h_color = "forestgreen")