#!/usr/local/bin/Rscript --vanilla

library("magick")

slide <- image_read("logo-slide.tiff")
banner <- image_crop(slide, "370x80+2+2")
image_write(banner, "../banner.png", format = "png")

ccap <- image_read("c-07.jpg")
logo <- image_scale(ccap, "80x80")
image_write(logo, "../logo.png", format = "png")
