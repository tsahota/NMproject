library(hexSticker) # https://github.com/GuangchuangYu/hexSticker

### .png - original
# sticker("man/figures/lifecycle-defunct.svg",
#         package = "NMproject", p_size = 40, p_family = "Sanchez", s_x = 1, s_y = .75,
#         s_width = .6, s_height = .4, h_color = "#a1c6c7",
#         h_fill = "#74a9cf", url = "https://tsahota.github.io/NMproject/", u_color = "black",
#         u_size = 5, filename = "./man/figures/NMproject_logo.png")


# sticker("man/figures/workflow2.svg",
#         package = "NMproject", p_size = 20, p_y = 1.5, p_family = "Sanchez", 
#         s_x = 1, s_y = .75,s_width = .6, s_height = .4, 
#         h_color = "#a1c6c7", h_fill = "#74a9cf",
#         url = "https://tsahota.github.io/NMproject/", u_color = "black",
#         u_size = 4, filename = "man/figures/NMproject_logo.png")
# 
# sticker("man/figures/workflow2.svg",
#         package = "NMproject", p_size = 6, p_y = 1.5, p_family = "Sanchez", 
#         s_x = 1, s_y = .75,s_width = .6, s_height = .4, 
#         h_color = "#a1c6c7", h_fill = "#74a9cf",
#         url = "https://tsahota.github.io/NMproject/", u_color = "black",
#         u_size = 1.4, filename = "man/figures/NMproject_logo.svg")
# 
# sticker("man/figures/workflow3.svg",
#         package = "NMproject", p_size = 6, p_y = 1.5, p_family = "Sanchez", 
#         s_x = 1, s_y = .75,s_width = .6, s_height = .4, 
#         h_color = "#a1c6c7", h_fill = "#74a9cf",
#         url = "https://tsahota.github.io/NMproject/", u_color = "black",
#         u_size = 1.4, filename = "man/figures/NMproject_logo.svg")

sticker("man/figures/pptworkflow.svg",
        package = "NMproject", p_size = 7, p_y = 1.35, p_family = "Sanchez", 
        s_x = 1, s_y = .75,s_width = .7, s_height = .6, 
        #h_color = "#95a5a6", h_fill = "#2c3e50",
        h_color = "#8faaac", h_fill = "#2c3e50",
        url = "https://tsahota.github.io/NMproject/", u_color = "white",
        u_size = 1.4, filename = "man/figures/NMproject_logo.svg")

unlink("man/figures/logo.svg")
usethis::use_logo("man/figures/NMproject_logo.svg", geometry = "240x278", retina = TRUE)
pkgdown::build_site()
