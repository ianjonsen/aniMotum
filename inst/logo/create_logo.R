require(ggplot2, quietly = TRUE)
require(aniMotum, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(sf, quietly = TRUE)
require(hexSticker, quietly = TRUE)
require(cowplot, quietly = TRUE)

fit <- fit_ssm(sese,
               model = "mp",
               time.step = 24,
               control = ssm_control(verbose = 0))

d <- grab(fit, "data", as_sf = TRUE)
pmp <- grab(fit, "predicted", as_sf = TRUE, normalise = TRUE)

prj <- "+proj=stere +lon_0=68 +units=km +datum=WGS84"
bb <- st_bbox(d %>% st_transform(crs = prj))

m <- ggplot() + 
  geom_sf(data = d, col="steelblue3", size=0.15) + 
  geom_sf(data = pmp %>% filter(g > 0.4), aes(col = g), size=0.1) +
  geom_sf(data = pmp %>% filter(g <= 0.4), aes(col = g), size=0.1) +
  scale_colour_viridis_c(option = "E") +
  coord_sf(xlim = extendrange(r=c(bb["xmin"]-1625, bb["xmax"]), f=0.01),
           ylim = extendrange(r=c(bb["ymin"], bb["ymax"]+150), f=0.01),
           expand = FALSE,
           crs = prj) + 
  theme_minimal() +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme(legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.5),
        panel.grid.minor = element_line(linewidth = 0.5),
        axis.text = element_blank())

m1 <- ggdraw() +
  draw_image("inst/logo/img/sese_male.png",  x=0.5, y=0.82, scale=0.275, hjust=0.5, vjust=0.5, interpolate=FALSE) +
  draw_image("inst/logo/img/huwh2.png",  x = 0.78, y = 0.78, scale=0.25, hjust=0.5, vjust=0.5) +
  draw_image("inst/logo/img/whsh2.png",  x = 0.27, y = 0.79, scale = 0.16, hjust=0.5, vjust=0.5) +
  draw_image("inst/logo/img/lbtu2.png", x = 0.9, y = 0.725, scale = 0.125, hjust=0.5, vjust=0.5) +
  draw_image("inst/logo/img/kipe2.png",  x = 0.15, y = 0.735, scale = 0.1, hjust=0.5, vjust=0.5) +
  draw_image("inst/logo/img/madu1.png",  x = 0.53, y = 1.1, scale=0.075, hjust=0.5, vjust=0.5) +
  draw_plot(m)

s <- sticker(
  m1,
  package = "aniMotum",
  p_size = 16,
  p_y = 1.6,
  p_family = "sans",
  p_color = "#C4B56CFF",
  h_color = "white",
  s_x = 0.945,
  s_y = 0.725,
  s_width = 1.85,
  s_height = 1.85,
  h_fill =  "#045a8d",
  spotlight = FALSE,
  l_x = 0.94,
  l_y = 1.08,
  l_width = 2,
  filename = "man/figures/logo.png"
)

