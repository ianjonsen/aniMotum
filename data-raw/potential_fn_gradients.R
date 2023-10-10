## code to prepare `potential_fn_gradients` dataset goes here

usethis::use_data(potential_fn_gradients, overwrite = TRUE)

## IDJ - 

## use World Mercator projection
proj <- "+proj=merc +units=km +datum=WGS84"

## create world land raster
wm <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")

y <- terra::rast(crs = sf::st_crs(wm)$wkt,
          vals = 1,
          resolution = c(0.1, 0.1),
          xmin = -180,
          xmax = 180,
          ymin = -86,
          ymax = 84)

x <- terra::rasterize(wm, y, fun = "min")
x <- terra::project(x, proj)

## generate gradient rasters
# set land to NA, water to 1
x[is.na(x)] <- -1
x[x == 1] <- NA
x[x == -1] <- 1

## calculate gradient rasters - these are needed to keep fish off land
dist <- terra::distance(x)
x1 <- terra::terrain(dist, v = "slope", unit = "radians")
y1 <- terra::terrain(dist, v = "aspect", unit = "radians")
grad.x <- -1 * x1 * cos(0.5 * pi - y1)
grad.y <- -1 * x1 * sin(0.5 * pi - y1)
grad <- c(grad.x, grad.y)

grad <- terra::wrap(grad)
save(grad, file = "inst/extdata/grad.rda", compress = "xz")
