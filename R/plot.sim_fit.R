##' @title plot
##'
##' @description visualize tracks simulated from a `aniMotum` model fit
##'
##' @param x a `aniMotum` simulation data.frame with class `sim_fit`
##' @param type plots tracks as "line", "points" or "both" (default). 
##' @param zoom logical; should map extent be defined by track extent (TRUE; default) or 
##' should global map be drawn (FALSE).  
##' @param or orientation of projected map, default is to centre on 
##' start of fitted track (ignored if `mapproj` package is not installed). This argument
##' is deprecated and, if specified, will be ignored with a warning.
##' @param ncol number of columns to arrange multiple plots
##' @param hires logical; use high-resolution coastline data. Attempts to use
##' high-res coastline data via [rnaturalearth::ne_countries] with `scale = 10`, 
##' if the `rnaturalearthhires` data package is installed. This extends the
##' plot rendering time so is set to FALSE by default, in which case 
##' [rnaturalearth::ne_countries] with `scale = 50` data are used.
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of simulated tracks. 
##' 
##' @importFrom ggplot2 ggplot aes geom_point geom_path theme_minimal
##' @importFrom ggplot2 element_blank xlab ylab geom_sf 
##' @importFrom ggplot2 coord_sf
##' @importFrom sf st_transform st_as_sf st_crs st_make_valid
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices extendrange
##' @importFrom rnaturalearth ne_countries
##' @method plot sim_fit
##'
##' @examples
##' fit <- fit_ssm(ellie, model = "crw", time.step = 24)
##' trs <- sim_fit(fit, what = "p", reps = 2)
##' plot(trs, type = "b")
##'
##' @export
##' @md

plot.sim_fit <- function(x, 
                        type = "points",
                        zoom = TRUE,
                        ncol = 1,
                        hires = FALSE,
                        ortho = TRUE,
                        ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  stopifnot("x must be a sim_fit object with class `sim_fit`" = inherits(x, "sim_fit"))
  
  if(type == "lines") {
    message("setting type to points...")
    type <- "points"
  } else if(type == "both") {
    message("setting type to points...")
    type <- "points"
  }  
  ## lines not working with sf for some reason...so force all to points
  type <- match.arg(type, choices = c("points"))
  
  ## get worldmap
  if(all(hires, requireNamespace("rnaturalearthhires", quietly = TRUE))) {
    wm <- ne_countries(scale = 10, returnclass = "sf")
  } else {
    wm <- ne_countries(scale = 50, returnclass = "sf")
  }
  wm.df <- wm[, c("geometry", "region_un")]

  ## do plots
  p <- lapply(x$sims, function(x) {

    if(min(x$lon) < -175 & max(x$lon > 175)) {
      x$lon <- ifelse(x$lon < 0, x$lon + 360, x$lon)
    }

    lat0 <- round(x$lat[1])
    lat0 <- ifelse(lat0 < -40, -40, ifelse(lat0 > 40, 40, lat0))
    lon0 <- round(x$lon[1])
    
    x <- st_as_sf(x, coords = c("lon", "lat"), crs = 4326)

    wm.df <- wm.df |>
      st_transform(crs = paste0(
        "+proj=ortho +lon_0=",
        lon0,
        " +lat_0=",
        lat0,
        " +units=km +datum=WGS84"
      )) |>
      st_make_valid()
  
    x <- st_transform(x, crs = st_crs(wm.df)$input)
    
    bounds <- st_bbox(x)
    
    m <- ggplot() +
      geom_sf(
        data = wm.df,
        aes(group = region_un),
        fill = grey(0.6)
      ) +
      xlim(bounds[c(1,3)]) +
      ylim(bounds[c(2,4)])
      
    switch(type, 
           lines = {
             m <- m + 
               geom_sf(data = subset(xl, rep != 0),
                         colour = "dodgerblue",
                         linewidth = 0.5,
                         alpha = 0.6
                         )
           },
           points = {
             m <- m + 
               geom_sf(data = subset(x, rep != 0),
                          colour = "dodgerblue",
                          size = 0.3,
                          alpha = 0.6)
           },
           both = {
             m <- m + 
               geom_sf(data = subset(xl, rep != 0),
                         colour = "dodgerblue",
                         linewidth = 0.5,
                         alpha = 0.6
               ) +
               geom_sf(data = subset(x, rep != 0),
                          colour = "dodgerblue",
                          size = 0.75,
                          alpha = 0.6)
           })
      
    m <- m + 
      geom_sf(
        data = subset(x, rep == 0),
        colour = "firebrick",
        size = 1
      ) +
      xlab(element_blank()) +
      ylab(element_blank()) + 
      theme_minimal()
      
  })
  ## arrange plots
  wrap_plots(p, ncol = ncol, heights = rep(1, ceiling(length(p)/ncol)))
}
  