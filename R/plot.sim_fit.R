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
                        type = c("lines","points","both"),
                        zoom = TRUE,
                        ncol = 1,
                        hires = FALSE,
                        ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  stopifnot("x must be a sim_fit object with class `sim_fit`" = inherits(x, "sim_fit"))
  
  type <- match.arg(type)
  
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

    if(!zoom) { 
      bounds <- c(-180,180,-89.99,89.99)
    } else {
      bounds <- c(range(x$lon), range(x$lat))
    }
    
    m <- ggplot() +
      geom_sf(data = wm.df,
              aes(group = region_un),
              fill = grey(0.6))
    
    m <- m + coord_sf(xlim = bounds[1:2],
                      ylim = bounds[3:4])
    
    switch(type, 
           lines = {
             m <- m + 
               geom_path(data = subset(x, rep != 0),
                         aes(lon, lat, group = rep),
                         colour = "dodgerblue",
                         linewidth = 0.5,
                         alpha = 0.6
                         )
           },
           points = {
             m <- m + 
               geom_point(data = subset(x, rep != 0),
                          aes(lon, lat),
                          colour = "dodgerblue",
                          size = 0.75,
                          alpha = 0.6)
           },
           both = {
             m <- m + 
               geom_path(data = subset(x, rep != 0),
                         aes(lon, lat, group = rep),
                         colour = "dodgerblue",
                         linewidth = 0.5,
                         alpha = 0.6
               ) +
               geom_point(data = subset(x, rep != 0),
                          aes(lon, lat),
                          colour = "dodgerblue",
                          size = 0.75,
                          alpha = 0.6)
           })
    m <- m + 
      geom_point(
        data = subset(x, rep == 0),
        aes(lon, lat),
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
  