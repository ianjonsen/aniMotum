##' @title plot
##'
##' @description visualize tracks simulated from a `foieGras` model fit
##'
##' @param x a `foieGras` simulation data.frame with class `simfit`
##' @param type plots tracks as "line", "points" or "both" (default). 
##' @param zoom logical; should map extent be defined by track extent (TRUE) or 
##' should global map be drawn (FALSE; default).  
##' @param or orientation of projected map, default is to centre on 
##' start of fitted track (ignored if `mapproj` package is not installed).
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
##' @importFrom ggplot2 element_blank xlab ylab geom_polygon 
##' @importFrom ggplot2 coord_map coord_quickmap theme_void
##' @importFrom broom tidy
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices extendrange
##' @importFrom rnaturalearth ne_countries
##' @method plot simfit
##'
##' @examples
##' fit <- fit_ssm(ellie, model = "crw", time.step = 24)
##' trs <- simfit(fit, what = "p", reps = 2)
##' plot(trs, type = "b")
##'
##' @export
##' @md

plot.simfit <- function(x, 
                        type = c("lines","points","both"),
                        zoom = FALSE,
                        or = NULL,
                        ncol = 1,
                        hires = FALSE,
                        ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  stopifnot("x must be a simfit object with class `simfit`" = inherits(x, "simfit"))
  
  type <- match.arg(type)
  
  ## get worldmap
  if(all(hires, requireNamespace("rnaturalearthhires", quietly = TRUE))) {
    wm <- ne_countries(scale = 10, returnclass = "sp")
  } else {
    wm <- ne_countries(scale = 50, returnclass = "sp")
  }
  wm <- suppressMessages(tidy(wm))
  wm$region <- wm$id
  wm.df <- wm[,c("long","lat","group","region")]
  
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
 
    if(is.null(or)) or <- c(x$lat[1], x$lon[1], 0)
    
      m <- ggplot() + 
        geom_polygon(data = wm.df, 
                     aes(long, lat, group = group), 
                     fill = grey(0.6))
      
      if(requireNamespace("mapproj", quietly = TRUE)) {
        m <- m + coord_map("ortho",
                  orientation = or,
                  xlim = bounds[1:2],
                  ylim = bounds[3:4])
      } else {
        m <- m + coord_quickmap(
          xlim = bounds[1:2],
          ylim = bounds[3:4]
        )
      }
    
    switch(type, 
           lines = {
             m <- m + 
               geom_path(data = subset(x, rep != 0),
                         aes(lon, lat, group = rep),
                         colour = "dodgerblue",
                         size = 0.5,
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
                         size = 0.5,
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
      theme_void()
      
  })
  ## arrange plots
  wrap_plots(p, ncol = ncol, heights = rep(1, ceiling(length(p)/ncol)))
}
  