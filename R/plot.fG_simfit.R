##' @title plot
##'
##' @description visualize tracks simulated from a foieGras model fit
##'
##' @param x a \code{foieGras} simulation data.frame with class \code{fG_simfit}
##' @param type plots tracks as "line", "points" or "both" (default). 
##' @param ext map extent for plotting, either "hemi" (default) for hemisphere, or
##' "tracks" to zoom in on track extents. In the latter case, the projection is Mercator.
##' @param ncol number of columns to arrange multiple plots
##' @param pal \code{hcl.colors} palette to use (default: "Zissou1"; type \code{hcl.pals()} for options)
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of simulated tracks. 
##' 
##' @importFrom ggplot2 ggplot element_blank xlab ylab geom_sf xlim ylim 
##' @importFrom ggplot2 theme_void
##' @importFrom sf st_as_sf st_transform st_bbox st_is_empty
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices hcl.colors extendrange
##' @importFrom rnaturalearth ne_countries
##' @method plot fG_simfit
##'
##' @examples
##' fit <- fit_ssm(sese1, vmax = 4, model = "crw", time.step = 72)
##' trs <- simfit(fit, what = "p", reps = 2)
##' plot(trs, type = "b")
##'
##' @export

plot.fG_simfit <- function(x, 
                           type = c("lines","points","both"),
                           ext = c("hemi", "tracks"),
                           ncol = 1,
                           pal = "Zissou1",
                        ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  type <- match.arg(type)
  ext <- match.arg(ext)
  
  ## get coastline
  if(requireNamespace("rnaturalearthdata", quietly = TRUE)) {
    wm <- ne_countries(scale = 50, returnclass = "sf") 
  } else {
    wm <- ne_countries(scale = 110, returnclass = "sf") 
  }
  
   p <- lapply(x$sims, function(xx) {
    prj <- paste0("+proj=ortho +lon_0=", trunc(xx$lon[1]), 
                  " +lat_0=", trunc(xx$lat[1]), 
                  " +units=km +datum=WGS84 +no_defs")
    wmx <- st_transform(wm, crs = prj)
    xx <- st_as_sf(xx, coords = c("lon","lat"), crs = 4326)
    xx <- st_transform(xx, crs = prj)
    
    if(ext == "tracks") {
      bounds <- st_bbox(xx)
      bounds[c(1,3)] <- extendrange(bounds[c(1,3)], f= 0.05)
      bounds[c(2,4)] <- extendrange(bounds[c(2,4)], f= 0.05)
    } else {
      bounds <- st_bbox(wmx)
    }

      m <- ggplot() + 
        geom_sf(data = wmx,
                fill = grey(0.5),
                colour = NA) +
        xlim(bounds[c(1,3)]) +
        ylim(bounds[c(2,4)])

    switch(type, 
           lines = {
             xl <- subset(xx, rep != 0)
             xl <- group_by(xl, rep)
             xl <- summarise(xl, do_union = FALSE)
             xl <- st_cast(xl, "MULTILINESTRING")
             
             m <- m + 
               geom_sf(data = xl,
                         colour = hcl.colors(n=5, palette = pal)[1],
                         size = 0.5,
                         alpha = 0.6
                         )
           },
           points = {
             xp <- subset(xx, rep != 0)
             xp <- group_by(xp, rep)
             xp <- summarise(xp, do_union = FALSE)
             xp <- st_cast(xp, "MULTIPOINT")
             m <- m + 
               geom_sf(data = xp,
                          colour = hcl.colors(n=5, palette = pal)[1],
                          size = 0.75,
                          alpha = 0.6)
           },
           both = {
             xl <- subset(xx, rep != 0)
             xl <- group_by(xl, rep)
             xl <- summarise(xl, do_union = FALSE)
             xl <- st_cast(xl, "MULTILINESTRING")
             xp <- subset(xx, rep != 0)
             xp <- group_by(xp, rep)
             xp <- summarise(xp, doUnion = FALSE)
             xp <- st_cast(xp, "MULTIPOINT")
             m <- m + 
               geom_sf(data = xl,
                         colour = hcl.colors(n=5, palette = pal)[1],
                         size = 0.5,
                         alpha = 0.6
               ) +
               geom_sf(data = xp,
                          colour = hcl.colors(n=5, palette = pal)[1],
                          size = 0.75,
                          alpha = 0.6)
           })
    xp0 <- subset(xx, rep == 0)
    xp0 <- summarise(xp0, do_union = FALSE)
    xp0 <- st_cast(xp0, "MULTIPOINT")
    
    m <- m + 
      geom_sf(
        data = xp0,
        colour = hcl.colors(n=5, palette = pal)[3],
        size = 1
      ) +
      xlab(element_blank()) +
      ylab(element_blank()) + 
      theme_void()
    
    m
  })

  wrap_plots(p, ncol = ncol, heights = rep(1, ceiling(length(p)/ncol)))
}
  