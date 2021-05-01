##' @title plot
##'
##' @description visualize tracks simulated from a foieGras model fit
##'
##' @param x a \code{foieGras} simulation data.frame with class \code{fG_simfit}
##' @param type plots tracks as "line", "points" or "both" (default). 
##' @param ext map extent for plotting, either "hemi" (default) for hemisphere, or
##' "tracks" to zoom in on track extents. In the latter case, the projection is Mercator.
##' @param or orientation of orthographic projection, default is to centre on start of fitted track
##' @param ncol number of columns to arrange multiple plots
##' @param pal \code{hcl.colors} palette to use (default: "Zissou1"; type \code{hcl.pals()} for options)
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of simulated tracks. 
##' 
##' @importFrom ggplot2 ggplot aes geom_point geom_path theme_minimal
##' @importFrom ggplot2 element_blank xlab ylab coord_fixed geom_polygon 
##' @importFrom ggplot2 coord_map theme_void
##' @importFrom broom tidy
##' @importFrom dplyr "%>%"
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
                           or = NULL,
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
    wm <- ne_countries(scale = 50, returnclass = "sp") 
  } else {
    wm <- ne_countries(scale = 110, returnclass = "sp") 
  }
  
  wm <- suppressMessages(tidy(wm))
  wm$region <- wm$id
  wm.df <- wm[,c("long","lat","group","region")]
  
  p <- lapply(x$sims, function(x) {
    if(min(x$lon) < -175 & max(x$lon > 175)) x$lon <- ifelse(x$lon < 0, x$lon + 360, x$lon)
    switch(ext, 
           hemi = {
             bounds <- c(-180,180,-84.99,89.99)
           },
           tracks = {
             bounds <- c(range(x$lon), range(x$lat))
           })
    
    if(is.null(or)) or <- c(x$lat[1], x$lon[1], 0)
    
      m <- ggplot() + 
        geom_polygon(data = wm.df, 
                     aes(long, lat, group = group), 
                     fill = grey(0.4)) +
        coord_map("ortho",
                  orientation = or,
                  xlim = bounds[1:2],
                  ylim = bounds[3:4])
    
    switch(type, 
           lines = {
             m <- m + 
               geom_path(data = x%>% filter(rep != 0),
                         aes(lon, lat, group = rep),
                         colour = hcl.colors(n=5, palette = pal)[1],
                         size = 0.5,
                         alpha = 0.6
                         )
           },
           points = {
             m <- m + 
               geom_point(data = x%>% filter(rep != 0),
                          aes(lon, lat),
                          colour = hcl.colors(n=5, palette = pal)[1],
                          size = 0.75,
                          alpha = 0.6)
           },
           both = {
             m <- m + 
               geom_path(data = x%>% filter(rep != 0),
                         aes(lon, lat, group = rep),
                         colour = hcl.colors(n=5, palette = pal)[1],
                         size = 0.5,
                         alpha = 0.6
               ) +
               geom_point(data = x%>% filter(rep != 0),
                          aes(lon, lat),
                          colour = hcl.colors(n=5, palette = pal)[1],
                          size = 0.75,
                          alpha = 0.6)
           })
    m <- m + 
      geom_point(
        data = x %>% filter(rep == 0),
        aes(lon, lat),
        colour = hcl.colors(n=5, palette = pal)[3],
        size = 1
      ) +
      xlab(element_blank()) +
      ylab(element_blank()) + 
      theme_void()
      
  })

  wrap_plots(p, ncol = ncol, heights = rep(1, ceiling(length(p)/ncol)))
}
  