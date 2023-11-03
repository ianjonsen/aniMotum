##' @title plot
##'
##' @description visualize tracks simulated from a `aniMotum` model fit
##'
##' @param x a `aniMotum` simulation data.frame with class `sim_fit`
##' @param type deprecated. All tracks are rendered as points.
##' @param zoom logical; should map extent be defined by track extent (TRUE; default) or 
##' should global map be drawn (FALSE).  
##' @param ncol number of columns to arrange multiple plots
##' @param hires logical; use high-resolution coastline data. Attempts to use
##' high-res coastline data via [rnaturalearth::ne_countries] with `scale = 10`, 
##' if the `rnaturalearthhires` data package is installed. This extends the
##' plot rendering time so is set to FALSE by default, in which case 
##' [rnaturalearth::ne_countries] with `scale = 50` data are used.
##' @param ortho logical; use an orthographic projection centered on the track
##' starting location(s) (TRUE; default). An orthographic projection may be 
##' optimal for high latitude tracks and/or tracks that traverse long distances. 
##' If FALSE then a global Mercator projection is used.
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of simulated tracks. 
##' 
##' @importFrom ggplot2 ggplot aes geom_point geom_path theme_minimal theme_void
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
##' plot(trs)
##'
##' @export
##' @md

plot.sim_fit <- function(x, 
                         type = NULL,
                         zoom = TRUE,
                         ncol = 1,
                         hires = FALSE,
                         ortho = TRUE,
                         ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  if(!is.null(type)) message("the `type` argument is deprecated. Rendering all tracks as points")
  
  stopifnot("x must be a sim_fit object with class `sim_fit`" = inherits(x, "sim_fit"))
  
  
  ## get worldmap
  if(all(hires, requireNamespace("rnaturalearthhires", quietly = TRUE))) {
    wm <- ne_countries(scale = 10, returnclass = "sf")
  } else {
    wm <- ne_countries(scale = 50, returnclass = "sf")
  }
  wm.sf <- wm[, c("geometry", "region_un")]
  
  pos <- lapply(x$sims, function(x) select(x, rep, lon, lat)) |>
    bind_rows()
  
  mlon <- sapply(x$sims, function(x) x$lon[1]) |> mean() 
  mlat <- sapply(x$sims, function(x) x$lat[1]) |> mean() 
  mlat <- round(mlat / 10) * 10
  
  pos.sf <- st_as_sf(pos, coords = c("lon","lat"), crs = 4326)
  
  if(ortho) {
    pos.sf <- st_transform(pos.sf, 
                           crs = paste0("+proj=ortho +lon_0=", mlon, 
                                        " +lat_0=", mlat, 
                                        " +ellps=WGS84 +no_defs"))
    wm.sf <- wm.sf |>
      st_transform(crs = st_crs(pos.sf)) |>
      st_make_valid()
  }
  
  if(!zoom) bounds <- st_bbox(wm.sf)
  if(zoom) bounds <- st_bbox(pos.sf)

  ## do plots
  p <- lapply(x$sims, function(x) {
    
    x <- st_as_sf(x, coords = c("lon", "lat"), crs = 4326) |>
      st_transform(crs = st_crs(pos.sf))
    
    m <- ggplot() +
      geom_sf(
        data = wm.sf,
        aes(group = region_un),
        fill = grey(0.6)
      ) +
      xlim(bounds[c(1,3)]) +
      ylim(bounds[c(2,4)])
    
    m <- m +
      geom_sf(
        data = subset(x, rep != 0),
        colour = "dodgerblue",
        size = 0.5,
        alpha = 0.6
      ) + 
      geom_sf(
        data = subset(x, rep == 0),
        colour = "firebrick",
        size = 1
      ) + 
      xlab(element_blank()) +
      ylab(element_blank())
    
    if(zoom) {
      m <- m + 
      theme_minimal()
    } else {
      m <- m +
        theme_void()
    }
    
  })
  ## arrange plots
  wrap_plots(p, ncol = ncol, heights = rep(1, ceiling(length(p)/ncol)))
}