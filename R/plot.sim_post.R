##' @title plot
##'
##' @description visualize tracks simulated from a `aniMotum` model fit
##'
##' @param x a `aniMotum` simulation data.frame with class `sim_fit`
##' @param type plots tracks as "line", "points" or "both" (default).
##' @param zoom logical; should map extent be defined by track extent (TRUE; default) or 
##' should global map be drawn (FALSE).  
##' @param ncol number of columns to arrange multiple plots
##' @param hires logical; use high-resolution coastline data. Attempts to use
##' high-res coastline data via [rnaturalearth::ne_countries] with `scale = 10`, 
##' if the `rnaturalearthhires` data package is installed. If not, then 
##' [rnaturalearth::ne_countries] with `scale = 50` data are used.
##' @param ortho logical; use an orthographic projection centered on the track
##' starting location(s) (TRUE; default). An orthographic projection may be 
##' optimal for high latitude tracks and/or tracks that traverse long distances. 
##' If FALSE then a global Mercator projection is used.
##' @param alpha opacity of simulated track points/lines. Lower opacity can ease
##'  visualization when multiple simulated overlap one another.
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of posterior simulated tracks. 
##' 
##' @importFrom ggplot2 ggplot aes geom_point geom_path theme_void
##' @importFrom ggplot2 element_blank xlab ylab geom_sf 
##' @importFrom ggplot2 coord_sf
##' @importFrom sf st_transform st_as_sf st_crs st_make_valid st_cast
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices extendrange
##' @importFrom rnaturalearth ne_countries
##' @method plot sim_post
##'
##' @examples
##' fit <- fit_ssm(ellie, model = "crw", time.step = 24)
##' psim <- sim_post(fit, what = "p", reps = 10)
##' plot(psim, type = "lines")
##'
##' @export
##' @md

plot.sim_post <- function(x, 
                          type = c("lines","points","both"),
                         zoom = TRUE,
                         ncol = 1,
                         hires = TRUE,
                         ortho = TRUE,
                         alpha = 0.5,
                         ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  stopifnot("x must be a sim_post object with class `sim_post`" = inherits(x, "sim_post"))
  
  type <- match.arg(type)
  
  ## get worldmap
  if(all(hires, requireNamespace("rnaturalearthhires", quietly = TRUE))) {
    wm <- ne_countries(scale = 10, returnclass = "sf")
  } else {
    wm <- ne_countries(scale = 50, returnclass = "sf")
  }
  wm.sf <- wm[, c("geometry", "region_un")]
  
  pos <- lapply(x$psims, function(x) select(x, rep, lon, lat)) |>
    bind_rows()
  
  mlon <- sapply(x$psims, function(x) x$lon[1]) |> mean() |> round()
  mlat <- sapply(x$psims, function(x) x$lat[1]) |> mean() |> round()
  pos.sf <- st_as_sf(pos, coords = c("lon","lat"), crs = 4326)
  
  if(ortho) {
    pos.sf <- st_transform(pos.sf, 
                           crs = paste0("+proj=ortho +lon_0=", mlon, 
                                        " +lat_0=", mlat, 
                                        " +units=km +ellps=WGS84 +no_defs"))
    wm.sf <- wm.sf |>
      st_transform(crs = st_crs(pos.sf))
  }
  
  if(!zoom) bounds <- st_bbox(wm.sf)
  if(zoom) bounds <- st_bbox(pos.sf)

  ## do plots
  p <- lapply(x$psims, function(x) {
    
    x <- st_as_sf(x, coords = c("lon", "lat"), crs = 4326) |>
      st_transform(crs = st_crs(pos.sf))
    
    if(type %in% c("lines","both")) {
      xl <- x |> 
        group_by(rep) |>
        summarise(do_union = FALSE) |>
        st_cast("MULTILINESTRING")
    }
    
    if(type %in% c("points","both")) {
      xp <- x |> 
        group_by(rep) |>
        summarise(do_union = FALSE) |>
        st_cast("MULTIPOINT")
    }

    m <- ggplot() +
      geom_sf(
        data = wm.sf,
        aes(group = region_un),
        fill = grey(0.6),
        colour = NA
      ) +
      xlim(bounds[c(1,3)]) +
      ylim(bounds[c(2,4)])
    
    if(type %in% c("lines","both")) {
      m <- m +
        geom_sf(
          data = subset(xl, rep != 0),
          colour = "dodgerblue",
          linewidth = 0.3,
          alpha = alpha
        ) +
        geom_sf(
          data = subset(xl, rep == 0),
          colour = "firebrick",
          linewidth = 0.5
        )
    }
    
    if(type %in% c("points","both")) {
      m <- m +
        geom_sf(
          data = subset(xp, rep != 0),
          colour = "dodgerblue",
          size = 0.5,
          alpha = alpha
        ) +
        geom_sf(
          data = subset(xp, rep == 0),
          colour = "firebrick",
          size = 0.8
        )
    } 
    
    if(zoom) {
      m <- m +
        xlab(element_blank()) +
        ylab(element_blank()) +
        theme_minimal()
    } else {
      m <- m +
        xlab(element_blank()) +
        ylab(element_blank()) +
        theme_void()
    }
    
  })
  ## arrange plots
  wrap_plots(p, ncol = ncol, heights = rep(1, ceiling(length(p)/ncol)))
}