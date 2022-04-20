##' @title map
##'
##' @description map foieGras-estimated locations and behavioural indices with
##' coastline and projection options
##'
##' @param x a `foieGras` ssm fit object with class `ssm_df`
##' @param y optionally, a `foieGras` mpm fit object with class `mpm_df`
##' @param what specify which location estimates to map: fitted, predicted or
##' rerouted
##' @param aes a list of map controls and aesthetics (shape, size, col, fill, alpha)
##' for each map feature (estimated locations, confidence ellipses, track lines, 
##' observed locations, land masses, water bodies). Constructed by `aes_lst()` and
##' can be modified for custom maps (see examples)
##' @param by.id when mapping multiple tracks, should locations be coloured by
##' id (logical; default = TRUE if `nrow(x) > 1` else FALSE; ignored if behavioural
##' index provided)
##' @param by.date when mapping single tracks, should locations be coloured by 
##' date (logical; default = FALSE; ignored if behavioural
##' index provided)
##' @param crs `proj4string` for re-projecting locations, if NULL the
##' default projection (Mercator) for the fitting the SSM will be used
##' @param ext.rng proportion (can exceed 1) to extend the plot range in x and y
##' dimensions
##' @param map_type background map type ("default" uses [rnaturalearth] 
##' to add landmasses). If [rnaturalearthdata] is installed; if packages [ggspatial]
##' and [rosm] are installed then any tile map type returned by [rosm::osm.types]
##' can be used for a potentially more detailed coastline at fine spatial scales,
##' given appropriate zoom settings (see [ggspatial::annotation_map_tile] for details).
##' @param normalise logical; if output includes a move persistence estimate, 
##' should g (the move persistence index) be normalised to have minimum = 0 and 
##' maximum = 1 (default = FALSE). 
##' @param group logical; should g be normalised among individuals as a group, 
##' a 'relative g', or separately to highlight regions of lowest and highest move
##' persistence along a track (default = FALSE).
##' @param ... additional arguments passed to [ggspatial::annotation_map_tile]
##' 
##' @return a map as a ggplot2 object
##' 
##' @importFrom ggplot2 ggplot geom_sf aes aes_string ggtitle xlim ylim unit 
##' @importFrom ggplot2 element_text theme  scale_fill_gradientn scale_fill_manual 
##' @importFrom ggplot2 element_blank scale_colour_manual scale_colour_gradientn
##' @importFrom ggplot2 element_line coord_sf
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_as_sfc st_buffer 
##' @importFrom sf st_crs st_coordinates st_cast st_multipolygon st_polygon st_union
##' @importFrom utils data
##' @importFrom grDevices extendrange grey
##' @importFrom dplyr group_by summarise
##' @importFrom grDevices hcl.colors
##' 
##' @examples 
##' ## an ssm fit object
##' fit <- fit_ssm(sese1, model = "rw", time.step = 24, control = ssm_control(verbose = 0))
##' 
##' ## redner default map
##' map(fit, what = "p")
##' 
##' ## construct aes list to modify map
##' aes <- aes_lst()
##' 
##' ## modify aes to turn on estimated track line & observed Argos locations 
##' aes$obs <- aes$line <- TRUE
##' ## set ocean colour to pale blue
##' aes$df$fill[6] <- hcl.colors(n=1, palette = "Blues", alpha = 0.3)
##' 
##' ## map using modified aes list, with polar stereographic projection centered
##' ##    on approximate track midpoint, extend x,y limits by 10%
##' m <- map(fit, what = "p", aes = aes, crs = "+proj=stere +lon_0=90 +units=km +datum=WGS84", ext.rng = c(0.1,0.1))
##' 
##' ## use thinner graticule lines than ggplot2 default
##' m + theme(panel.grid = element_line(size = 0.1))
##' 
##' 
##' @export
##' @md

map <- function(x,
                y = NULL,
                what = c("fitted", "predicted", "rerouted"),
                aes = aes_lst(),
                by.id = TRUE,
                by.date = FALSE,
                crs = NULL,
                ext.rng = c(0.05, 0.05),
                map_type = "default",
                normalise = FALSE,
                group = FALSE,
                ...) {

  what <- match.arg(what)
  stopifnot("x must be a foieGras ssm fit object with class `ssm_df`" = 
              inherits(x, "ssm_df"))
  stopifnot("y must either be NULL or a foieGras mpm fit object with class `mpm_df`" = 
              inherits(y, "mpm_df") | is.null(y))
  stopifnot("individual `ssm` fit objects with diloc_sfering projections not currently supported" = 
              length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) == 1)
  if(!is.null(crs)) {
    stopifnot("crs must be a proj4string with units=km,
              \n eg. `+proj=stere +lat_0=-90 +lon_0=0 +datum=WGS84 +units=km +no_defs`" = 
                is.character(crs))
  }
  
  if(map_type != "default" & !(requireNamespace("rosm", quietly = TRUE) | 
                               requireNamespace("ggspatial", quietly = TRUE))) {
    cat("required packages `rosm` and/or `ggspatial` are not installed, 
        switching map_type to default\n")
    map_type <- "default"
  }
 
  ## estimated locations in projected form
  loc_sf <- grab(x, what = what, as_sf = TRUE, normalise = normalise, group = group)
  
  if(!is.null(crs)) {
    if (length(grep("+units=km", crs, fixed = TRUE)) == 0) {
      cat("converting projection units from m to km to match SSM output")
      crs <- paste(crs, "+units=km")
    }
    loc_sf <- st_transform(loc_sf, crs = crs)
  } else {
    crs <- st_crs(loc_sf)
  }
  
  ## generate track lines & cast tp MULTILINESTRING for plot efficiency
  if (!is.na(aes$df[3,3])) {
    line_sf <- group_by(loc_sf, id)
    line_sf <- summarise(line_sf, do_union = FALSE)
    line_sf <- st_cast(line_sf, "MULTILINESTRING")
  } else {
    line_sf <- NULL
  }
  
  ## calc confidence ellipses around estimated locations & dissolve overlapping segments
  if(!is.na(aes$df[2,5])) {
    locs <- st_coordinates(loc_sf)
    locs <- data.frame(id = loc_sf$id, x = locs[,1], y = locs[,2], x.se = loc_sf$x.se, y.se = loc_sf$y.se)
    locs.lst <- split(locs, locs$id)
    conf_poly <- lapply(locs.lst, function(x) {
      conf <- lapply(1:nrow(x), function(i)
        with(x, elps(x[i], y[i], x.se[i], y.se[i], 90)))
      lapply(conf, function(x)
        st_polygon(list(x))) %>%
        st_multipolygon()
    })
    conf_sf <- st_as_sfc(conf_poly)
    conf_sf <- st_as_sf(conf_sf, crs = st_crs(loc_sf))
    conf_sf$id <- unique(loc_sf$id)
    ## dissolve individual polygons where they overlap one another
    conf_sf <- st_union(conf_sf, by_feature = TRUE)
  } else {
    conf_sf <- NULL
  }

  
  ## get observations & set map extents
  if (!is.na(aes$df[4,2])) {
    obs_sf <- st_transform(grab(x, "data", as_sf = TRUE), crs = crs)
    extents <- st_bbox(obs_sf)
  } else {
    obs_sf <- NULL
    extents <- st_bbox(loc_sf)
  }
  
  extents[c("xmin", "xmax")] <- extendrange(extents[c("xmin", "xmax")], 
                                           f = ext.rng[1])
  extents[c("ymin", "ymax")] <- extendrange(extents[c("ymin", "ymax")], 
                                           f = ext.rng[2])  

  ## select appropriate mapping fn based on x, y inputs
  if(all(is.null(y), nrow(x) == 1, !"g" %in% names(loc_sf))) {
    m <- map_single_track_base(map_type, 
                               obs_sf,
                               conf_sf, 
                               line_sf, 
                               loc_sf, 
                               by.date,
                               extents,
                               aes,
                               ...)
  }
  else if(all(is.null(y), nrow(x) > 1, !"g" %in% names(loc_sf))) {
    m <- map_multi_track_base(map_type, 
                         obs_sf,
                         conf_sf, 
                         line_sf, 
                         loc_sf, 
                         by.id,
                         by.date,
                         extents,
                         aes,
                         ...)
  }
  else if(all(is.null(y), nrow(x) == 1, "g" %in% names(loc_sf))) {
    m <- map_single_track_mp(map_type, 
                             obs_sf,
                             conf_sf, 
                             line_sf, 
                             loc_sf, 
                             extents,
                             aes,
                             ...)
  }
  else if(all(is.null(y), nrow(x) > 1, "g" %in% names(loc_sf))) {
    m <- map_multi_track_mp(map_type, 
                            obs_sf,
                            conf_sf, 
                            line_sf, 
                            loc_sf, 
                            extents,
                            aes,
                            ...)
  }
  else if(all(!is.null(y), nrow(x) == 1, !"g" %in% names(loc_sf))) map_single_track_mpm()
  else if(all(!is.null(y), nrow(x) > 1, !"g" %in% names(loc_sf))) map_multi_track_mpm()
  else if(all(!is.null(y), nrow(x) == 1, "g" %in% names(loc_sf))) {
    warning("multpile move persistence estimates detected for individual animals, mapping move persistence obtained from fit_mpm()\n", 
            immediate. = TRUE)
    map_single_track_mpm()
  }
  else if(all(!is.null(y), nrow(x) > 1, "g" %in% loc_sf)) {
    warning("multpile move persistence estimates detected for individual animals, mapping move persistence obtained from fit_mpm()\n", 
            immediate. = TRUE)
    map_multi_track_mpm()
  }
  
  return(m)
   
}



##' @title aes_lst
##' 
##' @description set aesthetics as a named list for: 1) mapping i) estimated 
##' locations; ii) estimated confidence ellipses; iii) estimated track lines; 
##' iv) observed locations; v) land masses; vi) water bodies; and 2) colour 
##' palettes for i) behavioural indices or ii) individual animal tracks
##' 
##' @param est logical; turns on estimated locations (default = TRUE)
##' @param conf logical; turns on estimated confidence ellipses. Default varies 
##' depending on whether behavioural index is being mapped &/or if single versus
##' multiple tracks are being mapped.
##' @param line logical; turns on estimated track line(s) (default varies)
##' @param mp logical; turns on move persistence index (default = TRUE, if present in model fit)
##' @param obs logical; turns on observed locations (default = FALSE)
##' @param shape gpplot2 shape value (integer: 0, 25) for estimated & observed 
##' locations 
##' @param size ggplot2 size value for estimated locations & track lines, and 
##' observed locations
##' @param col colour for estimated locations and track lines, and observed 
##' locations
##' @param fill fill colour for estimated locations & confidence ellipses, 
##' observed locations, land masses, and water bodies
##' @param alpha transparency for specified fills/colours
##' @param mp_pal continuous colour palette for move persistence values 
##' @param id_pal discrete colour palette for track id's
##' @param date_pal continuous colour palette for displaying date along track
##' @importFrom tibble tibble
##' @export

aes_lst <- function(est = TRUE, conf = TRUE, line = FALSE, mp = TRUE, obs = FALSE,
    shape = c(19, NA, NA, 17, NA, NA),
                   size = c(1, NA, 0.2, 1, NA, NA),
                   col = c("dodgerblue", NA, "grey50", "orange", NA, NA),
                   fill = c("dodgerblue", "dodgerblue", NA, "orange", "grey60", "white"),
                   alpha = c(1, 0.4, 1, 1, 1, NA),
                   mp_pal = hcl.colors(100, palette = "Cividis", rev = FALSE),
                   id_pal = "Harmonic",
                   date_pal = hcl.colors(100, palette = "Viridis", rev = FALSE)
                   ) {
  
  stopifnot("aesthetic vectors must have length = 6
            type aes_lst()$df to see required final aes structure" = 
              all(length(shape) == 6, 
                length(size) == 6, 
                length(col) == 6, 
                length(fill) == 6,
                length(alpha) == 6))
  
  list(est = est,
       conf = conf,
       line = line,
       mp = mp,
       obs = obs,
    df = data.frame(
    feature = c("estimated locations",
                "confindence ellipses",
                "track lines",
                "observed locations",
                "land masses",
                "water bodies"),
    shape = shape,
    size = size,
    col = col,
    fill = fill,
    alpha = alpha
    ),
    mp_pal = mp_pal,
    id_pal = id_pal,
    date_pal = date_pal
  )

}
