##' @title map
##'
##' @description map aniMotum-estimated locations and behavioural indices with
##' coastline and projection options
##'
##' @param x a `aniMotum` ssm fit object with class `ssm_df` or (old) `fG_ssm`;
##' or [fit_mpm()] output (class `mpm_df`, aniMotum 1.3-1 on), e.g. a move
##' persistence model fitted directly to Kinéis locations, mapped from its
##' own track locations. An `mpm_df` map has no confidence ellipses or
##' observed locations
##' @param y optionally, a `aniMotum` mpm fit object with class `mpm_df` or (old)
##' `fG_mpm`, fitted to the locations of the ssm fit `x`. Ignored when `x` is
##' an `mpm_df`
##' @param what specify which location estimates to map: fitted, predicted or
##' rerouted. If not given and `y` is supplied, the locations the move
##' persistence model was fitted to; otherwise fitted
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
##' @param buffer distance (in km) to buffer locations for subsetting land 
##' polygons (default = 10000). If map extents are expanded by many factors then
##' the buffer distance may need to be increased, otherwise this should not be
##' used. Ignored if `map_type != "default"`.
##' @param map_type background map type ("default" uses [rnaturalearth::ne_countries] 
##' to add land polygons). If the `rnaturalearthdata` package is installed then 
##' high-resolution land polygons will be used. If the `ggspatial`
##' and `rosm` packages are installed then any tile map type returned by 
##' [rosm::osm.types] can be used for a potentially more detailed coastline at 
##' fine spatial scales, given appropriate zoom settings 
##' (see [ggspatial::annotation_map_tile] for details).
##' @param normalise logical; if output includes a move persistence estimate, 
##' should g (the move persistence index) be normalised to have minimum = 0 and 
##' maximum = 1 (default = TRUE). 
##' @param group logical; should g be normalised among individuals as a group, 
##' a 'relative g', or separately to highlight regions of lowest and highest move
##' persistence along a track (default = FALSE).
##' @param silent logical; generate maps silently (default = FALSE).
##' @param ... additional arguments passed to [ggspatial::annotation_map_tile]
##' 
##' @return a map as a ggplot2 object
##' 
##' @importFrom ggplot2 ggplot geom_sf aes ggtitle xlim ylim unit 
##' @importFrom ggplot2 element_text theme  scale_fill_gradientn scale_fill_manual 
##' @importFrom ggplot2 element_blank scale_colour_manual scale_colour_gradientn
##' @importFrom ggplot2 coord_sf
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_as_sfc st_buffer st_make_valid
##' @importFrom sf st_crs st_coordinates st_cast st_multipolygon st_polygon st_union
##' @importFrom utils data
##' @importFrom grDevices extendrange grey
##' @importFrom dplyr group_by summarise
##' @importFrom grDevices hcl.colors
##' 
##' @examples 
##' # create an ssm fit object
##' 
##' fit <- fit_ssm(ellie, model = "rw", time.step = 24, control = ssm_control(verbose = 0))
##' 
##' # render default map
##' 
##' map(fit, what = "p")
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
                buffer = 10000, 
                map_type = "default",
                normalise = TRUE,
                group = FALSE,
                silent = FALSE,
                ...) {

  ## fit_mpm() output (1.3-1 on) records which ssm locations it was fitted
  ##  to: map those unless `what` is given
  if (missing(what) && inherits(y, "mpm_df") && !is.data.frame(y) &&
      identical(y$source, "ssm")) what <- y$what
  what <- match.arg(what)
  ## fit_mpm() output (1.3-1 on) as x: mapped from the track locations it holds
  mpm.x <- inherits(x, "mpm_df") && !is.data.frame(x)
  stopifnot("x must be a aniMotum ssm fit object with class `ssm_df`, or fit_mpm() output with class `mpm_df`" = 
              any(inherits(x, "ssm_df"), inherits(x, "fG_ssm"), mpm.x))
  stopifnot("y must either be NULL or a aniMotum mpm fit object with class `mpm_df`" = 
              any(inherits(y, "mpm_df"), inherits(y, "fG_mpm"), is.null(y)))
  if (mpm.x && !is.null(y)) {
    warning("`y` ignored: x is fit_mpm() output, which holds its own locations", call. = FALSE)
    y <- NULL
  }
  if (!mpm.x)
    stopifnot("individual `ssm` fit objects with differing projections not currently supported" = 
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
  if (mpm.x) {
    loc_sf <- mpm_map_locations(x, normalise, group)
    ## no location standard errors or observed locations in an mpm fit
    aes$conf <- FALSE
    aes$obs <- FALSE
  } else if (all(what == "rerouted", !"rerouted" %in% names(x$ssm[[1]]))){
    stop("what = 'rerouted' can not be used as model fits do not have rerouted paths")
  } else {
    loc_sf <- grab(x, what = what, as_sf = TRUE, normalise = normalise, group = group)
  }
  
  ## handle mpm fits if present
  if(!is.null(y)) {
    loc_sf <- join(x, y, what.ssm = what, as_sf = TRUE, normalise = normalise, group = group)
  }
  
  if(!is.null(crs)) {
    if (length(grep("+units=km", crs, fixed = TRUE)) == 0) {
      cat("converting projection units from m to km to match SSM output")
      crs <- paste(crs, "+units=km")
    }
    loc_sf <- st_transform(loc_sf, crs = crs)
  } else {
    crs <- st_crs(loc_sf)
  }

  ## generate track lines & cast to MULTILINESTRING for plot efficiency
  if (aes$line) {
    ## one line per track segment, so lines do not cross data gaps
    line_sf <- if ("segment" %in% names(loc_sf)) group_by(loc_sf, id, segment) else group_by(loc_sf, id)
    line_sf <- summarise(line_sf, do_union = FALSE)
    line_sf <- st_cast(line_sf, "MULTILINESTRING")
  } else {
    line_sf <- NULL
  }

  ## calc confidence ellipses around estimated locations & dissolve overlapping segments
  if(aes$conf) {
    locs <- st_coordinates(loc_sf)
    locs <- data.frame(id = loc_sf$id, x = locs[,1], y = locs[,2], x.se = loc_sf$x.se, y.se = loc_sf$y.se)
    ## Kinéis-only tracks: widen from the 1-standard-error calibration to the
    ##  joint 95% ellipse calibration (heavy-tailed errors); 1 for all other
    ##  tracks
    ci.scale <- kineis_ci_scale(x, locs$id, type = "ellipse")
    locs$x.se <- locs$x.se * ci.scale
    locs$y.se <- locs$y.se * ci.scale
    ## check for NA's in se estimates
    if(any(is.na(locs$x.se),is.na(locs$y.se))) {
      aes$conf <- FALSE
      message("NA's detected in location standard errors, can not render confidence ellipses")
    }
  }
  if(aes$conf) {
    locs.lst <- split(locs, locs$id)
    conf_poly <- lapply(locs.lst, function(x) {
      conf <- lapply(1:nrow(x), function(j)
        with(x, elps(x[j], y[j], x.se[j], y.se[j], 90)))
      lapply(conf, function(x)
        st_polygon(list(x))) |>
        st_multipolygon()
    })

    conf_sf <- st_as_sfc(conf_poly)
    conf_sf <- st_as_sf(conf_sf, crs = st_crs(loc_sf))
    conf_sf$id <- unique(loc_sf$id)
    ## dissolve individual polygons where they overlap one another
    conf_sf <- st_union(conf_sf, by_feature = TRUE)
    conf_sf <- st_make_valid(conf_sf)
  } else {
    conf_sf <- NULL
  }

  
  ## get observations & set map extents
  if (aes$obs) {
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
  n.track <- length(unique(loc_sf$id))
  if(all(n.track == 1, any(!"g" %in% names(loc_sf), !aes$mp))) {
    m <- map_single_track_base(map_type, 
                               obs_sf,
                               conf_sf, 
                               line_sf, 
                               loc_sf, 
                               by.date,
                               extents,
                               buffer,
                               aes,
                               silent,
                               ...)
  }
  else if(all(n.track > 1, any(!"g" %in% names(loc_sf), !aes$mp))) {
    m <- map_multi_track_base(map_type, 
                         obs_sf,
                         conf_sf, 
                         line_sf, 
                         loc_sf, 
                         by.id,
                         by.date,
                         extents,
                         buffer, 
                         aes,
                         silent,
                         ...)
  }
  else if(all(n.track == 1, "g" %in% names(loc_sf), aes$mp)) {
    m <- map_single_track_mp(map_type, 
                             obs_sf,
                             conf_sf, 
                             line_sf, 
                             loc_sf, 
                             extents,
                             buffer,
                             aes,
                             silent,
                             ...)
  }
  else if(all(n.track > 1, "g" %in% names(loc_sf), aes$mp)) {
    m <- map_multi_track_mp(map_type, 
                            obs_sf,
                            conf_sf, 
                            line_sf, 
                            loc_sf, 
                            extents,
                            buffer,
                            aes,
                            silent,
                            ...)
  }
  
  return(m)
   
}

## track locations of fit_mpm() output (1.3-1 on) as projected sf points, for
##  map(). Mercator (km), centred on 180 degrees if a track crosses it, as
##  for SSM fits
mpm_map_locations <- function(x, normalise, group) {
  d <- grab(x, "fitted", normalise = normalise, group = group)
  if (all(is.na(d$lon)))
    stop("no longitude and latitude to map: the move persistence model was fitted to projected coordinates",
         call. = FALSE)
  d <- d[!is.na(d$lon) & !is.na(d$lat), ]
  cross <- any(vapply(split(d$lon, d$id), function(lon)
    any(abs(diff(wrap_lon(lon, -180))) > 300), logical(1)))
  prj <- paste0("+proj=merc +lon_0=", if (cross) 180 else 0,
                " +datum=WGS84 +units=km +no_defs")
  st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE), st_crs(prj))
}
