##' @title map_single_track_base
##'
##' @description map foieGras-estimated locations from a single track without
##' a behavioural index
##' 
##' @param map_type coastline data to be used in maps
##' @param obs_sf observed location geometry
##' @param conf_sf confidence ellipses around estimated locations, if specified
##' @param line_sf track line, if specified
##' @param loc_sf estimated location geometry
##' @param by.date colour estimated locations by date (logical)
##' @param extents map extents
##' @param buffer distance (in km) to buffer locations for subsetting land 
##' polygons (default = 10000). If map extents are expanded by many factors then
##' the buffer distance may need to be increased, otherwise this should not be
##' used.
##' @param aes a tibble of map aesthetics (size, shape, col, fill, alpha) to
##' be applied, in order, to: 1) estimated locations,; 2) confidence ellipses; 
##' 3) track lines; 4) observed locations; 5) land regions; 6) water regions
##' @param silent logical; map silently (default = FALSE)
##' @param ... additional arguments passed to [ggspatial::annotation_map_tile]
##' @importFrom ggplot2 ggplot geom_sf aes aes_string ggtitle xlim ylim unit 
##' @importFrom ggplot2 element_text theme scale_fill_gradientn scale_fill_manual 
##' @importFrom ggplot2 element_blank scale_colour_manual scale_colour_gradientn
##' @importFrom ggplot2 element_rect coord_sf
##' @importFrom rnaturalearth ne_countries
##' @importFrom sf st_union st_convex_hull st_intersection st_collection_extract 
##' @importFrom sf st_sf st_crs st_make_valid
##' @importFrom dplyr "%>%"
##' 
##' @keywords internal
map_single_track_base <- function(map_type, 
                                  obs_sf,
                                  conf_sf, 
                                  line_sf, 
                                  loc_sf, 
                                  by.date,
                                  extents,
                                  buffer,
                                  aes,
                                  silent,
                                  ...) {
  
  ## if input aes is identical to default aes_df() then plot components
  if(identical(aes, aes_lst())) {
    aes$obs <- FALSE
    aes$line <- FALSE
  }
  
  if (by.date) {
    lab_dates <-
      with(loc_sf, pretty(seq(min(date), max(date), l = 10), n = 5)) %>% as.Date()
  }
  
  prj <- st_crs(loc_sf)
  
  ## get worldmap
  if (map_type == "default") {
    if (requireNamespace("rnaturalearthhires", quietly = TRUE)) {
      wm <- ne_countries(scale = 10, returnclass = "sf") %>%
        st_transform(crs = prj) %>%
        st_make_valid()
      if(!silent) cat("using map scale: 10\n")
    } else {
      wm <- ne_countries(scale = 50, returnclass = "sf") %>%
        st_transform(crs = prj) %>%
        st_make_valid()
      if(!silent) cat("using map scale 50\n")
    }

    ## define map region & clip land polygons
    if(!is.null(obs_sf)) pts <- obs_sf
    else pts <- loc_sf
    land <- suppressWarnings(st_buffer(pts, dist = buffer) %>% 
                               st_union() %>% 
                               st_convex_hull() %>% 
                               st_intersection(wm) %>% 
                               st_collection_extract('POLYGON') %>% 
                               st_sf() %>%
      st_make_valid())
    
    p <- ggplot() + 
      geom_sf(data = land,
              fill = aes$df$fill[5],
              colour = aes$df$col[5],
              alpha = aes$df$alpha[5])
  }
  
  else {
    ## use OSM map tiles if pkg's installed & map_type != "default"
    if (requireNamespace("ggspatial", quietly = TRUE) &
        requireNamespace("rosm", quietly = TRUE)) {
      p <- ggplot() +
        ggspatial::annotation_map_tile(type = map_type, ...)
    }
  }
  
  ## map observations
  if (aes$obs) {
    p <- p +
      geom_sf(
        data = obs_sf,
        colour = aes$df$col[4],
        size = aes$df$size[4],
        stroke = 0.2,
        shape = aes$df$shape[4],
        alpha = aes$df$alpha[4]
      )
  }
  
  ## map confidence ellipses
  if (aes$conf) {
    p <- p +
      geom_sf(
        data = conf_sf,
        fill = aes$df$fill[2],
        stroke = 0,
        lwd = 0,
        alpha = aes$df$alpha[2]
      )
  }
  
  ## map estimated track lines
  if(aes$line & !by.date) {
    p <- p +
      geom_sf(
        data = line_sf,
        colour = aes$df$col[3],
        size = aes$df$size[3]
      )
  } else if(aes$line & by.date) {
    p <- p +
      geom_sf(
        data = line_sf,
        aes(colour = as.numeric(as.Date(date))),
        size = aes$df$size[3]
      )
    if(!aes$est) {
      p <- p +
        scale_colour_gradientn(breaks = as.numeric(lab_dates), 
                               colours = aes$date_pal, 
                               labels = lab_dates,
                               limits = range(lab_dates))
    }
  }
  
  ## map estimated locs
  if (aes$est & !by.date) {
    p <- p +
      geom_sf(
        data = loc_sf,
        colour = aes$df$col[1],
        size = aes$df$size[1],
        stroke = 0.2,
        shape = aes$df$shape[1],
        alpha = aes$df$alpha[1]
      )
  } else if(all(aes$est, by.date)) {
    p <- p +
      geom_sf(
        data = loc_sf,
        aes(colour = as.numeric(as.Date(date))),
        size = aes$df$size[1],
        stroke = 0.2,
        shape = aes$df$shape[1],
        alpha = aes$df$alpha[1]
      ) +
      scale_colour_gradientn(breaks = as.numeric(lab_dates), 
                             colours = aes$date_pal, 
                             labels = lab_dates,
                             limits = range(lab_dates))
      
  }
  
  ## enforce map extents
  p <- p +
    coord_sf(xlim = c(extents["xmin"], extents["xmax"]),
             ylim = c(extents["ymin"], extents["ymax"]), 
             crs = prj, # req'd for rosm baselayers
             default_crs = prj,
             expand = FALSE)
  
  ## set plot theme stuff
  p <- p + theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 8, vjust = 0),
          legend.key.width = unit(0.1, "npc"),
          legend.key.height = unit(0.02, "npc"),
          panel.background = element_rect(fill = aes$df$fill[6], colour = NA)
          )
  
  return(p)
}

