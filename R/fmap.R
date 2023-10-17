##' @title fmap
##'
##' @description map aniMotum fitted or predicted locations, with or without
##' Argos observations, optionally apply a different projection
##'
##' @param x a \code{aniMotum} ssm fit object with class `ssm_df`
##' @param y optionally, a \code{aniMotum} mpm fit object with class `mpm_df`; 
##' default is NULL
##' @param what specify which location estimates to map: fitted, predicted or
##' rerouted
##' @param conf include confidence regions around estimated location (logical; 
##' default = TRUE, unless y is an mpm fit object then conf is FALSE)
##' @param obs include Argos observations on map (logical; default = FALSE)
##' @param obs.shp point shape for observations (default = 17)
##' @param by.id when mapping multiple tracks, should locations be coloured by
##' id (logical; default = TRUE if nrow(x) > 1 else FALSE)
##' @param by.date when mapping single tracks, should locations be coloured by 
##' date (logical; default = TRUE if nrow(x) == 1 else FALSE)
##' @param crs `proj4string` for re-projecting locations, if NULL the
##' default projection ("+proj=merc") for the fitting the SSM will be used
##' @param ext.rng factors to extend the plot range in x and y dimensions
##' (can exceed 1)
##' @param size size of estimated location points (size = NA will draw no points). 
##' Optionally, a vector of length 2 with size of observed locations given by 2nd 
##' value (ignored if obs = FALSE)
##' @param col.ssm colour of ssm-fitted or ssm-predicted locations (ignored if by.id = TRUE)
##' @param col.obs colour of observed locations (ignored if obs = FALSE)
##' @param lines logical indicating if lines are added to connect estimated 
##' locations (default = FALSE)
##' @param landfill colour to use for land (default = grey(0.6))
##' @param map_type background map type (default = NULL, which uses rnaturalearth 
##' to add landmasses); if packages \code{ggspatial} & \code{rosm} are installed 
##' then any map type returned by [rosm::osm.types] can be used for a more 
##' detailed map background.
##' @param pal [hcl.colors] palette to use (default: "Cividis"; type 
##' [hcl.pals()] for options)
##' @param rev reverse colour palette (logical)
##' @param last_loc colour to render last location of each track (default = NULL)
##' @param ... additional arguments passed to [ggspatial::annotation_map_tile]
##' @export

fmap <- function(x,
                 y = NULL,
                 what = c("fitted", "predicted", "rerouted"),
                 conf = TRUE,
                 obs = FALSE,
                 obs.shp = 17,
                 by.id = TRUE,
                 by.date = TRUE,
                 crs = NULL,
                 ext.rng = c(0.05, 0.05),
                 size = 0.25,
                 col.ssm = "dodgerblue",
                 col.obs = "black",
                 lines = FALSE,
                 landfill = grey(0.6),
                 map_type = "default",
                 pal = "Cividis",
                 rev = FALSE, 
                 last_loc = NULL,
                 ...) {
  
  cat(.Deprecated("map", 
              package = "aniMotum", 
              msg = "As of aniMotum 1.0-5, 'fmap' is deprecated. \nUse 'map' instead."))
  
  if(FALSE) {
    what <- match.arg(what)
    
    if (!inherits(x, "ssm_df"))
      stop("x must be a aniMotum ssm fit object with class `ssm_df`")
    if (!inherits(y, "mpm_df") & !is.null(y))
      stop("y must either be NULL or a aniMotum mpm fit object with class `mpm_df`")
    if (map_type != "default" &
        !(
          requireNamespace("rosm", quietly = TRUE) |
          requireNamespace("ggspatial", quietly = TRUE)
        )) {
      cat(
        "required packages `rosm` and/or `ggspatial` are not installed, switching map_type to default\n"
      )
      map_type <- "default"
    }
    
    if (inherits(x, "ssm_df")) {
      if (length(unique(sapply(x$ssm, function(.)
        st_crs(.$predicted)$epsg))) == 1) {
        if (!is.null(y) & !"g" %in% names(x)) {
          conf <- FALSE
          ## increase geom_point size if left at default and y is supplied
          if (size[1] == 0.25)
            size[1] <- 1
          sf_locs <-
            try(join(x, y, what.ssm = what, as_sf = TRUE), silent = TRUE)
          if (inherits(sf_locs, "try-error"))
            stop(
              "number of rows in ssm object do not match the number of rows in mpm object, try modifying the `what` argument"
            )
        } else if (is.null(y)) {
          sf_locs <- grab(x, what = what, as_sf = TRUE)
          if (!is.null(last_loc)) {
            sf_locs_last <- sf_locs %>%
              group_by(id) %>%
              filter(date == max(date))
          }
          if (conf)
            locs <- grab(x, what = what, as_sf = FALSE)
        } else if (!is.null(y) & "g" %in% names(x)) {
          stop("I'm not sure which move persistence estimates you want to use")
        }
        
      } else if (length(unique(sapply(x$ssm, function(.)
        st_crs(.$predicted)$epsg))) > 1) {
        stop(
          "individual aniMotum ssm fit objects with differing projections not currently supported"
        )
      }
      
    }
    
    if (conf) {
      locs.lst <- split(locs, locs$id)
      conf_poly <- lapply(locs.lst, function(x) {
        conf <- lapply(1:nrow(x), function(i)
          with(x, elps(x[i], y[i], x.se[i], y.se[i], 90)))
        lapply(conf, function(x)
          st_polygon(list(x))) %>%
          st_multipolygon()
      })
      sf_conf <- st_as_sfc(conf_poly)
      sf_conf <- st_as_sf(sf_conf, crs = st_crs(sf_locs))
      sf_conf$id <- unique(locs$id)
      ## dissolve individual polygons where they overlap one another
      sf_conf <- st_union(sf_conf, by_feature = TRUE)
    }
    
    if (is.null(crs))
      crs <- st_crs(sf_locs)
    else {
      if (!is.character(crs))
        stop(
          "\ncrs must be a proj4string,
                                \neg. `+proj=stere +lat_0=-90 +lon_0=0 +datum=WGS84 +units=km +no_defs`"
        )
      
      if (length(grep("+units=km", crs, fixed = TRUE)) == 0) {
        cat("\nconverting units from m to km to match SSM output")
        crs <- paste(crs, "+units=km")
      }
      sf_locs <- st_transform(sf_locs, crs = crs)
      if (!is.null(last_loc))
        sf_locs_last <- st_transform(sf_locs_last, crs = crs)
      if (conf)
        sf_conf <- st_transform(sf_conf, crs = crs)
    }
    
    if (obs) {
      sf_data <- st_transform(grab(x, "data", as_sf = TRUE), crs = crs)
      bounds <- st_bbox(sf_data)
    } else {
      bounds <- st_bbox(sf_locs)
    }
    
    bounds[c("xmin", "xmax")] <-
      extendrange(bounds[c("xmin", "xmax")], f = ext.rng[1])
    bounds[c("ymin", "ymax")] <-
      extendrange(bounds[c("ymin", "ymax")], f = ext.rng[2])
    
    if (lines) {
      sf_lines <- group_by(sf_locs, id)
      sf_lines <- summarise(sf_lines, do_union = FALSE)
      sf_lines <- st_cast(sf_lines, "MULTILINESTRING")
    }
    
    ## get worldmap
    if (map_type == "default") {
      if (requireNamespace("rnaturalearthdata", quietly = TRUE)) {
        wm <- ne_countries(scale = 50, returnclass = "sf")
        wm <- st_transform(wm, crs = crs)
        wm <- st_make_valid(wm)
      } else {
        wm <- ne_countries(scale = 110, returnclass = "sf")
        wm <- st_transform(wm, crs = crs)
        wm <- st_make_valid(wm)
      }
      
      p <- ggplot() +
        geom_sf(data = wm,
                fill = landfill,
                lwd = 0)
      
    } else {
      if (requireNamespace("ggspatial", quietly = TRUE) &
          requireNamespace("rosm", quietly = TRUE)) {
        p <- ggplot() +
          ggspatial::annotation_map_tile(type = map_type, ...)
      }
    }
    
    if (obs) {
      if (length(size) == 1) {
        cat(paste0(
          "geom size not specified for observations, using 'size = c(",
          size,
          ", 0.8)'"
        ))
        size <- c(size, 0.8)
      }
      p <-
        p + geom_sf(
          data = sf_data,
          colour = col.obs,
          size = size[2],
          shape = obs.shp,
          alpha = 0.75
        )
      
    }
    
    if (nrow(x) > 1) {
      if (conf) {
        p <- p + geom_sf(
          data = sf_conf,
          aes_string(fill = "id"),
          colour = NA,
          lwd = 0,
          alpha = 0.4,
          show.legend = ifelse(!lines &
                                 is.na(size), TRUE, FALSE)
        )
      }
      
      if (is.null(y)) {
        if (lines & is.na(size)[1]) {
          if (by.id) {
            p <- p + geom_sf(
              data = sf_lines,
              aes_string(colour = "id"),
              lwd = 0.25,
              show.legend = "line"
            )
          } else {
            p <- p + geom_sf(
              data = sf_lines,
              colour = col.ssm,
              lwd = 0.25,
              show.legend = "line"
            )
          }
        } else if (lines & !is.na(size)[1]) {
          if (by.id) {
            p <- p + geom_sf(data = sf_lines,
                             aes_string(colour = "id"),
                             lwd = 0.25) +
              geom_sf(
                data = sf_locs,
                aes_string(colour = "id"),
                size = ifelse(length(size) == 2, size[1], size),
                show.legend = "point"
              )
            
          } else {
            p <- p + geom_sf(data = sf_lines,
                             colour = col.ssm,
                             lwd = 0.25) +
              geom_sf(
                data = sf_locs,
                colour = col.ssm,
                size = ifelse(length(size) == 2, size[1], size),
                show.legend = "point"
              )
            
          }
        } else if (!lines & !is.na(size)[1]) {
          if (by.id) {
            p <- p + geom_sf(
              data = sf_locs,
              aes_string(colour = "id"),
              size = ifelse(length(size) == 2, size[1], size),
              show.legend = "point"
            ) +
              scale_colour_manual(values =
                                    hcl.colors(
                                      n = nrow(x),
                                      palette = pal,
                                      rev = rev
                                    ))
            
          } else {
            p <- p + geom_sf(
              data = sf_locs,
              colour = col.ssm,
              size = ifelse(length(size) == 2, size[1], size),
              show.legend = "point"
            )
            
          }
        }
        
        if (!is.null(last_loc)) {
          p <- p + geom_sf(data = sf_locs_last,
                           size = 1,
                           colour = last_loc)
        }
        
      } else if (!is.null(y)) {
        if (lines) {
          p <- p + geom_sf(data = sf_lines,
                           colour = grey(0.6),
                           lwd = 0.25)
          
        }
        
        p <- p + geom_sf(
          data = sf_locs,
          aes_string(colour = "g"),
          size = ifelse(length(size) == 2, size[1], size)
        ) +
          scale_colour_gradientn(
            colours =
              hcl.colors(
                n = 100,
                palette = pal,
                rev = rev
              ),
            name = expression(gamma[t]),
            limits = c(0, 1)
          )
        
      }
      
      p <- p + theme_minimal() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 8, vjust = 0))
      
      if (conf) {
        p <- p + scale_fill_manual(values =
                                     hcl.colors(
                                       n = nrow(x),
                                       palette = pal,
                                       rev = rev
                                     ))
      }
      
    } else if (nrow(x) == 1) {
      ##FIXME:: need to add conf ellipses here too but not sure how to colour them,
      ##FIXME:: given the track is coloured by date - perhaps make this optional but
      ##FIXME:: if on then conf ellipse is a single colour, or colour ellipses by date
      ##FIXME:: but don't dissolve them with st_union, otherwise colouring by date won't work...
      if (is.null(y)) {
        if (by.date) {
          lab_dates <-
            with(sf_locs, pretty(seq(min(date), max(date), l = 5))) %>% as.Date()
        }
        
        if (conf & !by.date) {
          p <- p + geom_sf(
            data = sf_conf,
            fill = "#78B7C5",
            colour = NA,
            lwd = 0,
            alpha = 0.5
          )
          
        } else if (conf & by.date) {
          p <- p + geom_sf(
            data = sf_conf,
            fill = grey(0.5),
            colour = NA,
            lwd = 0,
            alpha = 0.25
          )
        }
        
        if (by.date) {
          if (lines) {
            p <- p + geom_sf(
              data = sf_lines,
              colour = grey(0.5),
              alpha = 0.75,
              lwd = 0.25
            )
          }
          if (!is.na(size)[1]) {
            p <- p + geom_sf(data = sf_locs,
                             aes(colour = as.numeric(as.Date(date))),
                             size = size[1]) +
              scale_colour_gradientn(
                breaks = as.numeric(lab_dates),
                colours = hcl.colors(
                  n = 100,
                  palette = pal,
                  rev = rev
                ),
                labels = lab_dates
              )
          }
          
          p <- p + labs(title = paste("id:", x$id)) +
            theme_minimal() +
            theme(
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = 8, vjust = 0),
              legend.key.width = unit(0.12, "npc"),
              legend.key.height = unit(0.025, "npc"),
              panel.grid = element_line(size = 0.2)
            )
        } else if (!by.date) {
          if (lines) {
            p <- p + geom_sf(
              data = sf_lines,
              colour = hcl.colors(n = 5,
                                  palette = pal)[4],
              alpha = 0.75,
              lwd = 0.25
            )
          }
          if (!is.na(size)[1]) {
            p <- p + geom_sf(
              data = sf_locs,
              colour = hcl.colors(n = 5,
                                  palette = pal)[3],
              size = size[1]
            )
          }
          
          p <- p + labs(title = paste("id:", x$id)) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid = element_line(size = 0.2))
        }
        
      } else if (!is.null(y)) {
        if (lines) {
          p <- p + geom_sf(
            data = sf_lines,
            colour = hcl.colors(n = 5,
                                palette = pal)[4],
            alpha = 0.75,
            lwd = 0.25
          )
        }
        p <- p + geom_sf(
          data = sf_locs,
          aes_string(colour = "g"),
          size = ifelse(length(size) == 2, size[1], size)
        ) +
          scale_colour_gradientn(
            colours =
              hcl.colors(
                n = 100,
                palette = pal,
                rev = rev
              ),
            name = expression(gamma[t]),
            limits = c(0, 1)
          )
        
        p <- p + labs(title = paste("id:", x$id)) +
          theme_minimal() +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8, vjust = 0))
      }
      
      
    }
    #  if(map_type != "default") {
    p <- p +
      coord_sf(
        xlim = c(bounds["xmin"], bounds["xmax"]),
        ylim = c(bounds["ymin"], bounds["ymax"]),
        crs = crs
      )
    #  }
    
    return(p)
  }
}
