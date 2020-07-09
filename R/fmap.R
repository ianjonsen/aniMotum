##' @title fmap
##'
##' @description map foieGras fitted or predicted locations, with or without
##' Argos observations, optionally apply a different projection
##'
##' @param x a foieGras fitted object
##' @param what specify which location estimates to map: fitted or predicted
##' @param obs include Argos observations on map (logical)
##' (logical); ignored if `obs = FALSE`
##' @param crs `proj4string` for re-projecting locations, if NULL the
##' default projection ("+proj=merc") for the fitting the SSM will be used
##' @param ext.rng factors to extend the plot range in x and y dimensions
##' (can exceed 1)
##' @param size size of estimated location points; optionally a vector of length 2, with size of observed locations given by 2nd value
##' @param col colour of observed locations (ignored if obs = FALSE)
##' @importFrom ggplot2 ggplot geom_sf aes aes_string ggtitle xlim ylim unit element_text theme 
##' @importFrom ggplot2 element_blank scale_colour_manual scale_colour_gradientn scale_fill_manual
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_as_sfc st_buffer st_crs st_coordinates st_cast st_multipolygon st_polygon st_union
##' @importFrom utils data
##' @importFrom grDevices extendrange grey
##' @importFrom dplyr summarise "%>%" group_by mutate
##' @importFrom wesanderson wes_palette
##' @export

fmap <- function(x,
                     what = c("fitted", "predicted"),
                     obs = FALSE,
                     crs = NULL,
                     ext.rng = c(0.05, 0.05),
                     size = 0.75,
                     col = "black")
{
  what <- match.arg(what)

  if(inherits(x, "fG_ssm")) {
    if(length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) == 1) {
      sf_locs <- grab(x, what=what)
      locs <- grab(x, what=what, as_sf = FALSE)
      
    } else if(length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) > 1) {
      stop("individual foieGras fit objects with differing projections not currently supported")
    }

  } else {
    stop("input must be a foieGras ssm fit object with class fG_ssm")
  }
    
  locs.lst <- split(locs, locs$id)
  conf_poly <- lapply(locs.lst, function(x) {
    conf <- lapply(1:nrow(x), function(i)
      with(x, elps(x[i], y[i], x.se[i], y.se[i], 90))
    )
    lapply(conf, function(x) st_polygon(list(x))) %>%
      st_multipolygon()
  })
  sf_conf <- st_as_sfc(conf_poly) %>%
    st_as_sf(crs = st_crs(sf_locs)) %>%
    mutate(id = unique(locs$id)) %>%
    st_union(by_feature = TRUE) ## dissolve individual polygons where they overlap one another
    
  if (is.null(crs)) prj <- st_crs(sf_locs)
  else {
    prj <- crs
    if(!is.character(prj)) stop("\ncrs must be a proj4string, 
                                \neg. `+proj=stere +lat_0=-90 +lon_0=0 +ellps=WGS84 +units=km +no_defs`")
    #prj <- paste0("+init=epsg:", prj)
    
    if(length(grep("+units=km", prj, fixed = TRUE)) == 0) {
      cat("\nconverting units from m to km to match SSM output")
      prj <- paste(prj, "+units=km")
    }
    sf_locs <- st_transform(sf_locs, crs = prj)
    sf_conf <- st_transform(sf_conf, crs = prj)
  }
  
  if (obs) {
    sf_data <- grab(x, "data") %>%
      st_transform(., crs = prj)
    bounds <- st_bbox(sf_data)
  } else {
    bounds <- st_bbox(sf_locs)
  }
 
  bounds[c("xmin","xmax")] <- extendrange(bounds[c("xmin","xmax")], f = ext.rng[1])
  bounds[c("ymin","ymax")] <- extendrange(bounds[c("ymin","ymax")], f = ext.rng[2])

  sf_lines <- sf_locs %>%
    group_by(id) %>%
    summarise(do_union = FALSE) %>%
    st_cast("MULTILINESTRING")

  ## get coastline
  coast <- sf::st_as_sf(rworldmap::countriesLow) %>%
    st_transform(crs = prj)
  
  p <- ggplot() +
    geom_sf(data = coast,
            fill = grey(0.6),
            lwd=0) +
    xlim(bounds[c("xmin","xmax")]) +
    ylim(bounds[c("ymin","ymax")])

  if(obs)
    p <- p + geom_sf(data = sf_data, colour = col, size = ifelse(length(size) == 2, size[2], size), shape = 9, alpha = 0.75)

  if(nrow(x) > 1) {
    p <- p +
      geom_sf(data = sf_conf, aes_string(fill = "id"), colour = NA, lwd = 0, alpha = 0.4, show.legend = FALSE) +
      geom_sf(data = sf_lines,
              colour = "dodgerblue",
              size = 0.1
              ) +
      geom_sf(data = sf_locs,
              aes_string(colour = "id"),
              size = ifelse(length(size) == 2, size[1], size),
              show.legend = "point"
                     ) +
      scale_colour_manual(values = wes_palette(name = "Zissou1", n = nrow(x), type = "continuous")) +
      scale_fill_manual(values = wes_palette(name = "Zissou1", n = nrow(x), type = "continuous")) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8, vjust = 0)
      )
  } else {
    lab_dates <- with(sf_locs, pretty(seq(min(date), max(date), l = 4))) %>% as.Date()

    p <- p +
      geom_sf(data = sf_lines,
                     colour = "dodgerblue",
                     size = 0.1) +
      geom_sf(data = sf_locs,
                    aes(colour = as.numeric(as.Date(date))),
                     size = ifelse(length(size) == 2, size[1], 1)
                     ) +
      scale_colour_gradientn(breaks = as.numeric(lab_dates), colours = wes_palette(name = "Zissou1", type = "continuous"), labels = lab_dates) +
      labs(title = paste("id:", x$id)) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 8, vjust = 0),
            legend.key.width = unit(0.12, "npc"),
            legend.key.height = unit(0.025, "npc")
      )
  }

  return(p)
}
