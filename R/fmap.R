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
##' @importFrom ggplot2 element_blank scale_colour_viridis_c scale_colour_viridis_d
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_buffer st_crs st_coordinates st_cast
##' @importFrom utils data
##' @importFrom grDevices extendrange grey
##' @importFrom dplyr summarise "%>%" group_by
##' @export

fmap <- function(x,
                     what = c("fitted", "predicted"),
                     obs = FALSE,
                     crs = NULL,
                     ext.rng = c(0.05, 0.05),
                     size = 1,
                     col = "black")
{
  what <- match.arg(what)

  if(inherits(x, "fG_ssm")) {
    if(length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) == 1)
      sf_locs <- grab(x, what=what)

    else if(length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) > 1) {
      stop("individual foieGras fit objects with differing projections not currently supported")
    }

  } else {
    stop("input must be a foieGras ssm fit object with class fG_ssm")
  }

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
    p <- p + geom_sf(data = sf_data, colour = col, size = ifelse(length(size) == 2, size[2], 0.5), shape = 9, alpha = 0.75)

  if(length(unique(x$id)) > 1) {

    p <- p +
      geom_sf(data = sf_lines,
              colour = "dodgerblue",
              size = 0.1
              ) +
      geom_sf(data = sf_locs,
              aes_string(colour = "id"),
              size = ifelse(length(size) == 2, size[1], 1),
              show.legend = "point"
                     ) +
    scale_colour_viridis_d() +
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
      scale_colour_viridis_c(breaks = as.numeric(lab_dates), option = "viridis", labels = lab_dates) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 8, vjust = 0),
            legend.key.width = unit(0.12, "npc"),
            legend.key.height = unit(0.025, "npc")
      )
  }

  return(p)
}
