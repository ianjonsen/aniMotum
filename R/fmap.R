##' @title fmap
##'
##' @description map foieGras fitted or predicted locations, with or without
##' Argos observations, optionally apply a different projection
##'
##' @param x a foieGras fitted object
##' @param what specify which location estimates to map: fitted or predicted
##' @param obs include Argos observations on map (logical)
##' (logical); ignored if `obs = FALSE`
##' @param crs `proj4string` or `epsg` for reprojecting locations, if NULL the
##' default projection (eg. 3395) for the fitting the SSM will be used
##' @param ext.rng factors to extend the plot range in x and y dimensions
##' (can exceed 1)
##' @param size size of estimated location points
##' @importFrom ggplot2 ggplot geom_sf aes aes_string ggtitle xlim ylim unit element_text theme element_blank scale_colour_viridis_c scale_colour_viridis_d
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_buffer st_crs st_coordinates st_cast
##' @importFrom utils data
##' @importFrom grDevices extendrange grey
##' @importFrom dplyr summarise "%>%"
##' @export

fmap <- function(x,
                     what = c("fitted", "predicted"),
                     obs = FALSE,
                     crs = NULL,
                     ext.rng = c(0.05, 0.05),
                     size = 1)
{
  what <- match.arg(what)

  if(all(class(x) %in% c("foieGras", "list"))) {
    switch(what,
         fitted = {
           sf_locs <- x$fitted
         },
         predicted = {
           sf_locs <- x$predicted
         })
  } else if(class(x)[1] == "sf"){
    sf_locs <- x
    what <- substitute(x)

  } else if(all(class(x) %in% c("fG", "tbl_df", "tbl", "data.frame"))) {
    if(length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) == 1)
      sf_locs <- grab(x, what=what)

    else if(length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) > 1) {
      stop("individual fit objects with differing projections not currently supported by `quickmap()`")
    }

  } else {
    stop("input must be either an `fG` compound tibble, an individual `foieGras` fit object, or the output of `foieGras::grab()`")
  }

  if(is.null(crs)) {
    prj <- st_crs(sf_locs)
    if(class(x)[1] == "foieGras")
      sf_data <- x$data
    else if(class(x)[1] == "fG")
      sf_data <- grab(x, "data")
    } else {
    sf_locs <- sf_locs %>% st_transform(., crs)
    prj <- st_crs(sf_locs)
    if(class(x)[1] == "foieGras")
      sf_data <- x$data %>% st_transform(., crs)
    else if(class(x)[1] == "fG")
      sf_data <- grab(x, "data") %>% st_transform(., crs)
    }

  if(obs) bounds <- st_bbox(sf_data)
  else bounds <- st_bbox(sf_locs)
  bounds[c("xmin","xmax")] <- extendrange(bounds[c("xmin","xmax")], f = ext.rng[1])
  bounds[c("ymin","ymax")] <- extendrange(bounds[c("ymin","ymax")], f = ext.rng[2])

  sf_lines <- sf_locs %>%
    group_by(id) %>%
    summarise(do_union = FALSE) %>%
    st_cast("MULTILINESTRING")

  ## get coastline
  coast <- rnaturalearth::ne_countries(scale=10, returnclass = "sf") %>%
    st_transform(crs = prj)

  p <- ggplot() +
    geom_sf(data = coast,
            fill = grey(0.6),
            lwd=0) +
    xlim(bounds[c("xmin","xmax")]) +
    ylim(bounds[c("ymin","ymax")])

  if(obs)
    p <- p + geom_sf(data = sf_data, colour = "orange2", size = 1, shape = 9, alpha = 0.75)

  if(length(unique(x$id)) > 1) {

    p <- p +
      geom_sf(data = sf_lines,
              colour = "dodgerblue",
              size = 0.1
              ) +
      geom_sf(data = sf_locs,
              aes_string(colour = "id"),
              size = size,
              show.legend = "point"
                     ) +
    scale_colour_viridis_d() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 7, vjust = 0),
            plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 5)
      )
  } else {
    lab_dates <- with(sf_locs, pretty(seq(min(date), max(date), l = 5))) %>% as.Date()

    p <- p +
      geom_sf(data = sf_lines,
                     colour = "dodgerblue",
                     size = 0.1) +
      geom_sf(data = sf_locs,
                    aes(colour = as.numeric(as.Date(date))),
                     size = size
                     ) +
      scale_colour_viridis_c(breaks = as.numeric(lab_dates), option = "viridis", labels = lab_dates) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 6, vjust = 0),
            legend.key.width = unit(1.75, "cm"),
            legend.key.height = unit(0.5, "cm"),
            plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 5)
      )
  }

  return(p)
}
