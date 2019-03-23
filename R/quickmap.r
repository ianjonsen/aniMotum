##' @title quickmap
##'
##' @description map foieGras fitted or predicted locations, with or without
##' Argos observations, optionally apply a different projection
##'
##' @param x a foieGras fitted object
##' @param what specify which location estimates to map: fitted or predicted
##' @param obs include Argos observations on map (logical)
##' @param outlier include all extreme outliers flagged by prefilter in plots
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
##' @importFrom dplyr summarise
##' @importFrom magrittr "%>%"
##' @export

quickmap <- function(x,
                     what = c("fitted", "predicted"),
                     obs = FALSE,
                     outlier = FALSE,
                     crs = NULL,
                     ext.rng = c(0.05, 0.05),
                     size = 1)
{
  if(all(class(x) %in% c("foieGras", "list"))) {
  what <- match.arg(what)

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
  } else {
    stop("you can only supply a foieGras fit object or the output from `foieGras::grab()`")
  }

  if(is.null(crs)) {
    prj <- st_crs(sf_locs)
    sf_data <- x$data
    } else {
    sf_locs <- sf_locs %>% st_transform(., crs)
    prj <- st_crs(sf_locs)
    sf_data <- x$data %>% st_transform(., crs)
    }

  bounds <- st_bbox(sf_locs)
  bounds[c("xmin","xmax")] <- extendrange(bounds[c("xmin","xmax")], f = ext.rng[1])
  bounds[c("ymin","ymax")] <- extendrange(bounds[c("ymin","ymax")], f = ext.rng[2])

  sf_lines <- sf_locs %>%
    group_by(id) %>%
    summarise(do_union = FALSE) %>%
    st_cast("MULTILINESTRING")

  ## get coastline
  countriesLow <- NULL
  data("countriesLow", package = "rworldmap", envir = environment())
  coast <- st_as_sf(countriesLow) %>% st_transform(., prj)

  p <- ggplot() +
    geom_sf(data = coast,
            fill = grey(0.6),
            lwd=0) +
    xlim(bounds[c("xmin","xmax")]) +
    ylim(bounds[c("ymin","ymax")])

  if(obs) {
    if(!outlier) {
      sf_data <- sf_data %>% filter(keep)
    }
    p <- p + geom_sf(data = sf_data, colour = "orange", size = 2, shape = 18, alpha = 0.3)
  }

  if(length(unique(x$id)) > 1) {

    p <- p + geom_sf(data = sf_locs,
                     aes_string(color = "id"),
                     size = size
                     ) +
    scale_colour_viridis_d()
  } else {
    lab_dates <- with(sf_locs, seq(min(date), max(date), l = 5)) %>% as.Date()

    p <- p + geom_sf(data = sf_lines,
                     colour = "dodgerblue",
                     size = 0.1) +
      geom_sf(data = sf_locs,
                    aes(colour = as.numeric(as.Date(date))),
                     size = size
                     ) +
      scale_colour_viridis_c(breaks = as.numeric(lab_dates), option = "viridis", labels = lab_dates, end = 0.95) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 8, vjust = 0),
            legend.key.width = unit(1.5, "cm"),
            legend.key.height = unit(0.5, "cm"),
            plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 5)
            ) +
      ggtitle(paste0("id: ", x$predicted$id[1], ";  ", x$pm, " ", what, " values @ ", x$ts, " h"),
              subtitle = paste0("epsg = ", prj))
  }

  return(p)
}
