##' map foieGras fitted or predicted locations
##'
##' @title quickmap
##' @param x a foieGras fitted object
##' @param what specify which location estimates to map: fitted or predicted
##' @param obs include Argos observations on map (logical)
##' @param outlier include all extreme outliers flagged by prefilter in plots (logical); ignored if `obs = FALSE`
##' @param crs `proj4string` or `epsg` for reprojecting locations, if NULL the default projection (eg. 4326) for the fitting the SSM will be used
##' @param ext.rng proportions to extend the plot range in x and y dimensions
##' @param size size of estimated location points
##' @importFrom ggplot2 ggplot geom_sf aes ggtitle
##' @importFrom ggplot2 theme element_blank scale_colour_viridis_d
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_buffer st_crs st_coordinates
##' @export

quickmap <- function(x,
                     what = c("fitted", "predicted"),
                     obs = FALSE,
                     outlier = FALSE,
                     crs = NULL,
                     ext.rng = c(0.1, 0.1),
                     size = 1)
{
  if(class(x)[1] != "sf") {
  what <- match.arg(what)

  switch(what,
         fitted = {
           sf_locs <- x$fitted
         },
         predicted = {
           sf_locs <- x$predicted
         })
  } else {
    sf_locs <- x
    what <- class(x)[2]
  }


  prj <- st_crs(sf_locs)
  if(!is.null(crs)) {
    sf_locs <- sf_locs %>% st_transform(., crs)
    prj <- st_crs(sf_locs)

    sf_data <- x$data %>% st_transform(., crs)
  } else {
    sf_data <- x$data
  }

  bounds <- st_bbox(sf_locs)
  bounds[c("xmin","xmax")] <- extendrange(bounds[c("xmin","xmax")], f = ext.rng[1])
  bounds[c("ymin","ymax")] <- extendrange(bounds[c("ymin","ymax")], f = ext.rng[2])

  ## get coastline shapes
  countriesLow <- NULL
  data("countriesLow", package = "rworldmap", envir = environment())
  coast <- suppressWarnings(st_as_sf(countriesLow) %>%
    st_transform(., prj) %>%
    st_buffer(., 0, nQuadSegs = 1000) %>%
    st_crop(., bounds))


  if(!is.null(crs)) {
    coast <- coast %>%
      st_transform(., crs)

    sf_locs <- sf_locs %>%
      st_transform(., crs)
  }

  p <- ggplot(data = sf_locs) +
    geom_sf(data = coast,
            fill = grey(0.4),
            lwd = 0)

  if(obs) {
    if(!outlier) {
      sf_data <- sf_data %>% filter(keep)
    }
    p <- p + geom_sf(data = sf_data, col = grey(0.8), size = 1, shape = 3)
  }

  if(length(unique(x$id)) > 1) {

    p <- p + geom_sf(data = sf_locs,
                     aes(color = id),
                     size = size
                     ) +
    scale_colour_viridis_d()
  } else {
    lab_dates <- with(sf_locs, seq(min(date), max(date), l = 5)) %>% as.Date()

    p <- p + geom_sf(data = sf_locs,
                    aes(colour = as.numeric(as.Date(date))),
                     size = size
                     ) +
      scale_colour_viridis_c("date", breaks = as.numeric(lab_dates), option = "viridis", labels = lab_dates) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8),
            legend.key.width = unit(1.5, "cm")
            ) +
      ggtitle(paste0("id: ", x$predicted$id[1], ";  model: ", x$pm, ";   ", what, " values"))

  }
#  p <- p + #theme(legend.position = "none") +
#    scale_x_continuous(breaks = seq(-180, 180, by = 5))

  return(p)
}
