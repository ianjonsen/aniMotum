##' map foieGras fitted or predicted locations
##'
##' @title quickmap
##' @param x a foieGras fitted object
##' @param what specify which location estimates to map: fitted or predicted
##' @importFrom ggplot2 ggplot geom_sf aes ggtitle
##' @importFrom ggplot2 theme element_blank scale_colour_viridis_d
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_buffer st_crs
##' @export

quickmap <- function(x, what = c("fitted", "predicted"), data = NULL, crs = NULL, ext.rng = c(0.1, 0.1))
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

    sf_data <- data %>% st_transform(., crs)
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

  if(!is.null(data)) {
    p <- p + geom_sf(data = sf_data, col = grey(0.7), size = 0.85)
  }

  if(length(unique(x$id)) > 1) {
    p <- p + geom_sf(data = sf_locs,
                     aes(color = id),
                     size = 0.6
                     ) +
    scale_colour_viridis_d()
  } else {
    p <- p + geom_sf(data = sf_locs,
                     col = switch(what, fitted = "firebrick", predicted = "dodgerblue"),
                     size = 0.6
                     )
  }
  p <- p + theme(legend.position = "none")

  return(p)
}
