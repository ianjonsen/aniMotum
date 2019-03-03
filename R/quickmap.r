##' map foieGras fitted or predicted locations
##'
##' @title quickmap
##' @param x a foieGras fitted object
##' @param what specify which location estimates to map: fitted or predicted
##' @importFrom ggplot2 ggplot geom_sf aes ggtitle
##' @importFrom ggplot2 theme element_blank scale_colour_viridis_d
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_buffer st_crs
##' @export

quickmap <- function(x, what = c("fitted", "predicted"), crs = NULL, ext.rng = c(0.1, 0.1))
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
  }

  orig_prj <- st_crs(sf_locs)
  bounds <- st_bbox(sf_locs)
  bounds[c("xmin","xmax")] <- extendrange(bounds[c("xmin","xmax")], f = ext.rng[1])
  bounds[c("ymin","ymax")] <- extendrange(bounds[c("ymin","ymax")], f = ext.rng[2])

  ## get coastline shapes
  countriesLow <- NULL
  data("countriesLow", package = "rworldmap", envir = environment())
  coast <- suppressWarnings(st_as_sf(countriesLow) %>%
    st_transform(., orig_prj) %>%
    st_crop(., bounds) %>%
    st_buffer(., 0, nQuadSegs = 100))

  if(!is.null(crs)) {
    coast <- coast %>%
      st_transform(., crs)

    sf_locs <- sf_locs %>%
      st_transform(., crs)
  }

  p <- ggplot(data = sf_locs) +
    geom_sf(data = coast,
            fill = grey(0.4),
            lwd = 0) +
    xlim(bounds[1], bounds[3]) +
    ylim(bounds[2], bounds[4])

  if(length(unique(x$id)) > 1) {
    p <- p + geom_sf(data = sf_locs,
                     aes(color = id),
                     size = 0.75
                     ) +
    scale_colour_viridis_d()
  } else {
    p <- p + geom_sf(data = sf_locs,
                     col = switch(what, fitted = "firebrick", predicted = "dodgerblue"),
                     size = 0.75
                     )
  }
  p <- p + theme(legend.position = "none")

  return(p)
}
