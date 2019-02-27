##' map foieGras fitted or predicted locations
##'
##' @title map
##' @param x a foieGras fitted object
##' @param est specify which location estimates to map: fitted or predicted
##' @param se include 2 * SE on time-series plots (SE's currently not displayed when proj = "ll")
##' @param outlier include outlier observations identified during prefilter-ing
##' @importFrom ggplot2 ggplot geom_sf geom_point geom_line geom_path aes ggtitle theme_bw theme element_blank
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_buffer st_crs
##' @importFrom ggspatial layer_spatial annotation_spatial
##' @export

map <- function(x, est = c("fitted","predicted"), se = TRUE, outlier = FALSE)
{
  est <- match.arg(est)
  d <- if(!outlier) {
    subset(x$data, keep)
  } else {
    d <- x$data
  }
  od <- subset(x$data, !keep)

  switch(est,
         fitted = {
           sf_locs <- x$fitted
         },
         predicted = {
           sf_locs <- x$predicted
         })

  orig_prj <- st_crs(sf_locs)

  sf_locs_ll <- sf_locs %>% st_transform(4326)
  bounds <- st_bbox(sf_locs_ll)

  ## get coastline shapes
  countriesLow <- NULL
  data("countriesLow", package = "rworldmap", envir = environment())
  coast <- st_as_sf(countriesLow) %>%
    st_crop(., bounds) %>%
    st_buffer(0) %>%
    st_transform(., orig_prj)

## FIXME: this code does not segment original countriesLow polygons properly
## FIXME: not sure how to fix...
  browser()

}
