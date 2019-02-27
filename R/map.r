##' map foieGras fitted or predicted locations
##'
##' @title map
##' @param x a foieGras fitted object
##' @param est specify which location estimates to map: fitted or predicted
##' @param se include 2 * SE on time-series plots (SE's currently not displayed when proj = "ll")
##' @param outlier include outlier observations identified during prefilter-ing
##' @importFrom ggplot2 ggplot geom_sf geom_point geom_line geom_path aes ggtitle theme_bw theme element_blank
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_intersection
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

  prj <- st_crs(sf_locs)

  ## get coastline shapes
  bounds <- st_bbox(sf_locs)
  countriesLow <- NULL
  data("countriesLow", package = "rworldmap", envir = environment())

  browser()

  ## FIXME: this doesn't work - GEOS returning an evaluation error when trying to crop the transformed coast data
  ## FIXME: try converting sf_locs to 4326 and then crop coast data to see if it's the polar projection that's
  ## FIXME: causing the issue
  coast <- st_as_sf(countriesLow) %>%
    st_transform(., prj) %>%
    st_crop(., bounds) %>%
    st_buffer(0)


#  bb <- st_bbox(c(

  browser()

}
