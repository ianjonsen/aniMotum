##' @title Utility functions
##'
##' @details called by prefilter
##' @param lon a vector of longitudes
##' @param lon_min the minimum longitude value to wrap appropriately, eg. 0 to
##' wrap -180, 180 on to 0, 360 and -180 to wrap 0,360 on to -180,180
##'
##' @examples
##' lon <- seq(-180,180)
##' lon1 <- wrap_lon(lon, 0)
##' range(lon)
##' range(lon1)
##' @export

wrap_lon <- function(lon, lon_min = -180) {

  (lon - lon_min) %% 360 + lon_min

  }


amf <- function() {
data.frame(
    lc = factor(
      c("3", "2", "1", "0", "A", "B", "Z"),
      levels = c("3", "2", "1", "0", "A", "B", "Z"),
      ordered = TRUE
    ),
    amf_x = c(1, 1.54, 3.72, 13.51, 23.9, 44.22, 44.22),
    amf_y = c(1, 1.29, 2.55, 14.99, 22.0, 32.53, 32.53)
#    amf_x = c(1, 1.54, 3.72, 13.51, 23.9, 120, 120),
#    amf_y = c(1, 1.29, 2.55, 14.99, 22.0, 100, 100)
    )

}

