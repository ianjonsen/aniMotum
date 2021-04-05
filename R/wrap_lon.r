##' @title wrap_lon
##'
##' @details wrap longitudes from an arbitrary minimum
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
##' @keywords internal

wrap_lon <- function(lon, lon_min = -180) {

  (lon - lon_min) %% 360 + lon_min

  }

