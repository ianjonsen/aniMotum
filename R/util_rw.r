##' @title Utility functions
##'
##' @details Internal function, typically not called by user
##'
##' @importFrom lubridate ymd_hms
##' @importFrom stats loess
##' @importFrom dplyr mutate distinct arrange filter select %>% left_join
##' @importFrom rgdal project
##'
##' @export

wrap_lon <- function(lon, lon_min = -180) {

  (lon - lon_min) %% 360 + lon_min

}
