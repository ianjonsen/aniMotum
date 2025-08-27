##' @title Use `sda` to identify extreme locations
##' 
##' @details `sda` is a vectorized implementation of the Argos filter 
##' presented in Freitas et al. (2008) Mar Mamm Sci 24:315-325. This function
##' checks for errors returned by `sda` and falls back to using the simpler 
##' `speedfilter` if an error is returned. Both `sda` and `speedfilter` are native
##' implementations of those from the `trip` package 
##' (MD Sumner: https://github.com/Trackage/trip).
##'
##' @param x data from `pf_obs_type()`
##' @param spdf turn speed filter on/off (logical; default is TRUE)
##' @param vmax max travel rate (m/s)
##' @param ang angles of outlier location "spikes" (default is `c(15,25)` deg);
##' `ang = NA` turns off `sda` filter in favour of 
##' `speedfilter`
##' @param distlim lengths of outlier location "spikes" in km (default is 
##' `c(2.5, 5)` m); `distlim = NA` turns off `sda` filter 
##' in favour of `speedfilter`. Either `ang = NA` or 
##' `distlim = NA` are sufficient.
##' @importFrom sf st_coordinates st_is_longlat st_crs st_transform 
##' @importFrom trip trip sda speedfilter
##' @keywords internal
##' @md

pf_sda_filter <- function(x, spdf, vmax, ang, distlim) {
## Use internal version of trip::sda to identify outlier locations
if (spdf) {
  if(inherits(x, "sf") && st_is_longlat(x)) {
    
    xy <- as.data.frame(st_coordinates(x))
    names(xy) <- c("lon","lat")
    x <- cbind(x, xy)
    
  } else if(inherits(x, "sf") && !st_is_longlat(x)) {
    
    xy <- st_transform(x, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
    xy <- as.data.frame(st_coordinates(xy))
    names(xy) <- c("lon","lat")
    x <- cbind(x, xy)
  } 
  
  ## was req'd when using trip::sda - keep in case we want to revert now that
  ##  {trip} has been updated and 'un-archived'
#  x.tr <- subset(x, keep)[, c("lon","lat","date","id","lc","smaj","smin",
#                              "eor","lonerr","laterr","keep","obs.type")]
#  names(x.tr)[1:2] <- c("x","y")
#  x.tr <- suppressWarnings(trip(as.data.frame(x.tr), TORnames = c("date", "id"), 
#                                correct_all = FALSE))
  x.tr <- subset(x, keep)
  p.GL <- sum(x$obs.type == "GL") / nrow(x)
  if(p.GL > 0.75) {
    filt <- "spd"
  } else {
    filt <- "sda"
  }

  if(any(is.na(ang))) ang <- c(0,0)
  if(any(is.na(distlim))) distlim <- c(0,0)
  trip.dat <- suppressWarnings(with(x.tr, trip(data.frame(lon, lat, tms = date, id), 
                                                     correct_all = FALSE)))
  
  if (filt == "sda") {
    tmp <-
      suppressWarnings(try(sda(trip.dat,
                               smax = vmax * 3.6,
                               # convert m/s to km/h
                               ang = ang,
                               distlim = distlim / 1000),     # convert m to km
                               silent = TRUE))
                       ## screen potential sdafilter errors
                       if (inherits(tmp, "try-error")) {
                         warning(
                           paste(
                             "\ntrip::sda produced an error on id",
                             x$id[1],
                             "using trip::speedfilter instead"
                           ),
                           immediate. = TRUE
                         )
                         
                         tmp <-
                           suppressWarnings(try(speedfilter(trip.dat,
                                                            max.speed = vmax * 3.6),    # convert m/s to km/h
                                                            silent = TRUE))
                                            
                                            if (inherits(tmp, "try-error")) {
                                              warning(
                                                paste(
                                                  "\ntrip::speedfilter also produced an error on id",
                                                  x$id[1],
                                                  "can not apply speed filter prior to SSM filtering"
                                                ),
                                                immediate. = TRUE
                                              )
                                            }
                       }
  } else if (filt == "spd") {
    tmp <-
      suppressWarnings(try(speedfilter(trip.dat, max.speed = vmax * 3.6),
                           silent = TRUE))
    
    if (inherits(tmp, "try-error")) {
      warning(
        paste(
          "\ntrip::speedfilter also produced an error on id",
          x$id[1],
          "can not apply speed filter prior to SSM filtering"
        ),
        immediate. = TRUE
      )
    }
  }
  
  x[x$keep, "keep"] <- tmp
  
} 
  
  return(x)
}