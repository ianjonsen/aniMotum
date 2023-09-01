##' @title emf
##'
##' @param gps error multiplication factor(s) for GPS locations, can be a scalar (x = y) or vector of length 2 (x != y)
##' @param emf.x error multiplication factors for Argos longitude classes 3, 2, 1, 0, A, B (Z assumed equal to B)
##' @param emf.y error multiplication factors for Argos latitude classes 3, 2, 1, 0, A, B (Z assumed equal to B)
##' @details Error Multiplication Factors for Argos (and GPS) locations. Default assumption is that GPS locations are
##' 10x more accurate than Argos lc 3 in both x and y directions.
##'  
##' User-specified Error Multiplication Factors (emf). emf's must be provided as a data.frame with the following columns:
##'
##' \code{emf.x} {emf values for the \code{x} direction}
##'
##' \code{emf.y} {emf values for \code{y} direction}
##'
##' \code{lc} {location class designations}
##'
##' The location class designations can be the standard Argos lc values: 3, 2, 1, 0, A, B, Z or other values. 
##' The number of classes specified is flexible though may not be amenable to a large number of classes. 
##' Whatever class designations are chosen must also appear in the input data \code{lc} column. 
##' A GPS location class ("G") is provided by default and assumes that GPS locations are 10 x more precise than Argos lc 3 locations.
##'
##' @export
emf <- function(gps = 0.1, 
                emf.x = c(1, 1.54, 3.72, 13.51, 23.9, 44.22),
                emf.y = c(1, 1.29, 2.55, 14.99, 22.0, 32.53)
) {
  
  if(!length(gps) %in% 1:2) stop("GPS emf must be a vector of length 1 or 2")
  if(length(emf.x) != 6) stop("Argos emf.x must be a vector of length 6")
  if(length(emf.y) != 6) stop("Argos emf.y must be a vector of length 6")
  
  if(length(gps) == 1) gps <- c(gps, gps)
  
  data.frame(
    emf.x = c(gps[1], emf.x, emf.x[6]),
    emf.y = c(gps[2], emf.y, emf.y[6]),
    lc = as.character(c("G", "3", "2", "1", "0", "A", "B", "Z"))
  )
  
}


##' @title rtnorm
##'
##' @details simulate values from a truncated normal distribution
##' @param n number of random values to generate
##' @param mean vector of means
##' @param sd vector of standard deviations
##' @param l lower limit of distribution
##' @param u upper limit of distribution
##' @importFrom stats pnorm qnorm runif
##' @keywords internal
rtnorm <- function(n, mean = 0, sd = 1, l = -Inf, u = Inf) {
  x <- runif(n, pnorm(l, mean, sd), pnorm(u, mean, sd))
  qnorm(x, mean, sd) 
}


##' @title wrap_lon
##'
##' @details wrap longitudes from an arbitrary minimum
##' @param lon a vector of longitudes
##' @param lon_min the minimum longitude value to wrap appropriately, eg. 0 to
##' wrap -180, 180 on to 0, 360 and -180 to wrap 0,360 on to -180,180
##' @keywords internal
wrap_lon <- function(lon, lon_min = -180) {
  
  (lon - lon_min) %% 360 + lon_min
  
}

##' @title sda: filter track for speed, distance and angle.
##'
##' @description Create a filter index of a track for "bad" points with a
##' combination of speed, distance and angle tests.
##' @param x fG_format object passed from pf_obs_type()
##' @param smax maximum speed, in km/h
##' @param ang minimum turning angle/s in degrees
##' @param distlim maximum step lengths in km
##' @references Freitas, C., Lydersen, C., Fedak, M. A. and Kovacs,
##' K. M. (2008), A simple new algorithm to filter marine mammal Argos
##' locations. Marine Mammal Science, 24: 315?V325. doi:
##' 10.1111/j.1748-7692.2007.00180.x
##' @details This is an implementation based on that in the
##' package trip by MD Sumner (https://github.com/Trackage/trip).
##' @return logical vector, with \code{FALSE} values where the tests failed
##' @importFrom traipse track_distance track_angle
##' @keywords internal

sda <- function(x, smax, ang = c(15, 25), distlim = c(2.5, 5.0)) {
  
  x$speed.ok <- speedfilter(x, max.speed = smax)
  if(all("lon" %in% names(x), "lat" %in% names(x))) {
    latlon <- TRUE
  } else {
    latlon <- FALSE
  }
  
  if(latlon) {
    dsts <- track_distance(x$lon, x$lat)
    angs <- track_angle(x$lon, x$lat)
  } else if (!latlon) {
    dx <- diff(x$x)
    dy <- diff(x$y)
    dsts <- c(NA, sqrt(dx^2 + dy^2))
    angs <- NA
  }
  
  ## simple way to deal with missing angles
  ### (which don't make sense for first and last position or zero-movement)
  angs[is.na(angs)] <- 180
  
  dprev <- dsts
  dnext <- c(dsts[-1L], 0)
  
  ## No Argos quality filter, anyone can do that
  ok <- (x$speed.ok | dprev <= distlim[2]) ##&  (x$lc > -9)
  
  x$filt.row <- 1:nrow(x)
  
  x$ok <- rep(FALSE, nrow(x))
  df <- x
  
  
  ## first subset
  
  df <- df[ok, ]
  
  ## distlim and angles, progressively
  for (i in 1:length(distlim)) {
    if(latlon) {
      dsts <- track_distance(df$lon, df$lat)
      angs <- track_angle(df$lon, df$lat)
    } else if (!latlon) {
      dx <- diff(df$x)
      dy <- diff(df$y)
      dsts <- c(NA, sqrt(dx^2 + dy^2))
      angs <- NA
    }
    
    dprev <- dsts
    dnext <- c(dsts[-1L], 0)
    
    
    angs[is.na(angs)] <- 180
    ok <- (dprev <= distlim[i] | dnext <= distlim[i])  | angs > ang[i]
    ok[c(1:2, (length(ok)-1):length(ok))] <- TRUE
    df <- df[ok, ]
    ok <- rep(TRUE, nrow(df))
  }
  
  x$ok[ match(df$filt.row, x$filt.row)] <- ok
  
  x$ok
}

##' @title speedfilter: filter track for speed.
##'
##' @description Create a filter index of a track for "bad" points based only on
##' speed. Called from `sda`
##' @param x fG_format object passed from `sda`
##' @param max.speed maximum speed, in km/h
##' @details This is an implementation based on that in the
##' package trip by MD Sumner (https://github.com/Trackage/trip).
##' @return logical vector, with \code{FALSE} values where the tests failed
##' @importFrom traipse track_distance_to
##' @keywords internal
speedfilter <- function (x, max.speed = NULL, test = FALSE) 
{

  if(all("lon" %in% names(x), "lat" %in% names(x))) {
    # req'd to drop geometry if inherits 'sf'
    coords <- as.matrix(x[, c("lon","lat")])[,1:2] 
    latlon <- TRUE
  } else {
    coords <- as.matrix(x[, c("x", "y")])
    latlon <- FALSE
  }
  dimnames(coords)[[2]] <- c("x","y")

  tids <- as.data.frame(x[, c("date","id")])
  time <- tids[, 1]
  id <- factor(tids[, 2])
  x <- coords[, 1]
  y <- coords[, 2]
  pprm <- 3
  grps <- levels(id)
  if (length(x) != length(y)) 
    stop("x and y vectors must be of same\nlength")
  if (length(x) != length(time)) 
    stop("Length of times not equal to number of points")
  okFULL <- rep(TRUE, nrow(coords))
  if (test) 
    res <- list(speed = numeric(0), rms = numeric(0))
  for (sub in grps) {
    ind <- id == sub
    xy <- matrix(c(x[ind], y[ind]), ncol = 2)
    tms <- time[ind]
    npts <- nrow(xy)
    if (pprm%%2 == 0 || pprm < 3) {
      msg <- paste("Points per running mean should be odd and", 
                   "greater than 3, pprm=3")
      stop(msg)
    }
    RMS <- rep(max.speed + 1, npts)
    offset <- pprm - 1
    ok <- rep(TRUE, npts)
    if (npts < (pprm + 1) && !test) {
      warning("Not enough points to filter ID: \"", sub, 
              "\"\n continuing . . . \n")
      okFULL[ind] <- ok
      next
    }
    index <- 1:npts
    iter <- 1
 
    while (any(RMS > max.speed, na.rm = TRUE)) {
      n <- length(which(ok))
      x1 <- xy[ok, ]
      if(latlon) {
        speed1 <- track_distance_to(x1[-nrow(x1), 1], x1[-nrow(x1),
                                                         2], x1[-1, 1], x1[-1, 2]) /
          1000
        speed1 <- speed1 / (diff(unclass(tms[ok])) / 3600)
        
        speed2 <- track_distance_to(x1[-((nrow(x1) - 1):nrow(x1)),
                                       1], x1[-((nrow(x1) - 1):nrow(x1)), 2], x1[-(1:2), 1], x1[-(1:2), 2]) /
          1000
        speed2 <-
          speed2 / ((unclass(tms[ok][-c(1, 2)]) - unclass(tms[ok][-c(n - 1, n)])) /
                      3600)
      } else if (!latlon) {
        speed1 <- sqrt((x1[-nrow(x1), 1] - x1[-1,1])^2 + 
                         (x1[-nrow(x1), 2] - x1[-1,2])^2) / 1000
        speed1 <- speed1 / ((unclass(tms[ok])) / 3600)
        speed2 <- sqrt((x1[-((nrow(x1) - 1): nrow(x1)), 1] - 
                          x1[-(1:2), 1])^2 +
                         (x1[-((nrow(x1) - 1):nrow(x1)), 2] -
                            x1[-(1:2), 2])^2) / 1000
        speed2 <- speed2 / ((unclass(tms[ok][-c(1, 2)]) - unclass(tms[ok][-c(n - 1, n)])) /
                              3600)
      }

      thisIndex <- index[ok]
      npts <- length(speed1)
      if (npts < pprm) 
        next
      
      sub1 <- rep(1:2, npts - offset) + rep(1:(npts - offset), 
                                            each = 2)
      sub2 <- rep(c(0, 2), npts - offset) + rep(1:(npts - 
                                                     offset), each = 2)
      rmsRows <- cbind(matrix(speed1[sub1], ncol = offset, 
                              byrow = TRUE), matrix(speed2[sub2], ncol = offset, 
                                                    byrow = TRUE))
      RMS <- c(rep(0, offset), sqrt(rowSums(rmsRows^2)/ncol(rmsRows)))
      if (test & iter == 1) {
        res$speed <- c(res$speed, 0, speed1)
        res$rms <- c(res$rms, 0, RMS)
        break
      }
      RMS[length(RMS)] <- 0
      bad <- RMS > max.speed
      segs <- cumsum(c(0, abs(diff(bad))))
      rmsFlag <- unlist(lapply(split(RMS, segs), function(x) {
        ifelse((1:length(x)) == which.max(x), TRUE, FALSE)
      }), use.names = FALSE)
      rmsFlag[!bad] <- FALSE
      RMS[rmsFlag] <- -10
      ok[thisIndex][rmsFlag > 0] <- FALSE
    }
    okFULL[ind] <- ok
  }
  if (test) 
    return(res)
  okFULL
}