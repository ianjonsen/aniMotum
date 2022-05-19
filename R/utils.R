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