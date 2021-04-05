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
##' The location class designations can be the standard Argos lc values: 3, 2, 1, 0, A, B, Z or other values. The number of classes specified is flexible though may not be amenable to a large number of classes. Whatever class designations are chosen must also appear in the input data \code{lc} column. A GPS location class ("G") is provided by default and assumes that GPS locations are 10 x more precise than Argos lc 3 locations.

##' @importFrom tibble as_tibble
##' @importFrom dplyr mutate "%>%"
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
  
  df <- data.frame(
    emf.x = c(gps[1], emf.x, emf.x[6]),
    emf.y = c(gps[2], emf.y, emf.y[6]),
    lc = c("G", "3", "2", "1", "0", "A", "B", "Z")
  ) %>%
    mutate(lc = as.character(lc))

  df %>%
    as_tibble()

}
