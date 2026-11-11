## -----------------------------------------------------------------------------
## Kinéis (KI) observation model: internal helpers
##
## Kinéis locations are supplied with a single CLS error radius (m). The KI
## observation model gives each location ground standard deviations
##   sd_east  = scale_east  * r^exponent
##   sd_north = scale_north * r^exponent
## with r the error radius in km. The east-west error is larger than the
## north-south error (tu126 olive ridley turtles, Kinéis against Fastloc GPS).
##
## The ground standard deviations are mapped to the projected x, y coordinates
## of the state-space model through the local Jacobian of the projection, so
## the covariance is correct in any projection: Mercator (scale sec(latitude)),
## or a non-conformal one such as the equidistant conic used by ArgosQC.
##
## Provisional defaults (scale_east = 1.061, scale_north = 0.739,
## exponent = 1) are the root mean square of the east and north errors,
## divided by the error radius, of 1,700 Kinéis locations paired within
## 5 minutes with Fastloc GPS fixes (21 olive ridley turtles, IMOS campaign
## tu126, 2026; gross errors excluded). A larger east than north error has
## also been found independently in other GPS-Kinéis data (personal
## communication).
## -----------------------------------------------------------------------------

##' @title Local Jacobian of the projection at Kinéis observations
##'
##' @description For each Kinéis (KI) observation, computes the derivatives of
##' the projected coordinates (x, y) with respect to ground displacements (km)
##' east and north, by finite differences. Columns `KIjac.xe`, `KIjac.xn`,
##' `KIjac.ye`, `KIjac.yn` are added (NA for other observation types).
##'
##' @param x projected sf data from `pf_sf_project()`
##' @param step ground step (km) for the finite differences
##'
##' @importFrom sf st_crs st_coordinates st_transform st_as_sf
##' @keywords internal
##' @noRd

pf_kineis_jacobian <- function(x, step = 0.1) {
  x$KIjac.xe <- x$KIjac.xn <- x$KIjac.ye <- x$KIjac.yn <- as.double(NA)
  if (!"obs.type" %in% names(x)) return(x)
  ki <- which(x$obs.type == "KI")
  if (length(ki) == 0) return(x)

  ## ground steps on the WGS84 ellipsoid: prime vertical radius of curvature
  ##  (east) and meridional radius of curvature (north), km
  semi_major <- 6378.137
  eccentricity_squared <- 0.00669437999014
  crs <- st_crs(x)
  xy <- st_coordinates(x[ki, ])
  lonlat <- st_coordinates(st_transform(x[ki, ], 4326))
  latitude <- lonlat[, 2] * pi / 180
  w <- 1 - eccentricity_squared * sin(latitude)^2
  radius_east <- semi_major / sqrt(w)
  radius_north <- semi_major * (1 - eccentricity_squared) / w^1.5
  delta_lon <- step / (radius_east * cos(latitude)) * 180 / pi
  delta_lat <- step / radius_north * 180 / pi

  project <- function(lon, lat) {
    st_coordinates(st_transform(
      st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"),
               crs = 4326),
      crs))
  }
  east <- project(lonlat[, 1] + delta_lon, lonlat[, 2])
  north <- project(lonlat[, 1], lonlat[, 2] + delta_lat)

  x$KIjac.xe[ki] <- (east[, 1] - xy[, 1]) / step
  x$KIjac.ye[ki] <- (east[, 2] - xy[, 2]) / step
  x$KIjac.xn[ki] <- (north[, 1] - xy[, 1]) / step
  x$KIjac.yn[ki] <- (north[, 2] - xy[, 2]) / step
  x
}

## names of the columns carried for the KI observation model
kineis_columns <- function() {
  c("error.radius", "KIjac.xe", "KIjac.xn", "KIjac.ye", "KIjac.yn")
}

## TMB data entries for the KI observation model
kineis_data <- function(d.all) {
  n <- nrow(d.all)
  if (!"error.radius" %in% names(d.all)) {
    return(list(KIrad = rep(NA_real_, n),
                KIjac = matrix(NA_real_, n, 4)))
  }
  list(KIrad = d.all$error.radius,
       KIjac = cbind(d.all$KIjac.xe, d.all$KIjac.xn,
                     d.all$KIjac.ye, d.all$KIjac.yn))
}

## initial (or fixed) values of the KI parameters
kineis_parameters <- function(control) {
  k <- control$kineis
  list(l_kineis_scale = log(k$scale),
       l_kineis_exponent = k$exponent)
}

## map entries: the KI parameters are fixed unless KI observations are present
## and their estimation is requested in ssm_control(kineis = ...)
kineis_map <- function(obs.type, control) {
  k <- control$kineis
  has_ki <- any(obs.type == "KI", na.rm = TRUE)
  out <- list()
  if (!has_ki || !isTRUE(k$estimate)) out$l_kineis_scale <- factor(c(NA, NA))
  if (!has_ki || !isTRUE(k$estimate.exponent)) out$l_kineis_exponent <- factor(NA)
  out
}

## tidy the KI rows of the reported parameter table: drop fixed parameters
## and name the east and north scales
kineis_report <- function(fxd, map) {
  drop <- c(if ("l_kineis_scale" %in% names(map)) "kineis_scale",
            if ("l_kineis_exponent" %in% names(map)) "kineis_exponent")
  fxd <- fxd[!row.names(fxd) %in% drop, , drop = FALSE]
  i <- which(row.names(fxd) == "kineis_scale")
  if (length(i) == 2) row.names(fxd)[i] <- c("kineis_scale_east", "kineis_scale_north")
  fxd
}
