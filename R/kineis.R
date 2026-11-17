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
  ## the Jacobian columns are added only when there are KI observations, so
  ##  prefilter() output for other data is unchanged
  if (!"obs.type" %in% names(x)) return(x)
  ki <- which(x$obs.type == "KI")
  if (length(ki) == 0) return(x)
  x$KIjac.xe <- x$KIjac.xn <- x$KIjac.ye <- x$KIjac.yn <- as.double(NA)

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
  ## no KI observations: the Jacobian columns are absent (and error.radius may
  ##  be absent, or present but missing for every row)
  if (!all(kineis_columns() %in% names(d.all))) {
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

## -----------------------------------------------------------------------------
## Standard-error inflation for Kinéis (KI) tracks
##
## Kinéis errors are serially correlated through CLS Kalman smoothing. The KI
## observation model treats them as independent, so the SSM standard errors
## are too small. The fitted and predicted standard errors of tracks made up
## only of KI observations are multiplied by a constant factor; the point
## estimates are unchanged.
##
## Default factors, by process model, are calibrated at 1 standard error:
## 68.3% of the x and y errors of SSM fits to Kinéis locations only, relative
## to withheld Fastloc GPS locations, fall within 1 inflated standard error
## (21 olive ridley turtles, IMOS campaign tu126, 2026; GPS residual <= 30,
## >= 6 satellites, spikes removed). Estimated factors (95% interval across
## tags): crw 1.78 (1.59-1.96), rw 1.14 (1.05-1.27), mp 1.74 (1.62-1.93, 20
## tags: one mp fit did not converge); rounded up to 1.8, 1.15 and 1.75.
## Coverage within 1 standard error varied widely between tags (held out one
## at a time: mean 0.64, range 0.24-0.84) and was lower in the first 30 days
## after release (0.60) than later (0.73). Kinéis errors are heavier-tailed
## than normal: with these factors, about 89-90% (not 95%) of errors fell
## within 1.96 standard errors. For mp, only x.se and y.se are inflated (not
## g.se).
##
## Two 95% factors are stored with each fit, so that map() and plot() can
## draw 95% confidence regions for Kinéis-only tracks without refitting; the
## stored (1-SE) standard errors are multiplied by the 95% factor / factor:
##  1. ellipse95, for 95% confidence ellipses (elps(), semi-axes
##     sqrt(qchisq(0.95, 2)) standard errors): 95% of GPS locations inside
##     the ellipse. crw 2.47 (2.28-2.71), rw 1.64 (1.51-1.81), mp 2.34
##     (2.16-2.60); rounded up to 2.5, 1.65 and 2.35;
##  2. interval95, for 95% intervals on each coordinate (plot() ribbons):
##     95% of x and y errors within 1.96 standard errors. crw 2.37
##     (2.13-2.56), rw 1.54 (1.45-1.70), mp 2.22 (2.07-2.42); rounded up to
##     2.4, 1.55 and 2.25.
## Standard errors are in projected coordinates; the factor is a scalar, so
## inflation does not depend on the projection. The KI observation covariance
## itself is mapped to the projection through the local Jacobian
## (pf_kineis_jacobian()) in all three models.
## -----------------------------------------------------------------------------

kineis_se_inflation_default <- c(crw = 1.8, rw = 1.15, mp = 1.75)
kineis_ellipse95_inflation_default <- c(crw = 2.5, rw = 1.65, mp = 2.35)
kineis_interval95_inflation_default <- c(crw = 2.4, rw = 1.55, mp = 2.25)

## factor for one track: obs.type are the types of the observations fitted
kineis_se_inflation <- function(obs.type, model, control) {
  k <- control$kineis$se.inflation
  ## a control list built without se.inflation gets the default
  if (is.null(k)) k <- "default"
  obs.type <- as.character(obs.type)
  ## factor: applied to the standard errors (calibrated at 1 standard error);
  ##  ellipse95, interval95: used only for 95% confidence ellipses and
  ##  intervals drawn by map() and plot()
  none <- function(note) list(factor = 1, ellipse95 = 1, interval95 = 1, note = note)
  if (length(obs.type) == 0 || !any(obs.type == "KI", na.rm = TRUE)) {
    return(none("no KI observations"))
  }
  if (!all(obs.type == "KI", na.rm = TRUE)) {
    return(none("KI mixed with other observation types: not inflated"))
  }
  if (identical(k, "default")) {
    if (!model %in% names(kineis_se_inflation_default)) {
      return(none(paste0("no default inflation for the ", model, " model: not inflated")))
    }
    return(list(factor = unname(kineis_se_inflation_default[model]),
                ellipse95 = unname(kineis_ellipse95_inflation_default[model]),
                interval95 = unname(kineis_interval95_inflation_default[model]),
                note = "default"))
  }
  ## a user-specified factor has no separate 95% factors: confidence regions
  ##  are drawn from the inflated standard errors, assuming normal errors
  list(factor = k, ellipse95 = k, interval95 = k, note = "user-specified")
}

## multiplier for the standard errors used in 95% confidence ellipses
##  (type = "ellipse") or intervals (type = "interval"), by track id: the 95%
##  factor / factor for Kinéis-only tracks, 1 otherwise (including fits made
##  before the 95% factors were stored)
kineis_ci_scale <- function(x, id, type = c("ellipse", "interval")) {
  type <- match.arg(type)
  element <- paste0(type, "95")
  scale <- vapply(x$ssm, function(f) {
    si <- if (is.list(f)) f$se.inflation else NULL
    if (is.null(si) || is.null(si[[element]]) || !isTRUE(si$factor > 0)) 1 else si[[element]] / si$factor
  }, numeric(1))
  names(scale) <- as.character(x$id)
  out <- unname(scale[as.character(id)])
  out[is.na(out)] <- 1
  out
}

## multiply the standard-error columns of fitted or predicted states
kineis_apply_se_inflation <- function(states, factor) {
  if (is.null(states) || factor == 1) return(states)
  for (column in intersect(c("x.se", "y.se", "u.se", "v.se", "s.se"), names(states))) {
    states[[column]] <- states[[column]] * factor
  }
  states
}
