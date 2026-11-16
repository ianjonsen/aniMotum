##' @title Estimate the Kinéis standard-error inflation factor from GPS
##'
##' @description Estimates the factor by which the standard errors of SSM fits
##' to Kinéis (KI) locations should be multiplied, using GPS locations from
##' the same tracks as truth. The SSM is fitted to the Kinéis locations only,
##' without inflation, and locations are predicted at the GPS times. The
##' factor is the one that puts a proportion `coverage` of GPS locations
##' inside the inflated `coverage` confidence regions. Pass the result to
##' `ssm_control(kineis = list(se.inflation = ))` when fitting Kinéis-only
##' tracks.
##'
##' @param x a data frame of Kinéis and GPS locations for one or more tracks,
##' with columns `id`, `date` (POSIXct), `lc`, `lon`, `lat` and
##' `error.radius` (m). Kinéis locations have a positive `error.radius`. GPS
##' locations have `lc = "G"`. Other location types are ignored.
##' @param model the process model, `"crw"` or `"rw"`.
##' @param coverage the target coverage of the confidence regions (default
##' 0.95).
##' @param nboot number of bootstrap resamples of tracks, used to compute a
##' 95% interval for the factor (default 1000; 0 for none). Needs at least 2
##' tracks. Use `set.seed()` beforehand for a reproducible interval.
##' @param control a list from [ssm_control()]. `kineis$se.inflation` is
##' ignored: the SSM is fitted without inflation.
##' @param ... further arguments passed to [fit_ssm()] (e.g. `vmax`,
##' `projection`), except `model`, `time.step` and `control`.
##'
##' @return a list with:
##' 1. `factor`: the estimated inflation factor;
##' 2. `interval`: the 2.5% and 97.5% bootstrap quantiles of the factor,
##' resampling tracks (`NA` with fewer than 2 tracks or `nboot = 0`);
##' 3. `model`, `coverage`: as supplied;
##' 4. `tracks`: a tibble with, for each track, the number of GPS locations
##' used, the coverage without inflation, the coverage with `factor`, and the
##' factor estimated from that track alone;
##' 5. `scores`: a tibble with, for each GPS location used, the predicted and
##' GPS coordinates (km, in the projection used for the fit), the predicted
##' standard errors, and `d2`, the squared standardised distance between them.
##'
##' @details
##' Kinéis errors are serially correlated through CLS Kalman smoothing, but
##' the KI observation model treats them as independent, so SSM standard
##' errors are too small. The factor is
##' `sqrt(quantile(d2, coverage) / qchisq(coverage, 2))`, where
##' `d2 = ((x - x.gps) / x.se)^2 + ((y - y.gps) / y.se)^2`. Only GPS locations
##' within each track's Kinéis time span are used. Tracks whose SSM fit does
##' not converge are dropped with a warning.
##'
##' GPS location error is ignored. GPS locations should be quality-filtered
##' beforehand (e.g. on residual and number of satellites, and removing
##' spikes): erroneous GPS locations inflate the factor.
##'
##' The factor applies to fits to Kinéis locations only. It is not applied to
##' tracks that mix Kinéis and GPS locations (see [ssm_control()]).
##'
##' @examples
##' \dontrun{
##' ## locs: Kinéis (error.radius > 0) and quality-filtered GPS (lc = "G")
##' ##  locations
##' set.seed(1)
##' k <- kineis_inflation(locs, model = "crw")
##' k$factor
##' k$interval
##' k$tracks
##'
##' ## apply to Kinéis-only tracks
##' fit <- fit_ssm(subset(locs, lc != "G"), model = "crw", time.step = 6,
##'                control = ssm_control(kineis = list(se.inflation = k$factor)))
##' }
##'
##' @importFrom stats qchisq quantile
##' @importFrom sf st_as_sf st_transform st_coordinates st_crs
##' @importFrom dplyr tibble
##' @importFrom tibble as_tibble
##' @export
##' @md
kineis_inflation <- function(x,
                             model = c("crw", "rw"),
                             coverage = 0.95,
                             nboot = 1000,
                             control = ssm_control(verbose = 0),
                             ...) {

  model <- match.arg(model)
  if (!is.numeric(coverage) || length(coverage) != 1 ||
      !isTRUE(coverage > 0 && coverage < 1))
    stop("coverage must be a single number between 0 and 1")
  if (!is.numeric(nboot) || length(nboot) != 1 || !isTRUE(nboot >= 0))
    stop("nboot must be a single number >= 0")
  required <- c("id", "date", "lc", "lon", "lat", "error.radius")
  missing.columns <- setdiff(required, names(x))
  if (length(missing.columns) > 0)
    stop("x is missing column(s): ", paste(missing.columns, collapse = ", "))
  if (!inherits(x$date, "POSIXct")) stop("x$date must be POSIXct")

  x <- as.data.frame(x)
  is.gps <- x$lc %in% "G"
  is.ki <- !is.gps & !is.na(x$error.radius) & x$error.radius > 0
  ki <- x[is.ki, ]
  gps <- x[is.gps & !is.na(x$lon) & !is.na(x$lat), c("id", "date", "lon", "lat")]
  gps <- gps[!duplicated(gps[, c("id", "date")]), ]
  if (nrow(ki) == 0) stop("x has no Kinéis locations (positive error.radius)")

  ## GPS times strictly inside each track's Kinéis time span
  ki.seconds <- as.numeric(ki$date)
  first.ki <- tapply(ki.seconds, as.character(ki$id), min)
  last.ki <- tapply(ki.seconds, as.character(ki$id), max)
  gps.id <- as.character(gps$id)
  gps.seconds <- as.numeric(gps$date)
  inside <- gps.id %in% names(first.ki)
  inside[inside] <- gps.seconds[inside] > first.ki[gps.id[inside]] &
    gps.seconds[inside] < last.ki[gps.id[inside]]
  gps <- gps[inside, ]
  if (nrow(gps) == 0) stop("x has no GPS locations (lc = \"G\") within the time span of the Kinéis locations")
  ki <- ki[ki$id %in% unique(gps$id), ]

  ## fit to Kinéis locations only, without inflation, predicting at GPS times
  control$kineis$se.inflation <- 1
  fit <- fit_ssm(ki,
                 model = model,
                 time.step = gps[, c("id", "date")],
                 control = control,
                 ...)
  if (any(!fit$converged)) {
    warning("SSM fit did not converge for ", sum(!fit$converged), " track(s), dropped: ",
            paste(fit$id[!fit$converged], collapse = ", "))
    fit <- fit[fit$converged, ]
  }
  if (nrow(fit) == 0) stop("no SSM fits converged")

  ## GPS locations in the projection used for the fit (km)
  pred <- grab(fit, "predicted", as_sf = FALSE)
  crs <- st_crs(grab(fit, "predicted", as_sf = TRUE))
  gps.xy <- st_coordinates(st_transform(st_as_sf(gps, coords = c("lon", "lat"), crs = 4326), crs))
  gps$x.gps <- gps.xy[, 1]
  gps$y.gps <- gps.xy[, 2]

  s <- merge(as.data.frame(pred)[, c("id", "date", "x", "y", "x.se", "y.se")],
             gps[, c("id", "date", "x.gps", "y.gps")],
             by = c("id", "date"))
  s <- s[!is.na(s$x.se) & !is.na(s$y.se), ]
  if (nrow(s) == 0) stop("no predicted locations with standard errors at the GPS times")
  s$d2 <- ((s$x - s$x.gps) / s$x.se)^2 + ((s$y - s$y.gps) / s$y.se)^2
  s <- s[order(s$id, s$date), ]

  q <- qchisq(coverage, 2)
  inflation_factor <- function(d2) sqrt(quantile(d2, coverage, names = FALSE) / q)
  k <- inflation_factor(s$d2)

  ## per track
  by.track <- split(s$d2, s$id)
  tracks <- tibble(id = names(by.track),
                   n.gps = vapply(by.track, length, numeric(1)),
                   coverage.uninflated = vapply(by.track, function(d) mean(d <= q), numeric(1)),
                   coverage.inflated = vapply(by.track, function(d) mean(d / k^2 <= q), numeric(1)),
                   factor = vapply(by.track, inflation_factor, numeric(1)))

  ## 95% interval for the factor, resampling tracks
  interval <- c(NA_real_, NA_real_)
  if (nboot > 0 && length(by.track) >= 2) {
    boot <- replicate(nboot, inflation_factor(unlist(sample(by.track, replace = TRUE),
                                                     use.names = FALSE)))
    interval <- quantile(boot, c(0.025, 0.975), names = FALSE)
  }
  names(interval) <- c("2.5%", "97.5%")

  list(factor = k,
       interval = interval,
       model = model,
       coverage = coverage,
       tracks = tracks,
       scores = as_tibble(s))
}
