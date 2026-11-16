##' @title Estimate the Kinéis standard-error inflation factor from GPS
##'
##' @description Estimates the factor by which the standard errors of SSM fits
##' to Kinéis (KI) locations should be multiplied, using GPS locations from
##' the same tracks as truth. The SSM is fitted to the Kinéis locations only,
##' without inflation, and locations are predicted at the GPS times. The
##' factor is the one that puts a proportion `coverage` of the x and y errors
##' (predicted minus GPS) within the matching multiple of the inflated
##' standard errors. By default, 68.3\% of errors fall within 1 standard
##' error, so `x.se` and `y.se` keep their meaning as standard errors. Pass
##' the result to `ssm_control(kineis = list(se.inflation = ))` when fitting
##' Kinéis-only tracks.
##'
##' @param argos Argos locations, as a data frame or the path to a .csv file:
##' an SMRU diag file (columns `REF`, `D_DATE`, `LQ`, `LAT`, `LON`,
##' `ERROR_RADIUS`), or a data frame with columns `id`, `date`, `lc`, `lon`,
##' `lat` and `error.radius` (m). Kinéis locations are those with a positive
##' error radius; other Argos locations are ignored.
##' @param gps GPS locations for the same tracks, as a data frame or the path
##' to a .csv file: an SMRU gps file (columns `REF`, `D_DATE`, `LAT`, `LON`),
##' or a data frame with columns `id`, `date`, `lon` and `lat`. Filter GPS
##' locations before calling `kineis_inflation()` (see details).
##' @param model the process model, `"crw"`, `"rw"` or `"mp"`.
##' @param coverage the target proportion of x and y errors within
##' `qnorm((1 + coverage) / 2)` inflated standard errors. The default,
##' `pnorm(1) - pnorm(-1)` (0.683), calibrates the standard errors themselves
##' (1 standard error), as for the default factors in [ssm_control()]. Use
##' 0.95 to calibrate intervals of 1.96 standard errors.
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
##' used, the coverage of x and y errors without inflation and with `factor`,
##' and the factor estimated from that track alone;
##' 5. `scores`: a tibble with, for each GPS location used, the predicted and
##' GPS coordinates (km, in the projection used for the fit), the predicted
##' standard errors (uninflated), and the standardised errors
##' `z.x = (x - x.gps) / x.se` and `z.y = (y - y.gps) / y.se`;
##' 6. `data`: the Kinéis and GPS locations merged into one data frame
##' (`id`, `date`, `lc`, `lon`, `lat`, `error.radius`), ordered by date within
##' id. GPS locations have `lc = "G"` and `error.radius = NA`.
##'
##' @details
##' Column names are matched ignoring case. Dates may be POSIXct or text in
##' one of the formats `"\%Y-\%m-\%dT\%H:\%M:\%SZ"` (IMOS/AODN), `"\%Y-\%m-\%d \%H:\%M:\%S"` or
##' `"\%m/\%d/\%y \%H:\%M:\%S"` (SMRU database export), and are taken to be UTC.
##' Locations with missing dates or coordinates, and duplicated GPS dates
##' within a track, are dropped.
##'
##' Kinéis errors are serially correlated through CLS Kalman smoothing, but
##' the KI observation model treats them as independent, so SSM standard
##' errors are too small. The factor is
##' `quantile(c(abs(z.x), abs(z.y)), coverage) / qnorm((1 + coverage) / 2)`,
##' pooling the x and y errors. Only GPS locations
##' within each track's Kinéis time span are used. Tracks whose SSM fit does
##' not converge are dropped with a warning, as are GPS locations without SSM
##' standard errors (Hessian not positive-definite). For `mp`, only the
##' location standard errors are used.
##'
##' If standardised errors were normal, a factor calibrated at one coverage
##' would hold at all others. Kinéis errors are heavier-tailed than normal: on
##' tu126, factors calibrated at 1 standard error gave about 89-90\% of errors
##' within 1.96 inflated standard errors, not 95\%. Use `coverage = 0.95` if
##' wide intervals matter more than the standard errors themselves.
##'
##' The Kinéis observation covariance is mapped to the projection through its
##' local Jacobian, and the standardised errors are computed in projected
##' coordinates. For conformal projections (the default Mercator, and those
##' chosen by `projection = "auto"`) the local scale is the same in x and y,
##' so the standardised errors do not depend on the projection. For
##' non-conformal projections they are approximate.
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
##' ## SMRU diag and gps files; filter the GPS locations first
##' diag <- read.csv("diag.csv")
##' gps <- read.csv("gps.csv")
##' gps <- subset(gps, RESIDUAL <= 30 & NSATS_DETECTED >= 6)
##'
##' set.seed(1)
##' k <- kineis_inflation(diag, gps, model = "crw")
##' k$factor
##' k$interval
##' k$tracks
##'
##' ## apply to Kinéis-only tracks
##' fit <- fit_ssm(subset(k$data, lc != "G"), model = "crw", time.step = 6,
##'                control = ssm_control(kineis = list(se.inflation = k$factor)))
##' }
##'
##' @importFrom stats pnorm qnorm quantile
##' @importFrom utils read.csv
##' @importFrom sf st_as_sf st_transform st_coordinates st_crs
##' @importFrom dplyr tibble
##' @importFrom tibble as_tibble
##' @export
##' @md
kineis_inflation <- function(argos,
                             gps,
                             model = c("crw", "rw", "mp"),
                             coverage = pnorm(1) - pnorm(-1),
                             nboot = 1000,
                             control = ssm_control(verbose = 0),
                             ...) {

  model <- match.arg(model)
  if (!is.numeric(coverage) || length(coverage) != 1 ||
      !isTRUE(coverage > 0 && coverage < 1))
    stop("coverage must be a single number between 0 and 1")
  if (!is.numeric(nboot) || length(nboot) != 1 || !isTRUE(nboot >= 0))
    stop("nboot must be a single number >= 0")

  argos <- kineis_inflation_locations(argos, "argos")
  gps <- kineis_inflation_locations(gps, "gps")
  gps <- gps[!duplicated(gps[, c("id", "date")]), ]

  ## Argos and GPS merged, ordered by date within id
  locations <- rbind(argos, gps)
  locations <- locations[order(locations$id, locations$date), ]
  row.names(locations) <- NULL

  ki <- argos[!is.na(argos$error.radius) & argos$error.radius > 0, ]
  if (nrow(ki) == 0) stop("argos has no Kinéis locations (positive error radius)")
  gps <- gps[, c("id", "date", "lon", "lat")]

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
  if (nrow(gps) == 0) stop("gps has no locations within the time span of the Kinéis locations")
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
  missing.se <- is.na(s$x.se) | is.na(s$y.se)
  if (any(missing.se)) {
    warning(sum(missing.se), " GPS location(s) dropped: no SSM standard errors at their times ",
            "(Hessian not positive-definite) in track(s) ",
            paste(unique(s$id[missing.se]), collapse = ", "))
  }
  s <- s[!missing.se, ]
  if (nrow(s) == 0) stop("no predicted locations with standard errors at the GPS times")
  s$z.x <- (s$x - s$x.gps) / s$x.se
  s$z.y <- (s$y - s$y.gps) / s$y.se
  s <- s[order(s$id, s$date), ]

  ## x and y standardised errors pooled; q standard errors for the coverage
  ##  (q = 1 for the default coverage)
  q <- qnorm((1 + coverage) / 2)
  inflation_factor <- function(z) quantile(z, coverage, names = FALSE) / q
  z <- c(abs(s$z.x), abs(s$z.y))
  k <- inflation_factor(z)

  ## per track
  by.track <- split(z, c(s$id, s$id))
  tracks <- tibble(id = names(by.track),
                   n.gps = vapply(by.track, length, numeric(1)) / 2,
                   coverage.uninflated = vapply(by.track, function(d) mean(d <= q), numeric(1)),
                   coverage.inflated = vapply(by.track, function(d) mean(d / k <= q), numeric(1)),
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
       scores = as_tibble(s),
       data = locations)
}

## read and standardise the argos or gps locations supplied to
##  kineis_inflation(): returns id, date, lc, lon, lat, error.radius
kineis_inflation_locations <- function(d, type = c("argos", "gps")) {
  type <- match.arg(type)
  if (is.character(d) && length(d) == 1) {
    if (!file.exists(d)) stop(type, " file not found: ", d)
    d <- read.csv(d, stringsAsFactors = FALSE)
  }
  if (inherits(d, "sf")) stop(type, " must be a data frame or a .csv file path, not an sf object")
  if (!is.data.frame(d)) stop(type, " must be a data frame or a .csv file path")
  d <- as.data.frame(d)
  names(d) <- tolower(names(d))

  ## SMRU (diag or gps file) or aniMotum column names
  if (all(c("ref", "d_date") %in% names(d))) {
    columns <- c(id = "ref", date = "d_date", lon = "lon", lat = "lat",
                 lc = "lq", error.radius = "error_radius")
  } else {
    columns <- c(id = "id", date = "date", lon = "lon", lat = "lat",
                 lc = "lc", error.radius = "error.radius")
  }
  if (type == "gps") columns <- columns[c("id", "date", "lon", "lat")]
  missing.columns <- setdiff(columns, names(d))
  if (length(missing.columns) > 0)
    stop(type, " is missing column(s): ", paste(missing.columns, collapse = ", "))

  out <- data.frame(id = as.character(d[[columns["id"]]]),
                    date = kineis_inflation_dates(d[[columns["date"]]], type),
                    lc = "G",
                    lon = as.numeric(d[[columns["lon"]]]),
                    lat = as.numeric(d[[columns["lat"]]]),
                    error.radius = NA_real_,
                    stringsAsFactors = FALSE)
  if (type == "argos") {
    lc <- as.character(d[[columns["lc"]]])
    ## SMRU numeric location classes
    smru.lc <- c("-9" = "Z", "-2" = "B", "-1" = "A", "0" = "0", "1" = "1", "2" = "2", "3" = "3")
    recode <- lc %in% names(smru.lc)
    lc[recode] <- smru.lc[lc[recode]]
    out$lc <- lc
    out$error.radius <- as.numeric(d[[columns["error.radius"]]])
  }
  out[!is.na(out$date) & !is.na(out$lon) & !is.na(out$lat), ]
}

## POSIXct dates (UTC) from POSIXct or text in an IMOS/AODN, ISO or SMRU
##  database export format
kineis_inflation_dates <- function(x, type) {
  if (inherits(x, "POSIXct")) return(x)
  x <- as.character(x)
  formats <- c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S")
  for (f in formats) {
    out <- as.POSIXct(x, format = f, tz = "UTC")
    if (all(is.na(out) == is.na(x) | x == "")) return(out)
  }
  stop(type, " dates could not be read; expected POSIXct or text in one of the formats ",
       paste(formats, collapse = ", "))
}
