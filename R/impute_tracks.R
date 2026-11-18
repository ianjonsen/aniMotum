##' @title Multiple imputation of regular-interval tracks
##'
##' @description Generates `reps` imputed tracks on a regular time grid, for
##' fitting behavioural models (e.g. [fit_hmm()], [fit_mpm()], [fit_mrm()])
##' with location uncertainty carried in by multiple imputation. Tracks are
##' imputed from either:
##' 1. an SSM fit: tracks are simulated from the fit's joint posterior with
##' [sim_post()], at the fit's predicted (regular) times;
##' 2. Kinéis locations, without an SSM: each location is perturbed by a draw
##' from its error distribution, defined by its CLS error radius, and the
##' perturbed locations are linearly interpolated to a regular time grid.
##'
##' @param x an `ssm_df` fit object (`rw`, `crw` or `mp`) with regular predicted
##' locations, or a data frame of Kinéis locations with columns `id`, `date`
##' (POSIXct), `lon`, `lat` and `error.radius` (m). SMRU column names (`REF`,
##' `D_DATE`, `LON`, `LAT`, `ERROR_RADIUS`) are also accepted, ignoring case.
##' @param reps number of imputed tracks (default 20).
##' @param time.step for Kinéis locations, the time step (h) of the regular
##' grid. Ignored for SSM fits, which use their own prediction times.
##' @param max_gap for Kinéis locations, the longest gap between consecutive
##' locations (h) that is interpolated across (default `2 * time.step`). Grid
##' times inside longer gaps are dropped and the track is split into segments
##' there. Ignored for SSM fits.
##' @param kineis for Kinéis locations, the Kinéis error model: a list with
##' `scale` (east and north error SDs relative to the error radius) and
##' `exponent` (on the error radius, in km); default as for [ssm_control()].
##' @param inflate for SSM fits, passed to [sim_post()] (default TRUE).
##' @param vmax for Kinéis locations, the maximum travel rate (m/s) of a
##' speed-distance-angle filter (`trip::sda()`, as `fit_ssm()` uses) applied
##' before imputation, to drop extreme locations; e.g. 4 for seals. A
##' location is dropped if reaching it implies a speed above `vmax` and it
##' forms a spike (angle and lengths as in `fit_ssm()`'s defaults,
##' `ang = c(15, 25)`, `distlim = c(2500, 5000)`). NULL (default): no
##' filter. Ignored for SSM fits, whose data were filtered by `fit_ssm()`.
##' @param crs the projection (proj4 string, units km) for the imputed
##' tracks' `x`, `y`, and, for Kinéis locations, for the interpolation. If
##' NULL (default), chosen from the extent of all the locations by
##' [auto_crs()]: Mercator where its scale varies little across the data,
##' otherwise Lambert conformal conic, polar stereographic or oblique
##' stereographic. All are conformal.
##'
##' @return a tibble with columns `rep` (0 = the point estimate: the SSM-
##' predicted track, or the Kinéis locations interpolated without
##' perturbation; 1 to `reps` = imputations), `id`, `segment` (contiguous runs
##' of grid times, numbered within `id`), `date`, `lon`, `lat`, and `x`, `y`
##' (km, in projection `crs`). The attribute `source` is `"ssm"` or
##' `"kineis"`; `time.step` holds the time step (h) and `crs` the projection.
##'
##' @details For Kinéis locations, the errors are drawn independently for each
##' location, with ground SDs `scale[1] * r^exponent` (east) and
##' `scale[2] * r^exponent` (north), r the error radius (km); the same error
##' model as `fit_ssm()` uses for KI observations. The perturbed locations are
##' projected and interpolated linearly in the projected plane, which follows
##' the great circle more closely than interpolation in longitude and
##' latitude, at high latitudes especially. Kinéis locations are
##' Kalman-smoothed estimates, so their errors are serially correlated;
##' independent draws ignore this. At time steps much longer than the interval
##' between Kinéis locations, each grid location is interpolated from the two
##' nearest locations, and the effect of ignoring the correlation is small.
##' Linear interpolation does not add uncertainty for movement between
##' locations, so `max_gap` should be short relative to the time step.
##'
##' For SSM fits, the simulated locations are reprojected from the fit's
##' projection to `crs`. The imputations include uncertainty in the SSM parameters
##' (see [sim_post()]). The imputation model is the SSM, which has constant
##' movement parameters: where the data constrain the track weakly, imputed
##' movement reverts towards the SSM's average behaviour, which can attenuate
##' differences between behaviours.
##'
##' @examples
##' fit <- fit_ssm(ellie, model = "crw", time.step = 24)
##' imp <- impute_tracks(fit, reps = 5)
##' table(imp$rep)
##'
##' @importFrom dplyr tibble bind_rows
##' @importFrom sf st_as_sf st_transform st_coordinates st_crs
##' @importFrom stats rnorm
##' @export
##' @md
impute_tracks <- function(x,
                          reps = 20,
                          time.step = NULL,
                          max_gap = NULL,
                          kineis = ssm_control()$kineis,
                          inflate = TRUE,
                          crs = NULL,
                          vmax = NULL) {

  if (!is.numeric(reps) || length(reps) != 1 || reps < 1)
    stop("reps must be a single number >= 1", call. = FALSE)
  if (!is.null(crs) && (!is.character(crs) || length(crs) != 1))
    stop("crs must be NULL or a proj4 string", call. = FALSE)
  if (!is.null(vmax) && (!is.numeric(vmax) || length(vmax) != 1 || !(vmax > 0)))
    stop("vmax must be NULL or a single positive number (m/s)", call. = FALSE)

  if (inherits(x, "ssm_df")) {
    out <- impute_from_ssm(x, reps, inflate, crs)
    attr(out, "source") <- "ssm"
    attr(out, "time.step") <- x$ssm[[1]]$ts
  } else if (is.data.frame(x) || is.character(x)) {
    if (is.null(time.step) || !is.numeric(time.step) || length(time.step) != 1 || time.step <= 0)
      stop("for Kinéis locations, time.step (h) must be a single positive number", call. = FALSE)
    if (is.null(max_gap)) max_gap <- 2 * time.step
    out <- impute_from_kineis(x, reps, time.step, max_gap, kineis, crs, vmax)
    attr(out, "source") <- "kineis"
    attr(out, "time.step") <- time.step
  } else {
    stop("x must be an ssm_df fit object or a data frame of Kinéis locations", call. = FALSE)
  }
  out
}

## imputations from an SSM fit, via sim_post()
impute_from_ssm <- function(x, reps, inflate, crs = NULL) {
  if (!all(x$pmodel %in% c("crw", "rw", "mp")))
    stop("x must be a `crw`, `rw` or `mp` SSM fit", call. = FALSE)
  if (any(vapply(x$ssm, function(s) !is.numeric(s$ts) || is.na(s$ts), logical(1))))
    stop("x must have regular predicted locations: fit with a positive numeric time.step",
         call. = FALSE)
  if (!all(x$converged)) {
    warning("dropping ", sum(!x$converged), " SSM fit(s) that did not converge: ",
            paste(x$id[!x$converged], collapse = ", "), call. = FALSE)
    x <- x[x$converged, ]
  }
  if (nrow(x) == 0) stop("no converged SSM fits", call. = FALSE)

  sims <- sim_post(x, what = "predicted", reps = reps, sim_only = FALSE, inflate = inflate)
  out <- bind_rows(lapply(seq_len(nrow(sims)), function(i) {
    s <- as.data.frame(sims$psims[[i]])
    s$id <- as.character(sims$id[i])
    s
  }))
  out$segment <- 1L
  ## reproject from the fit's projection to crs, chosen from the predicted
  ##  locations (rep 0) if not given, so it does not depend on the draws and
  ##  matches fit_mpm() without imputation
  if (is.null(crs)) crs <- auto_crs(out$lon[out$rep == 0], out$lat[out$rep == 0])
  xy <- lonlat_to_xy(out$lon, out$lat, crs)
  out$x <- xy[, 1]
  out$y <- xy[, 2]
  out <- out[order(out$rep, out$id, out$date), c("rep", "id", "segment", "date", "lon", "lat", "x", "y")]
  row.names(out) <- NULL
  out <- tibble(out)
  attr(out, "crs") <- crs
  out
}

## imputations directly from Kinéis locations: perturb each location by a
##  draw from its error-radius distribution, then interpolate to a regular grid
impute_from_kineis <- function(x, reps, time.step, max_gap, kineis, crs = NULL,
                               vmax = NULL) {
  d <- kineis_locations(x)
  if (!is.null(vmax)) d <- kineis_speed_filter(d, vmax)
  ## one projection for all individuals, chosen from all the locations
  if (is.null(crs)) crs <- auto_crs(d$lon, d$lat)
  scale <- kineis$scale
  exponent <- kineis$exponent
  if (is.null(scale) || length(scale) != 2 || is.null(exponent))
    stop("kineis must be a list with `scale` (length 2) and `exponent`", call. = FALSE)

  earth <- 6371.0088
  out <- lapply(split(d, d$id), function(s) {
    s <- s[order(s$date), ]
    s <- s[!duplicated(s$date), ]
    if (nrow(s) < 2) return(NULL)
    tt <- as.numeric(s$date) / 3600
    grid <- regular_grid(tt, time.step, max_gap)
    if (is.null(grid)) return(NULL)

    ## error SDs (km)
    r <- (s$error.radius / 1000)^exponent
    sd_east <- scale[1] * r
    sd_north <- scale[2] * r
    lon <- s$lon
    lat <- s$lat
    n <- nrow(s)

    lapply(0:reps, function(k) {
      if (k == 0) {
        plon <- lon
        plat <- lat
      } else {
        ## errors drawn on the ground (km), converted to degrees locally
        plat <- lat + rnorm(n, 0, sd_north) / earth * 180 / pi
        plon <- lon + rnorm(n, 0, sd_east) / (earth * cos(lat * pi / 180)) * 180 / pi
      }
      ## interpolate in the projected plane
      pxy <- lonlat_to_xy(plon, plat, crs)
      w <- grid$w
      gx <- (1 - w) * pxy[grid$i, 1] + w * pxy[grid$i + 1, 1]
      gy <- (1 - w) * pxy[grid$i, 2] + w * pxy[grid$i + 1, 2]
      ll <- xy_to_lonlat(gx, gy, crs)
      data.frame(rep = k, id = as.character(s$id[1]), segment = grid$segment,
                 date = as.POSIXct(grid$t * 3600, origin = "1970-01-01", tz = "UTC"),
                 lon = ll[, 1], lat = ll[, 2], x = gx, y = gy)
    })
  })
  out <- bind_rows(unlist(out, recursive = FALSE))
  if (nrow(out) == 0) stop("no track has enough Kinéis locations to impute", call. = FALSE)

  out <- out[order(out$rep, out$id, out$date), c("rep", "id", "segment", "date", "lon", "lat", "x", "y")]
  row.names(out) <- NULL
  out <- tibble(out)
  attr(out, "crs") <- crs
  out
}

## project longitude and latitude (degrees, WGS84) to x, y in crs, and back;
##  two-column matrices
lonlat_to_xy <- function(lon, lat, crs) {
  sf::sf_project("+proj=longlat +datum=WGS84 +no_defs", crs, cbind(lon, lat), keep = TRUE)
}
xy_to_lonlat <- function(x, y, crs) {
  ll <- sf::sf_project(crs, "+proj=longlat +datum=WGS84 +no_defs", cbind(x, y), keep = TRUE)
  ll[, 1] <- (ll[, 1] + 180) %% 360 - 180
  ll
}

## regular grid of times (h) within the span of tt, dropping grid times in
##  gaps longer than max_gap; returns the bracketing location index i, the
##  interpolation weight w and the segment number of each grid time
regular_grid <- function(tt, time.step, max_gap) {
  first <- ceiling(tt[1])
  if (first > tt[length(tt)]) return(NULL)
  t <- seq(first, tt[length(tt)], by = time.step)
  i <- findInterval(t, tt, rightmost.closed = TRUE)
  i[i >= length(tt)] <- length(tt) - 1
  gap <- tt[i + 1] - tt[i]
  ## keep grid times within short gaps, and grid times that fall exactly on a
  ##  location (no interpolation needed), even at the edge of a long gap
  keep <- gap <= max_gap | t == tt[i] | t == tt[i + 1]
  if (!any(keep)) return(NULL)
  ## a new segment starts after every dropped grid time
  segment <- cumsum(c(TRUE, diff(which(keep)) > 1))
  t <- t[keep]
  i <- i[keep]
  w <- (t - tt[i]) / (tt[i + 1] - tt[i])
  list(t = t, i = i, w = pmin(pmax(w, 0), 1), segment = segment)
}

## standardise Kinéis locations: id, date, lon, lat, error.radius (m); SMRU
##  or aniMotum column names, ignoring case; locations without a positive
##  error radius are dropped
kineis_locations <- function(d) {
  if (is.character(d) && length(d) == 1) {
    if (!file.exists(d)) stop("file not found: ", d, call. = FALSE)
    d <- utils::read.csv(d, stringsAsFactors = FALSE)
  }
  if (inherits(d, "sf")) stop("x must be a data frame, not an sf object", call. = FALSE)
  d <- as.data.frame(d)
  names(d) <- tolower(names(d))
  if (all(c("ref", "d_date") %in% names(d))) {
    columns <- c(id = "ref", date = "d_date", lon = "lon", lat = "lat", error.radius = "error_radius")
  } else {
    columns <- c(id = "id", date = "date", lon = "lon", lat = "lat", error.radius = "error.radius")
  }
  missing.columns <- setdiff(columns, names(d))
  if (length(missing.columns) > 0)
    stop("x is missing column(s): ", paste(missing.columns, collapse = ", "), call. = FALSE)
  out <- data.frame(id = as.character(d[[columns["id"]]]),
                    date = kineis_inflation_dates(d[[columns["date"]]], "x"),
                    lon = as.numeric(d[[columns["lon"]]]),
                    lat = as.numeric(d[[columns["lat"]]]),
                    error.radius = as.numeric(d[[columns["error.radius"]]]),
                    stringsAsFactors = FALSE)
  ki <- !is.na(out$error.radius) & out$error.radius > 0 &
    !is.na(out$date) & !is.na(out$lon) & !is.na(out$lat)
  if (!any(ki)) stop("x has no Kinéis locations (positive error radius)", call. = FALSE)
  if (any(!ki)) message(sum(!ki), " location(s) without a positive error radius, date or coordinates dropped")
  out[ki, ]
}

## Rubin's rules: pool estimates (rows = quantities, columns = imputations)
##  and their standard errors; returns the pooled estimate, total standard
##  error, degrees of freedom and 95% interval
pool_rubin <- function(est, se) {
  est <- as.matrix(est)
  se <- as.matrix(se)
  m <- ncol(est)
  qbar <- rowMeans(est, na.rm = TRUE)
  within <- rowMeans(se^2, na.rm = TRUE)
  if (m > 1) {
    between <- apply(est, 1, stats::var, na.rm = TRUE)
  } else {
    between <- rep(0, nrow(est))
  }
  total <- within + (1 + 1 / m) * between
  ## Rubin's degrees of freedom; infinite when there is no between-imputation
  ##  variance
  r <- (1 + 1 / m) * between / within
  df <- ifelse(between > 0 & within > 0, (m - 1) * (1 + 1 / r)^2, Inf)
  q <- stats::qt(0.975, df)
  data.frame(estimate = qbar, se = sqrt(total), df = df,
             lower = qbar - q * sqrt(total), upper = qbar + q * sqrt(total),
             fraction_missing = ifelse(total > 0, (1 + 1 / m) * between / total, 0))
}

## text progress bar over n steps; a no-op when show is FALSE or n < 2
progress_bar <- function(n, show = TRUE) {
  if (!isTRUE(show) || n < 2) {
    return(list(tick = function() invisible(NULL), close = function() invisible(NULL)))
  }
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  i <- 0
  list(tick = function() {
         i <<- i + 1
         utils::setTxtProgressBar(pb, i)
       },
       close = function() close(pb))
}

## drop extreme Kinéis locations with the speed-distance-angle filter that
##  fit_ssm() uses (pf_sda_filter(), trip::sda()), separately for each id,
##  with fit_ssm()'s default spike angles and lengths
kineis_speed_filter <- function(d, vmax, ang = c(15, 25), distlim = c(2500, 5000)) {
  d <- d[order(d$id, d$date), ]
  d <- d[!duplicated(d[, c("id", "date")]), ]
  keep <- unlist(lapply(split(d, d$id), function(z) {
    if (nrow(z) < 3) return(rep(TRUE, nrow(z)))
    z$keep <- TRUE
    z$obs.type <- "KI"
    f <- try(suppressWarnings(pf_sda_filter(z, spdf = TRUE, vmax = vmax, ang = ang,
                                            distlim = distlim)), silent = TRUE)
    if (inherits(f, "try-error")) {
      warning("speed filter failed for id ", z$id[1], "; its locations are kept",
              call. = FALSE)
      return(rep(TRUE, nrow(z)))
    }
    f$keep
  }), use.names = FALSE)
  keep[is.na(keep)] <- TRUE
  if (any(!keep))
    message("speed filter (vmax = ", vmax, " m/s) dropped ", sum(!keep), " of ",
            length(keep), " Kinéis locations")
  d[keep, ]
}
