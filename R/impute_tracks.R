##' @title Multiple imputation of regular-interval tracks
##'
##' @description Generates `reps` imputed tracks on a regular time grid, for
##' fitting behavioural models with [fit_bm()]
##' with location uncertainty carried in by multiple imputation. Tracks are
##' imputed from either:
##' 1. an SSM fit: tracks are simulated from the fit's joint posterior with
##' [sim_post()], at the fit's predicted (regular) times;
##' 2. location data, without an SSM: Kinéis locations, or any locations
##' with their errors (or none). Each location is perturbed by a draw from its
##' error distribution and the perturbed locations are linearly interpolated
##' to a regular time grid.
##'
##' @param x an `ssm_df` fit object (`rw`, `crw` or `mp`) with regular predicted
##' locations; or a data frame of locations (or the path to a csv file of
##' them) with columns `id`, `date` (POSIXct, or text such as
##' `"2024-01-31 12:00:00"`), `lon` and `lat`, and optionally their errors
##' (see details). Column names are matched ignoring case; SMRU names (`REF`,
##' `D_DATE`) are also accepted. Other columns are kept in the input data
##' returned (attribute `data`).
##' @param reps number of imputed tracks (default 20).
##' @param time.step for location data, the time step (h) of the regular
##' grid. Ignored for SSM fits, which use their own prediction times.
##' @param max_gap the longest gap between consecutive locations (h) that is
##' imputed across. Grid times inside longer gaps are dropped and the track is
##' split into segments there. For location data, the default is
##' `2 * time.step`. For SSM fits, the gaps are between the observations the
##' SSM used (`keep = TRUE`), and the default, NULL, keeps every predicted time
##' (one segment per track).
##' @param kineis for Kinéis locations, the Kinéis error model: a list with
##' `scale` (east and north error SDs relative to the error radius) and
##' `exponent` (on the error radius, in km); default as for [ssm_control()].
##' @param inflate for SSM fits, passed to [sim_post()] (default TRUE).
##' @param vmax for location data, the maximum travel rate (m/s) of a
##' speed-distance-angle filter (`trip::sda()`, as `fit_ssm()` uses) applied
##' before imputation, to drop extreme locations; e.g. 4 for seals. A
##' location is dropped if reaching it implies a speed above `vmax` and it
##' forms a spike (angle and lengths as in `fit_ssm()`'s defaults,
##' `ang = c(15, 25)`, `distlim = c(2500, 5000)`). NULL (default): no
##' filter. Ignored for SSM fits, whose data were filtered by `fit_ssm()`.
##' @param crs the projection (proj4 string, units km) for the imputed
##' tracks' `x`, `y`, and, for location data, for the interpolation. If
##' NULL (default), chosen from the extent of all the locations by
##' [auto_crs()]: Mercator where its scale varies little across the data,
##' otherwise Lambert conformal conic, polar stereographic or oblique
##' stereographic. All are conformal.
##' @param error.units for location data with standard-error columns, their
##' units, `"degrees"` or `"km"`, overriding the units implied by the column
##' names (see details). NULL (default): from the column names.
##' @param haulout optional haulout bouts at which imputed track segments end:
##' a data frame with `id`, `start` and `end` (e.g. from [ho_bouts()],
##' filtered as needed), or SMRU haulout records (a file path, or a data frame
##' with `ref`, `s_date` and `e_date`), merged into bouts by [ho_bouts()] with
##' its defaults. Grid times inside a bout are dropped and a new segment
##' starts at the first grid time after it, so no step spans time ashore.
##' NULL (default): tracks are not split at haulouts.
##'
##' @return a tibble with columns `rep` (0 = the point estimate: the SSM-
##' predicted track, or the Kinéis locations interpolated without
##' perturbation; 1 to `reps` = imputations), `id`, `segment` (contiguous runs
##' of grid times, numbered within `id`), `date`, `lon`, `lat`, and `x`, `y`
##' (km, in projection `crs`). Attributes: `source` (`"ssm"`, `"kineis"` or
##' `"data"`), `time.step` (h), `crs` (the projection), and, for location
##' data, `errors` (where the location errors came from) and `data` (the
##' input, every column, with standard `id` and `date` columns and `keep`:
##' whether each location was used).
##'
##' @details For location data, the errors of each location are read from the
##' first of these that `x` has:
##' 1. a Kinéis error radius (m), in a column named `error.radius`,
##' `error_radius` or any other spelling of "error radius" (ignoring case and
##' separators),
##' through the Kinéis error model (`kineis`). In SMRU diag data from before
##' Kinéis, the error radius is the CLS Argos (Kalman filter) radius, and
##' the ellipse semi-axes (`semi_major_axis`, `semi_minor_axis`) differ from
##' it; such locations are still imputed from their error radius through the
##' Kinéis error model, with a message;
##' 2. standard errors: `lon.se`, `lat.se` or `lonerr`, `laterr` (degrees), or
##' `x.se`, `y.se` (km, east and north on the ground). Set `error.units` if
##' the units differ from those the names imply. Constant errors (e.g. for
##' GPS) are given by repeating the value in every row;
##' 3. 95% intervals: `lon.lci`, `lon.uci`, `lat.lci`, `lat.uci` (degrees),
##' taken as normal: SE = (uci - lci) / (2 * 1.96).
##'
##' Without any of these the locations are taken as known: every imputation
##' is the interpolated track. Standard errors in degrees are converted to
##' km on the ground at each location's latitude. Note that `x.se`, `y.se`
##' must be ground distances: those in `fit_ssm()` output are in the fit's
##' projection (e.g. Mercator km, stretched by 1 / cos(latitude)).
##'
##' For Kinéis locations, the errors are drawn independently for each
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
                          vmax = NULL,
                          error.units = NULL,
                          haulout = NULL) {

  if (!is.numeric(reps) || length(reps) != 1 || reps < 1)
    stop("reps must be a single number >= 1", call. = FALSE)
  if (!is.null(crs) && (!is.character(crs) || length(crs) != 1))
    stop("crs must be NULL or a proj4 string", call. = FALSE)
  if (!is.null(vmax) && (!is.numeric(vmax) || length(vmax) != 1 || !(vmax > 0)))
    stop("vmax must be NULL or a single positive number (m/s)", call. = FALSE)

  if (inherits(x, "ssm_df")) {
    if (!is.null(max_gap) && (!is.numeric(max_gap) || length(max_gap) != 1 || !(max_gap > 0)))
      stop("max_gap must be NULL or a single positive number (h)", call. = FALSE)
    out <- impute_from_ssm(x, reps, inflate, crs, max_gap)
    attr(out, "source") <- "ssm"
    attr(out, "time.step") <- x$ssm[[1]]$ts
  } else if (is.data.frame(x) || is.character(x)) {
    if (is.null(time.step) || !is.numeric(time.step) || length(time.step) != 1 || time.step <= 0)
      stop("for location data, time.step (h) must be a single positive number", call. = FALSE)
    if (is.null(max_gap)) max_gap <- 2 * time.step
    out <- impute_from_locations(x, reps, time.step, max_gap, kineis, crs, vmax, error.units)
    attr(out, "time.step") <- time.step
  } else {
    stop("x must be an ssm_df fit object or a data frame of locations", call. = FALSE)
  }
  if (!is.null(haulout)) out <- ho_split(out, haulout)
  out
}

## imputations from an SSM fit, via sim_post()
impute_from_ssm <- function(x, reps, inflate, crs = NULL, max_gap = NULL) {
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
  ## drop predicted times inside gaps between observations longer than
  ##  max_gap, and start a new segment after each, as for location data
  if (!is.null(max_gap)) {
    seg <- bind_rows(lapply(seq_len(nrow(x)), function(k) {
      id <- as.character(x$id[k])
      d <- x$ssm[[k]]$data
      tt <- sort(unique(as.numeric(d$date[d$keep])))
      tp <- sort(unique(as.numeric(out$date[out$id == id])))
      data.frame(id = id, t = tp,
                 segment = gap_segments(tp / 3600, tt / 3600, max_gap))
    }))
    out$segment <- seg$segment[match(paste(out$id, as.numeric(out$date)),
                                     paste(seg$id, seg$t))]
    out <- out[!is.na(out$segment), ]
    if (nrow(out) == 0) stop("no predicted locations left after dropping gaps longer than max_gap",
                             call. = FALSE)
  }
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

## imputations directly from location data (Kinéis, or any locations with
##  standard errors, 95% intervals or no errors): perturb each location by a
##  draw from its error distribution, then interpolate to a regular grid
impute_from_locations <- function(x, reps, time.step, max_gap, kineis, crs = NULL,
                                  vmax = NULL, error.units = NULL) {
  tl <- track_locations(x, kineis, error.units)
  d <- tl$loc
  if (!is.null(vmax)) d <- kineis_speed_filter(d, vmax)
  ## one projection for all individuals, chosen from all the locations
  if (is.null(crs)) crs <- auto_crs(d$lon, d$lat)

  earth <- 6371.0088
  used <- integer(0)
  out <- lapply(split(d, d$id), function(s) {
    s <- s[order(s$date), ]
    s <- s[!duplicated(s$date), ]
    if (nrow(s) < 2) return(NULL)
    tt <- as.numeric(s$date) / 3600
    grid <- regular_grid(tt, time.step, max_gap)
    if (is.null(grid)) return(NULL)
    used <<- c(used, s$.row)

    ## error SDs on the ground (km)
    sd_east <- s$sd_east
    sd_north <- s$sd_north
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
  if (nrow(out) == 0) stop("no track has enough locations to impute", call. = FALSE)

  out <- out[order(out$rep, out$id, out$date), c("rep", "id", "segment", "date", "lon", "lat", "x", "y")]
  row.names(out) <- NULL
  out <- tibble(out)
  ## the input data, with every column, and whether each location was used
  data <- tl$data
  data$keep <- seq_len(nrow(data)) %in% used
  attr(out, "crs") <- crs
  attr(out, "source") <- if (tl$errors == "kineis") "kineis" else "data"
  attr(out, "errors") <- tl$errors
  attr(out, "data") <- data
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

## segment number of each predicted time tp (h) of an SSM fit, given the
##  times tt (h) of the observations it used: NA for predicted times inside
##  gaps between observations longer than max_gap, with a new segment after
##  each run of them. The same rule as regular_grid() for location data
gap_segments <- function(tp, tt, max_gap) {
  n <- length(tt)
  if (n < 2) return(rep(1L, length(tp)))
  i <- findInterval(tp, tt, rightmost.closed = TRUE)
  inside <- i >= 1 & i < n
  gap <- rep(0, length(tp))
  gap[inside] <- tt[i[inside] + 1] - tt[i[inside]]
  on_obs <- inside & (tp == tt[pmax(i, 1)] | tp == tt[pmin(i + 1, n)])
  keep <- gap <= max_gap | on_obs
  segment <- rep(NA_integer_, length(tp))
  if (any(keep)) segment[keep] <- cumsum(c(TRUE, diff(which(keep)) > 1))
  segment
}

## standardise location data for imputation. Returns a list:
##  loc: one row per usable location, with .row (its row in the input), id,
##   date, lon, lat, and the ground error SDs east and north (km), sd_east and
##   sd_north;
##  data: the input, every column, with standard `id` and `date` (POSIXct)
##   columns added where its own are named otherwise (e.g. SMRU REF, D_DATE);
##  errors: where the errors came from (see location_errors()).
## Column names are matched ignoring case; SMRU names (REF, D_DATE) are
##  accepted.
track_locations <- function(x, kineis = ssm_control()$kineis, error.units = NULL) {
  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) stop("file not found: ", x, call. = FALSE)
    x <- utils::read.csv(x, stringsAsFactors = FALSE)
  }
  if (inherits(x, "sf")) stop("x must be a data frame, not an sf object", call. = FALSE)
  data <- as.data.frame(x)
  d <- data
  names(d) <- tolower(names(d))
  smru <- all(c("ref", "d_date") %in% names(d)) && !all(c("id", "date") %in% names(d))
  id.col <- if (smru) "ref" else "id"
  date.col <- if (smru) "d_date" else "date"
  missing.columns <- setdiff(c(id.col, date.col, "lon", "lat"), names(d))
  if (length(missing.columns) > 0)
    stop("x is missing column(s): ", paste(missing.columns, collapse = ", "),
         " (id, date, lon and lat are required)", call. = FALSE)

  loc <- data.frame(.row = seq_len(nrow(d)),
                    id = as.character(d[[id.col]]),
                    date = kineis_inflation_dates(d[[date.col]], "x"),
                    lon = as.numeric(d$lon),
                    lat = as.numeric(d$lat),
                    stringsAsFactors = FALSE)
  err <- location_errors(d, loc$lat, kineis, error.units)
  loc$sd_east <- err$east
  loc$sd_north <- err$north
  ok <- !is.na(loc$date) & !is.na(loc$lon) & !is.na(loc$lat) &
    !is.na(loc$sd_east) & !is.na(loc$sd_north)
  if (!any(ok))
    stop("x has no usable locations", if (err$type == "kineis") " (positive error radius)" else "",
         call. = FALSE)
  if (any(!ok))
    message(sum(!ok), " location(s) without ",
            if (err$type == "kineis") "a positive error radius, " else if (err$type != "none") "errors, " else "",
            "a date or coordinates dropped")

  ## standard id and date columns in the data returned
  data$id <- loc$id
  data$date <- loc$date
  list(loc = loc[ok, ], data = data, errors = err$type)
}

## ground error SDs (km), east and north, of each location, from the first of
##  these that x has (names ignoring case):
##  1) Kinéis error radius (error.radius, error_radius or another spelling;
##     see error_radius_column(); m), with the Kinéis
##     error model `kineis` (as fit_ssm() uses for KI observations);
##  2) standard errors: lon.se and lat.se, or lonerr and laterr (degrees), or
##     x.se and y.se (km, east and north on the ground);
##  3) 95% intervals: lon.lci, lon.uci, lat.lci, lat.uci (degrees), taken as
##     normal: SE = (uci - lci) / (2 * 1.96).
##  Without any, the locations are taken as known (SDs of 0).
##  error.units ("degrees" or "km") overrides the units implied by the names
##  of standard-error columns. Returns list(type, east, north).
location_errors <- function(d, lat, kineis, error.units = NULL) {
  earth <- 6371.0088
  if (!is.null(error.units))
    error.units <- match.arg(error.units, c("degrees", "km"))
  n <- nrow(d)
  ## 1) Kinéis error radius
  r.col <- error_radius_column(names(d))
  if (!is.null(r.col)) {
    scale <- kineis$scale
    exponent <- kineis$exponent
    if (is.null(scale) || length(scale) != 2 || is.null(exponent))
      stop("kineis must be a list with `scale` (length 2) and `exponent`", call. = FALSE)
    r <- as.numeric(d[[r.col]])
    r[!is.na(r) & r <= 0] <- NA
    ## pre-Kinéis SMRU diag data also carry an error radius (the CLS Argos
    ##  Kalman filter radius), with ellipse semi-axes that differ from it (see
    ##  kineis_radius_rows()). The imputation has no Argos ellipse model, so
    ##  their radius is used with the Kinéis error model anyway
    argos <- !is.na(r) & !kineis_radius_rows(d, r)
    if (any(argos))
      message(sum(argos), " location(s) have an error radius but error ellipse semi-axes that ",
              "differ from it: Argos (Kalman filter) locations from before Kinéis. Their error ",
              "radius is used with the Kinéis error model anyway")
    r <- (r / 1000)^exponent
    return(list(type = "kineis", east = scale[1] * r, north = scale[2] * r))
  }
  ## 2) standard errors
  pairs <- list(c("lon.se", "lat.se", "degrees"), c("lonerr", "laterr", "degrees"),
                c("x.se", "y.se", "km"))
  for (p in pairs) {
    if (all(p[1:2] %in% names(d))) {
      se_e <- as.numeric(d[[p[1]]])
      se_n <- as.numeric(d[[p[2]]])
      if (any(se_e < 0 | se_n < 0, na.rm = TRUE))
        stop("location standard errors (", p[1], ", ", p[2], ") must not be negative", call. = FALSE)
      units <- if (is.null(error.units)) p[3] else error.units
      if (units == "degrees") {
        se_n <- se_n * pi / 180 * earth
        se_e <- se_e * pi / 180 * earth * cos(lat * pi / 180)
      }
      return(list(type = paste0("standard errors (", p[1], ", ", p[2], "; ", units, ")"),
                  east = se_e, north = se_n))
    }
  }
  ## 3) 95% intervals, in degrees
  ci <- c("lon.lci", "lon.uci", "lat.lci", "lat.uci")
  if (all(ci %in% names(d))) {
    if (identical(error.units, "km"))
      stop("95% interval columns (lon.lci, lon.uci, lat.lci, lat.uci) are longitudes and latitudes, in degrees",
           call. = FALSE)
    z <- stats::qnorm(0.975)
    se_e <- (as.numeric(d$lon.uci) - as.numeric(d$lon.lci)) / (2 * z)
    se_n <- (as.numeric(d$lat.uci) - as.numeric(d$lat.lci)) / (2 * z)
    if (any(se_e < 0 | se_n < 0, na.rm = TRUE))
      stop("95% intervals must have lower limits below their upper limits", call. = FALSE)
    return(list(type = "95% intervals (degrees)",
                east = se_e * pi / 180 * earth * cos(lat * pi / 180),
                north = se_n * pi / 180 * earth))
  }
  message("no location error columns found (error.radius; lon.se, lat.se; lonerr, laterr; ",
          "x.se, y.se; or lon.lci, lon.uci, lat.lci, lat.uci): locations are taken as known")
  list(type = "none", east = rep(0, n), north = rep(0, n))
}

## TRUE if a data frame of locations carries errors that location_errors()
##  reads
has_location_errors <- function(x) {
  if (is.character(x)) return(TRUE)
  nm <- tolower(names(x))
  !is.null(error_radius_column(nm)) ||
    all(c("lon.se", "lat.se") %in% nm) || all(c("lonerr", "laterr") %in% nm) ||
    all(c("x.se", "y.se") %in% nm) ||
    all(c("lon.lci", "lon.uci", "lat.lci", "lat.uci") %in% nm)
}

## behavioural estimates at the observation times of the input data: columns
##  `cols` of `est` (on the regular grid, with id, segment and date) are
##  interpolated linearly in time between the grid times on either side,
##  within a track segment. Observations outside the segments (e.g. in a data
##  gap longer than max_gap) get NA
bm_data <- function(data, est, cols) {
  out <- as.data.frame(data)
  est <- as.data.frame(est)
  if (!"segment" %in% names(est)) est$segment <- 1L
  for (v in cols) out[[v]] <- NA_real_
  for (i in unique(est$id)) {
    e <- est[est$id == i, ]
    e <- e[order(e$date), ]
    r <- which(out$id == i & !is.na(out$date))
    if (length(r) == 0) next
    tg <- as.numeric(e$date)
    ng <- length(tg)
    t <- as.numeric(out$date[r])
    j <- findInterval(t, tg)
    j0 <- pmax(j, 1)
    j1 <- pmin(j + 1, ng)
    exact <- j >= 1 & tg[j0] == t
    inside <- j >= 1 & j < ng & e$segment[j0] == e$segment[j1]
    w <- ifelse(inside, (t - tg[j0]) / (tg[j1] - tg[j0]), 0)
    use <- exact | inside
    for (v in cols) {
      val <- (1 - w) * e[[v]][j0] + w * e[[v]][j1]
      val[exact] <- e[[v]][j0][exact]
      out[[v]][r[use]] <- val[use]
    }
  }
  tibble(out)
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
            length(keep), " locations")
  d[keep, ]
}
