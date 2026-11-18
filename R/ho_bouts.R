##' @title Merge haulout records into haulout bouts
##'
##' @description Reads SMRU-format haulout records (a file or a data frame)
##' and merges records of the same individual separated by less than `gap`
##' hours into haulout bouts. SMRU tags start a new haulout record whenever
##' the tag is wetted, so one stay ashore (or on ice, or on a nesting beach)
##' is usually several records. The bouts can be filtered (e.g. by duration
##' or location) and passed to [fit_bm()] or [impute_tracks()] as
##' `haulout`, to end imputed track segments at each bout.
##'
##' @param haulout the path to an SMRU haulout csv file, or a data frame, with
##' columns `ref` (or the column named by `ref`), `s_date` and `e_date`
##' (`YYYY-MM-DDTHH:MM:SSZ` text, or POSIXct). Longitude and latitude
##' columns (`lon`, `lat`, or SMRU QC output's `ssm_lon`, `ssm_lat`) are
##' used if present.
##' @param gap records of an individual separated by less than `gap` hours
##' are merged into one bout (default 12).
##' @param min_dur bouts shorter than `min_dur` hours are dropped (default 0:
##' keep all).
##' @param ref name of the individual id column in `haulout` (default
##' `"ref"`, the SMRU name).
##' @param id_fun optional function applied to the ids, e.g. to match the
##' ids of the tracking data (see [smru_haulout()]).
##' @param tz time zone of the dates (default `"UTC"`).
##'
##' @return a tibble, one row per bout: `id`, `bout` (numbered within id),
##' `start`, `end`, `dur` (h), `n_records`, and the median `lon` and `lat` of
##' its records (NA if not available).
##'
##' @details SMRU tags record a haulout when the wet-dry sensor stays dry for
##' a set time (typically 10 min). At sea, long surface intervals can also be
##' recorded as haulouts. Filter the bouts before use, e.g. by `min_dur`, or
##' by their distance from land or from the colony.
##'
##' @examples
##' h <- data.frame(ref = "a",
##'                 s_date = c("2024-01-01T10:00:00Z", "2024-01-01T14:00:00Z",
##'                            "2024-01-05T02:00:00Z"),
##'                 e_date = c("2024-01-01T12:00:00Z", "2024-01-01T20:00:00Z",
##'                            "2024-01-05T02:20:00Z"))
##' ho_bouts(h)
##' ho_bouts(h, min_dur = 1)
##'
##' @importFrom dplyr tibble
##' @importFrom stats median
##' @importFrom utils read.csv
##' @export
##' @md
ho_bouts <- function(haulout,
                     gap = 12,
                     min_dur = 0,
                     ref = "ref",
                     id_fun = NULL,
                     tz = "UTC") {
  if (is.character(haulout)) {
    if (!file.exists(haulout)) stop("haulout file not found: ", haulout, call. = FALSE)
    haulout <- read.csv(haulout, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(haulout))
    stop("haulout must be a file path or a data frame", call. = FALSE)
  h <- as.data.frame(haulout)
  if (!ref %in% names(h))
    stop("column '", ref, "' not found in haulout; use `ref` to name the id column",
         call. = FALSE)
  if (!all(c("s_date", "e_date") %in% names(h)))
    stop("haulout must have columns `s_date` and `e_date`", call. = FALSE)

  id <- as.character(h[[ref]])
  if (!is.null(id_fun)) id <- id_fun(id)
  start <- ho_dates(h$s_date, tz)
  end <- ho_dates(h$e_date, tz)
  lon <- if ("lon" %in% names(h)) h$lon else if ("ssm_lon" %in% names(h)) h$ssm_lon else NA_real_
  lat <- if ("lat" %in% names(h)) h$lat else if ("ssm_lat" %in% names(h)) h$ssm_lat else NA_real_
  d <- data.frame(id = id, start = start, end = end,
                  lon = as.numeric(lon), lat = as.numeric(lat), stringsAsFactors = FALSE)
  bad <- is.na(d$start) | is.na(d$end) | d$end < d$start
  if (any(bad)) {
    warning(sum(bad), " haulout record(s) without valid start and end dates dropped",
            call. = FALSE)
    d <- d[!bad, ]
  }
  if (nrow(d) == 0) stop("no valid haulout records", call. = FALSE)

  out <- lapply(split(d, d$id), function(s) {
    s <- s[order(s$start), ]
    n <- nrow(s)
    ## a new bout starts when a record begins more than `gap` h after the
    ##  latest end of the records before it
    latest <- cummax(as.numeric(s$end))
    new <- c(TRUE, (as.numeric(s$start[-1]) - latest[-n]) / 3600 > gap)
    b <- cumsum(new)
    data.frame(id = s$id[1], bout = unique(b),
               start = as.POSIXct(tapply(as.numeric(s$start), b, min), origin = "1970-01-01", tz = tz),
               end = as.POSIXct(tapply(as.numeric(s$end), b, max), origin = "1970-01-01", tz = tz),
               n_records = as.integer(tabulate(b)),
               lon = as.numeric(tapply(s$lon, b, median, na.rm = TRUE)),
               lat = as.numeric(tapply(s$lat, b, median, na.rm = TRUE)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  out$dur <- as.numeric(difftime(out$end, out$start, units = "hours"))
  out <- out[out$dur >= min_dur, c("id", "bout", "start", "end", "dur", "n_records", "lon", "lat")]
  row.names(out) <- NULL
  tibble(out)
}

## haulout dates: POSIXct as given, or SMRU text (YYYY-MM-DDTHH:MM:SSZ), or
##  other text that as.POSIXct() reads
ho_dates <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXct")) return(x)
  x <- as.character(x)
  out <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = tz)
  other <- is.na(out) & !is.na(x)
  if (any(other))
    out[other] <- suppressWarnings(as.POSIXct(x[other], tz = tz, optional = TRUE))
  out
}

## haulout bouts from the `haulout` argument of impute_tracks(): a data frame
##  of bouts (id, start, end; e.g. from ho_bouts()) is used as given; SMRU
##  haulout records (a file, or a data frame with s_date and e_date) are
##  merged into bouts by ho_bouts() with its defaults
ho_as_bouts <- function(haulout) {
  if (is.data.frame(haulout) && all(c("id", "start", "end") %in% names(haulout))) {
    b <- as.data.frame(haulout)
    b$start <- ho_dates(b$start)
    b$end <- ho_dates(b$end)
    return(b[!is.na(b$start) & !is.na(b$end), c("id", "start", "end")])
  }
  as.data.frame(ho_bouts(haulout))[, c("id", "start", "end")]
}

## end imputed track segments at haulout bouts: grid times inside a bout are
##  dropped, and a new segment starts at the first grid time after each bout,
##  so no step spans time ashore. The same grid times are used in every
##  imputation, so the split is found on rep 0 and applied to all
ho_split <- function(imp, haulout) {
  b <- ho_as_bouts(haulout)
  ids <- unique(imp$id)
  b <- b[b$id %in% ids, ]
  if (nrow(b) == 0) {
    warning("no haulout bouts match the ids of the tracks; tracks not split", call. = FALSE)
    return(imp)
  }
  g <- as.data.frame(imp[imp$rep == 0, c("id", "segment", "date")])
  g <- g[order(g$id, g$date), ]
  g$drop <- FALSE
  g$brk <- FALSE
  for (i in unique(b$id)) {
    r <- which(g$id == i)
    t <- as.numeric(g$date[r])
    bi <- b[b$id == i, ]
    for (k in seq_len(nrow(bi))) {
      s <- as.numeric(bi$start[k])
      e <- as.numeric(bi$end[k])
      g$drop[r[t >= s & t <= e]] <- TRUE
      ## the first grid time after the bout starts a new segment
      after <- which(t > e)
      if (length(after) > 0 && any(t < s)) g$brk[r[min(after)]] <- TRUE
    }
  }
  keep <- g[!g$drop, ]
  ## renumber segments within id: a new segment at an existing segment
  ##  boundary, after dropped grid times, or after a bout
  keep$segment <- unlist(lapply(split(keep, keep$id), function(s) {
    n <- nrow(s)
    cumsum(c(TRUE, s$segment[-1] != s$segment[-n] | s$brk[-1]))
  }), use.names = FALSE)
  n_drop <- sum(g$drop)
  n_brk <- sum(keep$brk)
  message("haulout: ", nrow(b), " bout(s); ", n_drop, " grid time(s) dropped; ",
          n_brk, " segment break(s) added")
  key <- paste(imp$id, as.numeric(imp$date))
  m <- match(key, paste(keep$id, as.numeric(keep$date)))
  out <- imp[!is.na(m), ]
  out$segment <- as.integer(keep$segment[m[!is.na(m)]])
  attrs <- attributes(imp)[setdiff(names(attributes(imp)), c("names", "row.names", "class"))]
  for (a in names(attrs)) attr(out, a) <- attrs[[a]]
  out
}
