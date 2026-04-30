##' @title Add a haulout indicator to tracking data from an SMRU haulout file
##'
##' @description reads an SMRU-format haulout data file (or accepts a
##' pre-read data frame) and stamps a binary haulout indicator \code{ho} onto
##' each row of the tracking data based on whether that observation's timestamp
##' falls within any haulout interval [\code{s_date}, \code{e_date}] for the
##' matching individual. The resulting \code{ho} column is recognised
##' automatically by \code{fit_ssm()} when supplied via the \code{haulout}
##' argument, and is used by the \code{mp}, \code{crw}, and \code{rw} process
##' models to constrain location estimates during haulout periods.
##'
##' @param x a `data.frame`, `tibble` or `sf-tibble` of tracking observations
##' with at least columns `id` and `date` (POSIXct or coercible to one).
##' Typically the raw input to [aniMotum::fit_ssm], before [aniMotum::format_data]
##' is called.
##' @param haulout either a character string giving the path to the SMRU
##' haulout CSV file, or a pre-read data frame. Must contain columns matching
##' `ref` (or the column named by the `ref` argument), `s_date`, and `e_date`.
##' Only the `s_date` and `e_date` columns are used to define haulout intervals;
##' all other columns (including lat, lon) are ignored.
##' @param ref name of the column in `haulout` that identifies individuals and
##' corresponds to the `id` column in `x`. Default `"ref"`, which is the
##' standard SMRU column name.
##' @param tz timezone for parsing `s_date` and `e_date`. Default `"UTC"`.
##' @param id_fun optional function applied to the `ref` column of the haulout
##' file to transform individual IDs before matching against `x$id`. Useful
##' when the two files use different naming conventions, e.g.,
##' `id_fun = function(x) gsub("-", "_", x)` to replace hyphens with
##' underscores. Default `NULL` (no transformation).
##'
##' @details the SMRU haulout file contains one row per haulout event with
##' columns `s_date` (haulout start) and `e_date` (haulout end) in ISO 8601
##' UTC format (`YYYY-MM-DDTHH:MM:SSZ`). Interval membership is inclusive at
##' both boundaries: an observation at exactly `s_date` or `e_date` is coded
##' `ho = 1`.
##'
##' This function is intended to be called either directly by the user before
##' [aniMotum::fit_ssm], or internally via the `haulout` argument of
##' `fit_ssm`. In either case, the `ho` column must be present in `x` before
##' `fit_ssm` is called, as `ho` does not survive [aniMotum::format_data] or
##' [aniMotum::prefilter].
##'
##' A warning is issued for any individual in `x` that has no matching records
##' in the haulout file. All observations for that individual will have
##' `ho = 0`, which is the safe fallback (no haulout constraint applied).
##'
##' @return `x` with an additional integer column `ho`: `1` if the observation
##' falls within a haulout period, `0` otherwise. If `x` already has an `ho`
##' column it is overwritten with a warning.
##'
##' @examples
##' \dontrun{
##' ## called directly - add ho before fitting
##' d <- read.csv("my_tracks.csv")
##' d <- smru_haulout(d, "haulout_ct189.csv")
##' fit <- fit_ssm(d, model = "mp", time.step = 24,
##'                control = ssm_control(ho_scale = 0.01, verbose = 0))
##'
##' ## called via fit_ssm haulout argument - equivalent to above
##' fit <- fit_ssm(d, model = "mp", time.step = 24,
##'                haulout = "haulout_ct189.csv",
##'                control = ssm_control(ho_scale = 0.01, verbose = 0))
##'
##' ## pre-read and filter the haulout file before passing to fit_ssm
##' ho_raw <- read.csv("haulout_ct189.csv")
##' ho_raw <- subset(ho_raw, cid == "ct189")
##' fit <- fit_ssm(d, model = "mp", time.step = 24,
##'                haulout = ho_raw,
##'                control = ssm_control(ho_scale = 0.01, verbose = 0))
##'
##' ## IDs differ between files: haulout uses "ct189-576-25",
##' ## tracking data uses "ct189_576_25"
##' d <- smru_haulout(d, "haulout_ct189.csv",
##'                   id_fun = function(x) gsub("-", "_", x))
##' }
##'
##' @importFrom dplyr left_join
##' @export
##' @md

smru_haulout <- function(x,
                         haulout,
                         ref    = "ref",
                         tz     = "UTC",
                         id_fun = NULL) {

  ## ---- Input validation ---------------------------------------------------

  if (!is.data.frame(x))
    stop("x must be a data.frame, tibble or sf-tibble of tracking observations")
  if (!all(c("id", "date") %in% names(x)))
    stop("x must have columns `id` and `date`")

  ## Read haulout file if a path was supplied
  if (is.character(haulout)) {
    if (!file.exists(haulout))
      stop("haulout file not found: ", haulout)
    haulout <- read.csv(haulout, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(haulout))
    stop("`haulout` must be a file path or a data frame")
  if (!ref %in% names(haulout))
    stop("column '", ref, "' not found in haulout data; ",
         "use the `ref` argument to specify the correct column name")
  if (!all(c("s_date", "e_date") %in% names(haulout)))
    stop("`haulout` must contain columns `s_date` and `e_date`")

  ## ---- Parse haulout dates ------------------------------------------------

  parse_dt <- function(x) {
    out <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = tz)
    if (any(is.na(out)))
      warning("some haulout dates could not be parsed and will be ignored; ",
              "expected format: YYYY-MM-DDTHH:MM:SSZ",
              call. = FALSE)
    out
  }

  haulout$s_date <- parse_dt(haulout$s_date)
  haulout$e_date <- parse_dt(haulout$e_date)
  haulout        <- haulout[!is.na(haulout$s_date) & !is.na(haulout$e_date), ]

  ## Retain only the three columns needed
  ho        <- haulout[, c(ref, "s_date", "e_date")]
  names(ho)[1] <- "id"

  ## Apply optional ID transformation
  if (!is.null(id_fun)) {
    if (!is.function(id_fun))
      stop("`id_fun` must be a function or NULL")
    ho$id <- id_fun(ho$id)
  }

  ## ---- Ensure x$date is POSIXct ------------------------------------------

  if (!inherits(x$date, "POSIXct"))
    x$date <- as.POSIXct(x$date, tz = tz)

  ## ---- Warn if ho column already exists ------------------------------------

  if ("ho" %in% names(x))
    warning("`x` already has an `ho` column; it will be overwritten",
            call. = FALSE)

  ## ---- Stamp ho onto tracking data ----------------------------------------

  x$ho     <- 0L
  ids      <- unique(x$id)
  no_match <- character(0)

  for (id_i in ids) {

    obs_idx <- which(x$id == id_i)
    ho_i    <- ho[ho$id == id_i, ]

    if (nrow(ho_i) == 0L) {
      no_match <- c(no_match, id_i)
      next
    }

    obs_dates <- x$date[obs_idx]

    in_ho <- vapply(obs_dates, function(d) {
      any(!is.na(ho_i$s_date) &
            d >= ho_i$s_date  &
            d <= ho_i$e_date)
    }, logical(1L))

    x$ho[obs_idx] <- as.integer(in_ho)
  }

  if (length(no_match) > 0)
    warning("no haulout records found for the following individual(s); ",
            "all observations will have ho = 0:\n  ",
            paste(no_match, collapse = ", "), "\n",
            "check that the `ref` column in the haulout file matches `id` ",
            "in the tracking data, or use `id_fun` to reconcile naming differences",
            call. = FALSE)

  ## ---- Summary ------------------------------------------------------------

  n_ho  <- sum(x$ho)
  n_tot <- nrow(x)
  message(sprintf(
    "smru_haulout: %d of %d observations (%.1f%%) marked as haulout (ho = 1)",
    n_ho, n_tot, 100 * n_ho / n_tot
  ))

  x
}
