##' @title fit a **Move Persistence Model** (`mpm`)
##'
##' @description fit a random walk with time-varying move persistence (g) to
##' animal tracks. With `reps`, the model is fitted to tracks imputed from an
##' SSM fit or from Kinéis locations (see [impute_tracks()]), and the estimates
##' are pooled with Rubin's rules, so they carry location uncertainty. Without
##' `reps`, the model is fitted once, treating the locations as data.
##' [fit_bm()] fits this and the other behavioural models through one
##' interface (`model = "mpm"`); `fit_mpm()` is kept for compatibility.
##'
##' @param x one of:
##' 1. an `ssm_df` fit object from [fit_ssm()] (`rw`, `crw` or `mp`);
##' 2. a data frame of locations with their errors (Kinéis error radii,
##' standard errors or 95% intervals; see [impute_tracks()]), or the path to
##' a csv file of them, fitted by multiple imputation with `reps` and
##' `time.step`. Kinéis locations need `reps`; other locations with errors
##' are fitted as data without `reps`, ignoring the errors;
##' 3. a data frame of locations without errors: `id`, `date` and two
##' coordinate columns (see `coords`). Without `reps` they are fitted as data,
##' at their own times; with `reps` and `time.step`, they are interpolated to
##' a regular grid as known locations (every imputation is the same).
##' @param what for an `ssm_df` fit without `reps`, the locations fitted:
##' `"predicted"` (default), `"fitted"` or `"rerouted"`. With `reps`, the
##' imputed tracks are at the predicted times, so only `"predicted"`
##' @param model mpm model to fit; either `jmpm` (default) with a single,
##' pooled random walk variance parameter (`sigma_g`) for all tracks, or `mpm`
##' with a separate variance parameter (`sigma_(g,i)`) for each track, fitted
##' separately
##' @param coords for a data frame of locations, the column numbers of the
##' coordinates (default = 3:4). Coordinates named `lon` and `lat` are kept as
##' the track's longitude and latitude; others (e.g. projected `x`, `y`) are
##' fitted, but `lon` and `lat` are missing in the output, so the track cannot
##' be mapped
##' @param control list of control settings for the outer optimizer
##' (see [aniMotum::mpm_control] for details). With `verbose = 1` (default),
##' more than one fit (`mpm` with more than one track, or `reps`) shows a
##' progress bar over the fits; a single fit (e.g. `jmpm` to an `ssm_df` fit)
##' shows the number of objective function evaluations and the time elapsed.
##' `verbose = 0` is silent
##' @param inner.control list of control parameters for the inner optimization
##' @param verbose is deprecated, use ssm_control(verbose = 1) instead,
##' see [aniMotum::ssm_control] for details
##' @param optim is deprecated, use ssm_control(optim = "optim") instead,
##' see [aniMotum::ssm_control] for details
##' @param optMeth is deprecated, use ssm_control(method = "L-BFGS-B") instead,
##' see [ssm_control] for details
##' @param reps if NULL (default), the model is fitted once, treating the
##' locations as data. If a number, the model is fitted to `reps` tracks
##' imputed with [impute_tracks()], and the estimates are pooled with Rubin's
##' rules (see details)
##' @param time.step for Kinéis locations, the time step (h) of the regular
##' grid (see [impute_tracks()]); required
##' @param max_gap for Kinéis locations, the longest gap (h) interpolated
##' across (see [impute_tracks()])
##' @param kineis for Kinéis locations, the Kinéis error model (see
##' [impute_tracks()])
##' @param error.units for location data with standard-error columns, their
##' units, `"degrees"` or `"km"`, if not those the column names imply (see
##' [impute_tracks()])
##' @param haulout optional haulout bouts at which imputed track segments end
##' (see [impute_tracks()]); needs `reps`. NULL (default): not split.
##' @param vmax for Kinéis locations, the maximum travel rate (m/s) of a
##' speed-distance-angle filter applied before imputation, to drop extreme
##' locations (see [impute_tracks()]). NULL (default): no filter
##' @param crs the projection (proj4 string, units km) in which the model is
##' fitted. If NULL (default), chosen from the extent of all the locations by
##' [auto_crs()] (see [impute_tracks()]). Ignored for a data frame of
##' projected coordinates
##'
##' @return a list with class `mpm_df`:
##' 1. `fitted`: a tibble with, for each id, track segment and time, the
##' track's longitude and latitude (`lon`, `lat`: the SSM-estimated locations,
##' the Kinéis locations interpolated onto the regular grid without
##' perturbation, or the locations supplied), move persistence `g`, its
##' standard error (`g.se`; with `reps`, the total standard error from
##' Rubin's rules) and its 95% interval (`lower`, `upper`, truncated to 0 and
##' 1), and, with
##' `reps`, the fraction of missing information (`fraction_missing`: the share
##' of the variance due to location uncertainty);
##' 2. `par`: estimates of `sigma_g`, `sigma_x` and `sigma_y`, with 95%
##' intervals, by track (`mpm`) or joint (`jmpm`). `sigma_x` and `sigma_y` are
##' in the fitted coordinates divided by their largest absolute value;
##' 3. `convergence`: a tibble, by track (`mpm`) or joint (`jmpm`), of the
##' number of fits used (`fits`: 1, or the imputations pooled), the number
##' that failed or did not converge (`failed`), and, without `reps`, whether
##' the Hessian was positive definite (`pdHess`) and the `aic`;
##' 4. `tmb`: without `reps`, the TMB object of each fit (e.g. for
##' `TMB::sdreport(fit$tmb[[1]], getJointPrecision = TRUE)`); NULL with `reps`;
##' 5. `model`, `reps` (0 without imputation), `source` (`"ssm"`, `"kineis"` or
##' `"data"`), `time.step` (h; NA for irregular locations), `what` (the
##' `ssm_df` locations fitted) and `crs` (the projection fitted in; NA for a
##' data frame of projected coordinates);
##' 6. with `reps`, `imputed`: each imputation's `g` and `g.se` (`rep`, `id`,
##' `segment`, `date`), for analyses that carry location uncertainty forward
##' (see [join()]);
##' 7. for a data frame input, `data`: the input, every column, with `keep`
##' (whether each location was used) and g and its 95% interval (`g`,
##' `lower`, `upper`) interpolated to the observation times (NA outside the
##' fitted track segments).
##'
##' @details With `reps`, the move persistence model is fitted to each imputed
##' track, treating its locations as data, and Rubin's rules pool `g` at
##' each time and the log of each parameter. This replaces treating the SSM
##' point estimates as data, which ignores location uncertainty. Kinéis
##' locations are estimates with known error, at irregular times, so they are
##' only fitted this way. Imputed Kinéis tracks are split into segments at
##' gaps longer than `max_gap`; with `mpm` each segment is fitted separately,
##' and with `jmpm` each segment is a track. Track segments with fewer than 3
##' locations are dropped.
##'
##' The model is fitted in projected coordinates (km), in one conformal
##' projection for all individuals, chosen to suit the data (`crs`): a single
##' `sigma_x`, `sigma_y` is then not averaging over places stretched by
##' different amounts, as in Mercator over a wide range of latitudes.
##' Coordinates are divided by one common constant, their largest absolute
##' value, for the optimiser; g does not depend on this scaling.
##'
##' Move persistence g follows a random walk on its own scale, with standard
##' deviation `sigma_g` per time step, held within 0 to 1 by a soft barrier
##' (a penalty that is near zero inside 0 to 1 and rises quadratically
##' outside it), rather than a random walk on logit(g). The first g of each
##' track has a diffuse Normal(0.5, 2) prior. With a logistic link the
##' Laplace approximation's inner problem is not convex in logit(g), and fits
##' often failed; on the scale of g it is convex, with a single mode.
##' `sigma_g` is therefore on the scale of g, not comparable with estimates
##' from earlier versions.
##'
##' @references
##' Jonsen ID, McMahon CR, Patterson TA, et al. (2019) Movement responses to
##' environment: fast inference of variation among southern elephant seals with
##' a mixed effects model. Ecology. 100(1):e02566
##'
##' @examples
##' ## fit jmpm to two southern elephant seal tracks
##' xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, control = ssm_control(verbose = 0))
##'
##' fmpm <- fit_mpm(xs, model = "jmpm")
##' fmpm$par
##'
##' ## with location uncertainty, by multiple imputation
##' \dontrun{
##' fmi <- fit_mpm(xs, model = "jmpm", reps = 20)
##' plot(fmi)
##'
##' ## directly from Kinéis locations, without an SSM
##' fki <- fit_mpm(kineis_locations, model = "jmpm", reps = 20, time.step = 12)
##' plot(fki, type = "track", pages = 1)
##' }
##'
##' @importFrom dplyr tibble bind_rows
##' @importFrom stats qnorm
##' @export
##' @md

fit_mpm <- function(x,
                    what = "predicted",
                    model = c("jmpm", "mpm"),
                    coords = 3:4,
                    control = mpm_control(),
                    inner.control = NULL,
                    optim = NULL,
                    optMeth = NULL,
                    verbose = NULL,
                    reps = NULL,
                    time.step = NULL,
                    max_gap = NULL,
                    kineis = ssm_control()$kineis,
                    crs = NULL,
                    vmax = NULL,
                    error.units = NULL,
                    haulout = NULL
                    ) {

  model <- match.arg(model)

  ## warnings for deprecated arguments
  if(!is.null(verbose)) {
    warning("the `verbose` arg is deprecated as of 0.7-5, use `control = ssm_control(verbose)` instead. See `?ssm_control for details",
            call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
    control$verbose <- verbose
  }
  if(!is.null(optim)) {
    warning("the `optim` arg is deprecated as of 0.7-5, use `control = ssm_control(optim)` instead. See `?ssm_control for details",
            call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
    if(optim %in% c("nlminb", "optim")) control$optim <- optim
    else stop("invalid optimiser specified, see ?ssm_control for options")
  }
  if(!is.null(optMeth)) {
    warning("the `optMeth` arg is deprecated as of 0.7-5, use `control = ssm_control(optMeth)` instead. See `?ssm_control for details",
            call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
    if(optMeth %in% c("L-BFGS-B", "BFGS", "Nelder-Mead", "CG", "SANN", "Brent"))
      control$method <- optMeth
    else stop("invalid optimisation method specified, see ?ssm_control for options")
  }


  ## locations: rep 0 is the point-estimate track; reps 1, 2, ... are imputed
  ##  tracks
  if (!is.null(haulout) && is.null(reps))
    stop("haulout splits imputed tracks: give `reps` (e.g. 20)", call. = FALSE)
  loc <- mpm_locations(x, what, coords, reps, time.step, max_gap, kineis, crs, vmax,
                       error.units, haulout)
  data <- attr(loc, "data")
  source <- attr(loc, "source")
  crs <- attr(loc, "crs")
  time.step <- attr(loc, "time.step")
  what <- attr(loc, "what")
  loc <- as.data.frame(loc)
  estimate <- loc[loc$rep == 0, ]

  ## rescale x and y for the optimiser by one common constant, so sigma_x and
  ##  sigma_y are in the same units, and by the same constant in every
  ##  imputation, so the sigma estimates can be pooled. g is unaffected: each
  ##  axis has its own sigma and the model is linear within each axis
  scale <- max(abs(c(estimate$x, estimate$y)), na.rm = TRUE)
  loc$x <- loc$x / scale
  loc$y <- loc$y / scale

  ## track units: each segment of each individual (a segment needs >= 3
  ##  locations for the move persistence model)
  loc$track <- paste(loc$id, loc$segment, sep = "_segment_")
  estimate$track <- paste(estimate$id, estimate$segment, sep = "_segment_")
  n.track <- table(estimate$track)
  short <- names(n.track)[n.track < 3]
  if (length(short) > 0) {
    message(length(short), " track segment(s) with fewer than 3 locations dropped")
    loc <- loc[!loc$track %in% short, ]
    estimate <- estimate[!estimate$track %in% short, ]
  }

  ## one fit per imputation (jmpm), or per imputation and track segment (mpm);
  ##  without reps, one fit to the point-estimate track
  units <- if (model == "jmpm") "all" else unique(estimate$track)
  imps <- if (is.null(reps)) 0L else seq_len(reps)
  ## progress bar over more than one fit; mpmf()'s parameter trace is
  ##  switched off, as it would overwrite the bar
  show <- control$verbose == 1 && length(units) * length(imps) > 1
  ctl <- control
  if (show) ctl$verbose <- 0
  if (show)
    cat(sprintf("fitting %s: %d fits (%s)\n", model, length(units) * length(imps),
                if (is.null(reps)) "one per track" else
                  paste(reps, if (model == "mpm") "imputations of each track" else "imputations")))
  pb <- progress_bar(length(units) * length(imps), show)
  fits <- lapply(units, function(u) {
    lapply(imps, function(k) {
      d <- loc[loc$rep == k & (u == "all" | loc$track == u), ]
      d <- data.frame(id = d$id, tid = if (model == "jmpm") d$segment else NA,
                      date = d$date, x = d$x, y = d$y)
      f <- mpm_fit_one(d, model, ctl, inner.control,
                       quiet = show || !is.null(reps), keep = is.null(reps))
      pb$tick()
      f
    })
  })
  pb$close()
  names(fits) <- units

  fitted <- list()
  par <- list()
  convergence <- list()
  imputed <- list()
  for (u in units) {
    rep_ok <- imps[!vapply(fits[[u]], is.null, logical(1))]
    f <- Filter(Negate(is.null), fits[[u]])
    convergence[[u]] <- tibble(unit = if (u == "all") "joint" else u,
                               fits = length(f),
                               failed = length(imps) - length(f),
                               pdHess = if (is.null(reps) && length(f) == 1) f[[1]]$pdHess else NA,
                               aic = if (is.null(reps) && length(f) == 1) f[[1]]$aic else NA_real_)
    if (length(f) == 0) {
      warning("the move persistence model failed for ",
              if (!is.null(reps)) "every imputation of " else "",
              if (u == "all") "the joint fit" else u, call. = FALSE)
      next
    }
    if (is.null(reps) && !f[[1]]$pdHess)
      warning("the Hessian is not positive definite for ",
              if (u == "all") "the joint fit" else u,
              "; standard errors are unreliable", call. = FALSE)
    e <- if (u == "all") estimate else estimate[estimate$track == u, ]
    e <- e[order(e$id, e$segment, e$date), ]
    ## mpmf() orders rows by id, tid, date: align each fit to e
    key <- function(z) paste(z$id, z$date)
    gm <- do.call(cbind, lapply(f, function(z) z$fitted$g[match(key(e), key(z$fitted))]))
    gse <- do.call(cbind, lapply(f, function(z) z$fitted$g.se[match(key(e), key(z$fitted))]))
    pr <- mpm_combine(gm, gse)
    fitted[[u]] <- tibble(id = e$id, segment = e$segment, date = e$date,
                          lon = e$lon, lat = e$lat,
                          ## g can stray slightly outside 0 to 1 (its bounds are
                          ##  soft; see src/TMB/sub/move_persistence.hpp):
                          ##  report it, and its interval, within 0 to 1
                          g = pmin(pmax(pr$estimate, 0), 1), g.se = pr$se,
                          lower = pmax(pr$lower, 0), upper = pmin(pr$upper, 1))
    if (!is.null(reps)) {
      fitted[[u]]$fraction_missing <- pr$fraction_missing
      ## each imputation's estimates, for analyses that carry location
      ##  uncertainty forward
      imputed[[u]] <- bind_rows(lapply(seq_along(f), function(k)
        tibble(rep = rep_ok[k], id = e$id, segment = e$segment, date = e$date,
               g = pmin(pmax(gm[, k], 0), 1), g.se = gse[, k])))
    }
    pp <- mpm_combine(do.call(cbind, lapply(f, function(z) z$par[, 1])),
                      do.call(cbind, lapply(f, function(z) z$par[, 2])))
    nm <- rownames(f[[1]]$par)
    nm[nm == "log_sigma"] <- c("log_sigma_x", "log_sigma_y")
    par[[u]] <- tibble(unit = if (u == "all") "joint" else u,
                       parameter = sub("^log_", "", nm),
                       estimate = exp(pp$estimate),
                       lower = exp(pp$lower), upper = exp(pp$upper))
  }
  if (length(fitted) == 0) stop("the move persistence model failed for every track", call. = FALSE)

  tmb <- NULL
  if (is.null(reps)) {
    tmb <- lapply(fits, function(f) if (is.null(f[[1]])) NULL else f[[1]]$tmb)
    names(tmb) <- vapply(units, function(u) if (u == "all") "joint" else u, character(1))
  }

  out <- list(fitted = bind_rows(fitted),
              par = bind_rows(par),
              convergence = bind_rows(convergence),
              tmb = tmb,
              model = model,
              reps = if (is.null(reps)) 0L else reps,
              source = source,
              time.step = time.step,
              what = what,
              crs = crs)
  if (!is.null(reps)) out$imputed <- bind_rows(imputed)
  ## data frame input: the input, with the estimates at its observation times
  if (!is.null(data)) out$data <- bm_data(data, out$fitted, c("g", "lower", "upper"))
  class(out) <- append("mpm_df", class(out))
  out
}

## the locations to fit, as a tibble with columns rep (0 = the point-estimate
##  track), id, segment, date, lon, lat, x, y, and attributes source,
##  time.step and what
mpm_locations <- function(x, what, coords, reps, time.step, max_gap, kineis, crs = NULL,
                          vmax = NULL, error.units = NULL, haulout = NULL) {
  ## location data with errors (Kinéis error radii, standard errors or 95%
  ##  intervals)
  error.data <- !inherits(x, "ssm_df") &&
    (is.character(x) || (is.data.frame(x) && has_location_errors(x)))
  if (!is.null(reps)) {
    if (inherits(x, "ssm_df") && !grepl("^p", what))
      stop("with `reps`, imputed tracks are at the predicted (regular) times; use what = \"predicted\"",
           call. = FALSE)
    out <- impute_tracks(x, reps = reps, time.step = time.step, max_gap = max_gap,
                         kineis = kineis, crs = crs, vmax = vmax, error.units = error.units,
                         haulout = haulout)
    attr(out, "what") <- if (inherits(x, "ssm_df")) "predicted" else NA_character_
    return(out)
  }
  ## Kinéis locations are estimates at irregular times: fit them only by
  ##  multiple imputation. Other locations with errors (e.g. grab() output
  ##  with x.se, y.se) are fitted as data, as before, ignoring the errors
  kineis.data <- error.data && (is.character(x) ||
    !is.null(error_radius_column(names(x))))
  if (kineis.data)
    stop("Kinéis locations are estimates with known error, at irregular times: fit them by multiple imputation, with `reps` and `time.step` (h), e.g. fit_bm(x, model = \"mpm\", reps = 20, time.step = 12)",
         call. = FALSE)
  if (error.data)
    message("location errors are not used without `reps`: the locations are fitted as data. ",
            "Give `reps` and `time.step` to fit by multiple imputation from the errors")
  if (inherits(x, "ssm_df")) {
    g <- grab(x, what = what, as_sf = FALSE)
    what <- c("fitted", "predicted", "rerouted")[pmatch(what, c("fitted", "predicted", "rerouted"))]
    ## projected in crs, chosen from all the locations if not given, as for
    ##  imputed tracks
    if (is.null(crs)) crs <- auto_crs(g$lon, g$lat)
    xy <- lonlat_to_xy(g$lon, g$lat, crs)
    out <- tibble(rep = 0L, id = g$id, segment = 1L, date = g$date,
                  lon = g$lon, lat = g$lat, x = xy[, 1], y = xy[, 2])
    attr(out, "crs") <- crs
    attr(out, "source") <- "ssm"
    attr(out, "time.step") <- if (what == "predicted") x$ssm[[1]]$ts else NA_real_
    attr(out, "what") <- what
  } else if (is.data.frame(x)) {
    d <- as.data.frame(x)
    lonlat <- identical(tolower(names(d)[coords]), c("lon", "lat"))
    if (lonlat) {
      ## longitude and latitude are projected, as for the other inputs
      if (is.null(crs)) crs <- auto_crs(d[[coords[1]]], d[[coords[2]]])
      xy <- lonlat_to_xy(d[[coords[1]]], d[[coords[2]]], crs)
    } else {
      crs <- NA_character_
      xy <- cbind(d[[coords[1]]], d[[coords[2]]])
    }
    out <- tibble(rep = 0L, id = d[[1]], segment = 1L, date = d[[2]],
                  lon = if (lonlat) d[[coords[1]]] else NA_real_,
                  lat = if (lonlat) d[[coords[2]]] else NA_real_,
                  x = xy[, 1], y = xy[, 2])
    attr(out, "crs") <- crs
    attr(out, "source") <- "data"
    attr(out, "time.step") <- NA_real_
    attr(out, "what") <- NA_character_
    ## the input, every column, with standard id and date columns
    data <- d
    data$id <- d[[1]]
    data$date <- d[[2]]
    data$keep <- TRUE
    attr(out, "data") <- data
  } else {
    stop("x must be an ssm_df fit object or a data frame of locations",
         call. = FALSE)
  }
  out
}

## fit the move persistence model once. Returns NULL if the fit crashed or
##  did not converge (or, for an imputed track, if the Hessian is not positive
##  definite); otherwise the fitted values, the log-scale parameters, and,
##  if keep, the TMB object
mpm_fit_one <- function(d, model, control, inner.control, quiet, keep) {
  ## mpmf() prints optimiser errors from its own try(); silence them, the
  ##  failed fit is caught and counted by fit_mpm()
  old <- options(try.outFile = nullfile())
  on.exit(options(old))
  f <- try(suppressWarnings({
    if (quiet) {
      utils::capture.output(r <- mpmf(d, model = model, control = control,
                                      inner.control = inner.control))
      r
    } else {
      mpmf(d, model = model, control = control, inner.control = inner.control)
    }
  }), silent = TRUE)
  if (inherits(f, "try-error") || !inherits(f$opt, "list") || f$opt$convergence != 0 ||
      inherits(f$rep, "try-error")) return(NULL)
  pdHess <- isTRUE(f$rep$pdHess)
  if (!keep && !pdHess) return(NULL)
  fx <- summary(f$rep, "fixed")
  list(fitted = as.data.frame(f$fitted), par = fx[, 1:2, drop = FALSE],
       tmb = if (keep) f$tmb, aic = f$aic, pdHess = pdHess)
}

## estimate, standard error and 95% interval from one fit (Wald), or pooled
##  across imputations (Rubin's rules); columns of est and se are fits
mpm_combine <- function(est, se) {
  if (ncol(est) > 1) return(pool_rubin(est, se))
  est <- est[, 1]
  se <- se[, 1]
  list(estimate = est, se = se,
       lower = est - qnorm(0.975) * se, upper = est + qnorm(0.975) * se)
}

## print a summary of a fit_mpm() fit, not its TMB objects
##' @method print mpm_df
##' @export
##' @noRd
print.mpm_df <- function(x, ...) {
  ## fit_mpm() output before aniMotum 1.3-1 is a tibble
  if (is.data.frame(x)) return(NextMethod())
  cat("move persistence model (", x$model, ") fitted to ",
      switch(x$source, ssm = paste(x$what, "SSM locations"),
             kineis = "Kinéis locations",
             data = if (x$reps > 0) "location data" else "locations as data"),
      if (!is.na(x$time.step)) paste0(" at a ", x$time.step, " h time step") else "",
      if (x$reps > 0) paste0(", by multiple imputation (", x$reps, " imputations)") else "",
      "\n", sep = "")
  cat(length(unique(x$fitted$id)), "track(s),", nrow(x$fitted), "estimates of g\n\n")
  print(x$par, ...)
  invisible(x)
}
