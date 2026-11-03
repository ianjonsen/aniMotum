##' @title Prepare a single track for state-space model fitting
##'
##' @description Builds the prediction grid, time differences, haulout and data
##' gap indicators, observation model indices, and state initial values for a
##' single individual. This is the data preparation stage required by all of
##' aniMotum's state-space models.
##'
##' At present `ssm_prep` is used only by the joint (hierarchical) filter,
##' [aniMotum::jsfilter]. `sfilter` and `mpfilter` each carry their own copy of
##' this logic (they share 358 identical lines between them); migrating both
##' onto `ssm_prep` is the intended follow-up once this function has been
##' validated against their existing behaviour. It is written to be a drop-in
##' replacement for that block rather than a parallel implementation.
##'
##' @param x an sf-tibble for a single individual, as produced by
##' [aniMotum::prefilter]
##' @param time.step prediction interval in hours, a data.frame of prediction
##' times with `id` and `date`, or NA to estimate states at observation times
##' only
##' @param fit.to.subset fit to the prefiltered subset of observations (TRUE)
##' or to all observations (FALSE)
##' @param control a list of control values from [aniMotum::ssm_control]
##' @param ho_lookup optional data.frame of haulout indicators keyed on `id`
##' and `date`, as assembled by [aniMotum::fit_ssm]
##'
##' @return a named list with components `d.all` (the merged observation and
##' prediction grid), `prj` (the coordinate reference system), `dt`, `Y`,
##' `isd`, `obs_mod`, `K`, `m`, `M`, `c`, `GLerr`, `state0`, `gap_flag`,
##' `ho_flag`, `xs` (state initial values), `obs.types` (observation types
##' present) and `x` (the input data)
##'
##' @importFrom sf st_crs st_coordinates st_geometry
##' @importFrom dplyr left_join full_join mutate select
##' @importFrom stats approx filter cov na.omit median
##'
##' @keywords internal

ssm_prep <- function(x,
                     time.step = NA,
                     fit.to.subset = TRUE,
                     control = ssm_control(),
                     ho_lookup = NULL) {

  if (!inherits(x, c("sf", "tbl_df")))
    stop("x must be an sf-tibble produced by `prefilter()`", call. = FALSE)
  if (length(unique(x$id)) > 1)
    stop("ssm_prep() expects a single individual", call. = FALSE)

  ## drop records flagged to be ignored, if fit.to.subset is TRUE
  if (fit.to.subset) xx <- subset(x, keep)
  else xx <- x

  prj <- st_crs(xx)
  loc <- as.data.frame(st_coordinates(xx))
  names(loc) <- c("x", "y")
  st_geometry(xx) <- NULL
  d <- cbind(xx, loc)
  d$isd <- TRUE

  ## haulout indicator. ho_lookup is keyed on id + date and is extracted in
  ## fit_ssm() before format_data() and prefilter() drop unknown columns. The
  ## join is by date only, since ssm_prep() sees one individual at a time.
  ## Observation dates absent from ho_lookup fall back to 0.
  if (!is.null(ho_lookup)) {
    id_i <- unique(d$id)
    ho_i <- ho_lookup[ho_lookup$id == id_i, c("date", "ho")]
    d <- left_join(d, ho_i, by = "date")
    d$ho[is.na(d$ho)] <- 0L
  } else {
    d$ho <- 0L
  }

  ## generate prediction times
  if (!inherits(time.step, "data.frame") & all(!is.na(time.step))) {
    tsp <- time.step * 3600
    tms <- (as.numeric(d$date) - as.numeric(d$date[1])) / tsp
    index <- floor(tms)

    if (time.step > 1) {
      ## truncate so predictions start on the hour immediately prior to 1st obs
      ts <- data.frame(id = d$id[1],
                       date = seq(trunc(d$date[1], "hour"),
                                  by = tsp,
                                  length.out = max(index) + 2))
    } else {
      ## truncate so predictions start on the time.step immediately prior to
      ## the 1st obs
      ts1 <- trunc(d$date[1] - tsp, "mins") + tsp
      if (ts1 <= d$date[1]) {
        ts <- data.frame(id = d$id[1],
                         date = seq(ts1, by = tsp, length.out = max(index) + 2))
      } else {
        ts <- data.frame(id = d$id[1],
                         date = seq(ts1 - tsp, by = tsp,
                                    length.out = max(index) + 2))
      }
    }

  } else if (inherits(time.step, "data.frame") & all(!is.na(time.step))) {
    ts <- subset(time.step, id %in% unique(d$id))

  } else if (inherits(time.step, "data.frame") & any(is.na(time.step))) {
    stop("NA's are not allowed in user-supplied prediction times data.frame",
         call. = FALSE)
  }

  if (all(!is.na(time.step))) {
    ## nudge observation times that exactly match prediction times
    if (sum(d$date %in% ts$date) > 0) {
      o.times <- which(d$date %in% ts$date)
      d[o.times, "date"] <- d[o.times, "date"] + 0.5
    }
    d.all <- full_join(d, ts, by = c("id", "date"))
    d.all <- d.all[order(d.all$date), ]
    d.all$isd <- with(d.all, ifelse(is.na(isd), FALSE, isd))
    d.all$id <- with(d.all, ifelse(is.na(id), na.omit(unique(id))[1], id))
  } else {
    d.all <- d
  }

  ## interpolate the haulout indicator onto prediction rows. A prediction step
  ## is flagged as haulout only when bracketed by haulout observations on both
  ## sides; steps at a haulout boundary are conservatively left as 0.
  if (any(is.na(d.all$ho))) {
    ho_obs <- ifelse(d.all$isd, as.integer(d.all$ho), NA_integer_)

    ho_fwd <- ho_obs
    for (k in seq_along(ho_fwd))
      if (is.na(ho_fwd[k]) && k > 1L) ho_fwd[k] <- ho_fwd[k - 1L]

    ho_bwd <- ho_obs
    for (k in rev(seq_along(ho_bwd)))
      if (is.na(ho_bwd[k]) && k < length(ho_bwd)) ho_bwd[k] <- ho_bwd[k + 1L]

    d.all$ho <- as.integer(!is.na(ho_fwd) & !is.na(ho_bwd) &
                             ho_fwd == 1L & ho_bwd == 1L)
  }

  ## time differences in hours
  dt <- as.numeric(difftime(d.all$date,
                            c(as.POSIXct(NA), d.all$date[-nrow(d.all)]),
                            units = "hours"))
  dt[1] <- 0.000001   ## exactly 0 causes numerical issues in the CRW

  ## state initial values from linear interpolation + a 5-point moving average
  x.init1 <- approx(x = select(d, date, x), xout = d.all$date, rule = 2)$y
  x.init <- as.numeric(stats::filter(x.init1, rep(1, 5) / 5))
  x.na <- which(is.na(x.init))
  x.init[x.na] <- x.init1[x.na]

  y.init1 <- approx(x = select(d, date, y), xout = d.all$date, rule = 2)$y
  y.init <- as.numeric(stats::filter(y.init1, rep(1, 5) / 5))
  y.na <- which(is.na(y.init))
  y.init[y.na] <- y.init1[y.na]

  xs <- cbind(x.init, y.init)

  state0 <- c(xs[1, 1], xs[1, 2], 0, 0)
  attributes(state0) <- NULL

  ## Initial velocities for the CRW velocity states.
  ##
  ## The position equation is near-deterministic (innovation variance `tiny`),
  ## so mu and v are effectively tied: given an interpolated path, the only
  ## consistent starting velocity is v[t] = (x[t] - x[t-1]) / dt[t]. They
  ## cannot be chosen independently - setting v to zero would leave the whole
  ## displacement in the position innovation, which carries a variance of 1e-5.
  ##
  ## That construction is well behaved except where dt is very small, and it
  ## can be: an observation coinciding with a prediction time is nudged by
  ## 0.5 s to avoid dt = 0, leaving steps of roughly 1.4e-4 h. Dividing a
  ## displacement by such a dt produces an enormous starting velocity, and the
  ## likelihood then divides the velocity innovation by dt a second time, so
  ## the two compound. Flooring dt here bounds the starting velocities. The
  ## displacement across such a short step is negligible, so the position
  ## innovation stays near zero regardless, and the starting point remains
  ## self-consistent.
  dt.v <- pmax(dt, stats::median(dt[dt > 0], na.rm = TRUE) / 10)
  v.init <- cbind(c(0, diff(x.init)), c(0, diff(y.init))) / dt.v

  ## observation types present, used to decide which measurement parameters
  ## can be estimated
  d <- mutate(d, obs.type = factor(obs.type,
                                   levels = c("LS", "KF", "GL", "GPS"),
                                   labels = c("LS", "KF", "GL", "GPS")))
  p.obst <- table(d$obs.type) / nrow(d)
  obs.types <- round(which(table(d$obs.type) * p.obst > 0))

  ## observation model index. Prediction rows are set to 0 (LS) so that NA's
  ## do not propagate; isd zeroes their likelihood contribution regardless.
  obs_mod <- ifelse(d.all$obs.type %in% c("LS", "GPS"), 0,
                    ifelse(d.all$obs.type == "KF", 1, 2))
  obs_mod <- ifelse(is.na(obs_mod), 0, obs_mod)

  ## gap_flag. Derived from *observation* gaps rather than from dt: when a
  ## regular time.step is supplied the prediction grid fills gaps with regular
  ## steps, so no row of d.all carries a large dt during a data gap and a
  ## dt-based rule would never flag anything. Instead, find consecutive
  ## observation pairs separated by more than gap.thresh and flag every row
  ## falling strictly within those intervals.
  gap_flag <- integer(nrow(d.all))

  if (is.finite(control$gap.thresh)) {
    obs_dates <- d.all$date[d.all$isd]

    if (length(obs_dates) >= 2) {
      obs_gaps <- as.numeric(difftime(obs_dates[-1],
                                      obs_dates[-length(obs_dates)],
                                      units = "hours"))
      large_gaps <- which(obs_gaps > control$gap.thresh)

      for (g in large_gaps) {
        t_before <- obs_dates[g]
        t_after <- obs_dates[g + 1L]
        in_gap <- d.all$date > t_before & d.all$date < t_after
        gap_flag[in_gap] <- 1L
      }
    }
  }

  ## ho_flag. gap_flag takes precedence: a haulout long enough to exceed
  ## gap.thresh is treated as a data gap, so ho_flag is cleared wherever
  ## gap_flag is set. The likelihood therefore never sees both flags at once.
  ho_flag <- as.integer(d.all$ho)
  ho_flag[gap_flag == 1L] <- 0L

  list(
    d.all = d.all,
    prj = prj,
    dt = dt,
    Y = rbind(d.all$x, d.all$y),
    isd = as.integer(d.all$isd),
    obs_mod = as.integer(obs_mod),
    K = cbind(d.all$emf.x, d.all$emf.y),
    m = d.all$smin,
    M = d.all$smaj,
    c = d.all$eor,
    GLerr = cbind(d.all$x.sd, d.all$y.sd),
    state0 = state0,
    gap_flag = gap_flag,
    ho_flag = ho_flag,
    xs = xs,
    v.init = v.init,
    obs.types = obs.types,
    x = x
  )
}
