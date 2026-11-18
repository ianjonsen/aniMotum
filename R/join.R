##' @title Join behavioural estimates to locations or to tag-measured events
##'
##' @description `join()` does one of two things, depending on its inputs:
##' 1. `join(ssm, mpm)`: joins ssm-predicted locations and the mpm-estimated
##' move persistence into a single tibble. If the ssm-predicted tibble is a
##' projected sf object then the output of join will also be an sf object
##' (default). This can be avoided by using `as_sf = FALSE`;
##' 2. `join(fit, events)`: interpolates the estimates of a behavioural model
##' ([fit_bm()]) to the times of tag-measured events, such as
##' dives, CTD profiles or haulouts, so the events can be analysed against
##' behaviour. With `by_rep = TRUE`, the estimates of each imputation are
##' interpolated, one row per event and imputation, so a second-stage
##' analysis can be fitted to each imputation and pooled with Rubin's rules.
##'
##' @param x an `aniMotum` ssm fit (`ssm_df`), or a behavioural model fit from
##' [fit_bm()] (`mpm_df`, `mrm_mi`, `hmm_mi`)
##' @param y with an ssm fit `x`: an `aniMotum` mpm fit (`mpm_df`). With a
##' behavioural model fit `x`: a data frame of events with columns `id` and
##' `date` (POSIXct), e.g. dives (`date` = dive end), CTD profiles or
##' haulouts (`date` = start). Other columns are kept.
##' @param what.ssm specifies whether ssm `predicted` or `fitted` values are to be extracted
##' @param as_sf logical; if FALSE then return a tibble with un-projected lonlat
##' coordinates, otherwise return an sf tibble
##' @param normalise logical; if output includes a move persistence estimate, 
##' should g (the move persistence index) be normalised to have minimum = 0 and 
##' maximum = 1 (default = FALSE).
##' @param group logical; should g be normalised among individuals as a group, 
##' a 'relative g', or separately to highlight regions of lowest and highest move
##' persistence along a track (default = FALSE).
##' @param by_rep logical; for events, interpolate the estimates of each
##' imputation (`x$imputed`, from [fit_bm()] with `reps`) rather than the
##' pooled estimates (default FALSE).
##' @param ssm,mpm the names of `x` and `y` before aniMotum 1.4-1, still
##' accepted.
##'
##' @return for an ssm and an mpm fit, a single tbl with all individuals. For
##' events, the events tibble with the estimates added, interpolated
##' linearly in time between the model's time steps within a track segment
##' (NA for events outside the segments, e.g. during a haulout or a data
##' gap):
##' 1. move persistence (`mpm_df`): `g`, `lower`, `upper` (pooled) or `g`,
##' `g.se` (`by_rep`);
##' 2. movement rate (`mrm_mi`): `speed`, `speed_lower`, `speed_upper`,
##' `log_rate`, `log_rate.se` (pooled) or `log_rate`, `log_rate.se`, `speed`
##' (`by_rep`);
##' 3. hidden Markov model (`hmm_mi`): the state probabilities `p1`, `p2`,
##' ... and the most probable state `state`, from the interpolated
##' probabilities.
##'
##' With `by_rep = TRUE`, one row per event and imputation, with column `rep`.
##' Existing columns with these names are overwritten.
##'
##' @importFrom tibble as_tibble
##' @importFrom dplyr rename select mutate bind_rows
##' @examples
##' ## load example aniMotum fit objects (to save time)
##' ## generate a ssm fit object
##' xs <- fit_ssm(ellie, spdf=FALSE, model = "rw", time.step=24, control = ssm_control(verbose = 0))
##' xm <- fit_mpm(xs, what = "p", model = "mpm")
##' 
##' ## join predicted values as an un-projected tibble
##' xsm <- join(xs, xm)
##' xsm
##'
##' \dontrun{
##' ## move persistence at the end time of each dive, by imputation
##' g <- fit_bm(xs, model = "mpm", reps = 20)
##' dives <- data.frame(id = ..., date = ..., dive_dur = ...)
##' dg <- join(g, dives, by_rep = TRUE)
##' }
##' @export
##' @md

join <- function(x,
                 y,
                 what.ssm = "predicted",
                 as_sf = FALSE,
                 normalise = FALSE,
                 group = FALSE,
                 by_rep = FALSE,
                 ssm,
                 mpm) {

  ## names before 1.4-1
  if (missing(x) && !missing(ssm)) x <- ssm
  if (missing(y) && !missing(mpm)) y <- mpm
  if (missing(x) || missing(y)) stop("join() needs two inputs, x and y", call. = FALSE)

  ## a behavioural model fit and tag-measured events
  if (!inherits(x, "ssm_df") && !inherits(y, "mpm_df")) return(join_events(x, y, by_rep))

  ssm <- x
  mpm <- y
  if(!inherits(ssm, "ssm_df")) stop("ssm must be an aniMotum ssm fit object with class `ssm_df`")
  if(!inherits(mpm, "mpm_df")) stop("mpm must be an aniMotum mpm fit object with class `mpm_df`")
  
  x <- grab(ssm, what = what.ssm, as_sf = as_sf) 
  y <- grab(mpm, what = "fitted", normalise = normalise, group = group)
  
  if (inherits(mpm, "mpm_df") && !is.data.frame(mpm)) {
    ## fit_mpm() output from 1.3-1 on: match the estimates to the ssm
    ##  locations by id and time
    if (mpm$source != "ssm")
      stop("mpm was fitted to ", if (mpm$source == "kineis") "Kinéis" else "data frame",
           " locations, not to this ssm fit; its locations are in grab(mpm, \"fitted\")",
           call. = FALSE)
    m <- match(paste(x$id, x$date), paste(y$id, y$date))
    if (all(is.na(m)))
      stop("ssm and mpm have no times in common; check what.ssm", call. = FALSE)
    if (any(is.na(m)))
      warning(sum(is.na(m)), " ssm location(s) without a move persistence estimate", call. = FALSE)
    y <- y[m, c("g", "g.se")]
  } else if(all(c("logit_g","logit_g.se") %in% names(y))) {
    ## deal w old `fG_mpm` & earlier `mpm_df` classes
    y <- y[, c("logit_g","logit_g.se","g")]
  } else {
    y <- y[, c("g","g.se")]
    y$logit_g <- qlogis(ifelse(y$g < 0.001, 0.001, 
                               ifelse(y$g > 0.999, 0.999, y$g)))
    names(y) <- c("g", "logit_g.se", "logit_g")
    y <- y[, c("logit_g", "logit_g.se", "g")]
  }
  
  if(nrow(x) != nrow(y)) stop("number of rows in ssm is NOT equal to number of rows in mpm")
  y <- as.data.frame(y)
  
  xy <- cbind(x, y) 
  
  if(!as_sf) {
    xy <- as_tibble(xy)
  } else {
    ## ensures geometry is last column (for cases using older sf)
    nc <- ncol(xy)
    ng <- which(names(xy) == "geometry")
    if(nc != ng) {
      xy <- xy[, c(1:(ng-1), (ng+1):nc, ng)]
    }
  }
  
  class(xy) <- append(class(xy), "ssmmpm", after = 0)
  return(xy)
  
}
## behavioural estimates at the times of tag-measured events: the columns of
##  each model's estimates are interpolated linearly in time within a track
##  segment by bm_data(); with by_rep, those of each imputation
join_events <- function(x, events, by_rep = FALSE) {
  if (!is.data.frame(events) || !all(c("id", "date") %in% names(events)))
    stop("y must be a data frame of events with columns `id` and `date`", call. = FALSE)
  if (!inherits(events$date, "POSIXct"))
    stop("the events' `date` must be POSIXct", call. = FALSE)
  ev <- as.data.frame(events)
  ev$id <- as.character(ev$id)

  if (inherits(x, "mpm_df") && !is.data.frame(x)) {
    est <- x$fitted
    cols <- c("g", "lower", "upper")
    rcols <- c("g", "g.se")
  } else if (inherits(x, "mrm_mi")) {
    est <- x$fitted
    if (!"segment" %in% names(est)) est$segment <- 1L
    cols <- c("speed", "speed_lower", "speed_upper", "log_rate", "log_rate.se")
    rcols <- c("log_rate", "log_rate.se", "speed")
  } else if (inherits(x, "hmm_mi")) {
    est <- x$states
    cols <- rcols <- paste0("p", seq_len(x$n_states))
  } else {
    stop("x must be an ssm fit (with an mpm fit as y), or a behavioural model fit from fit_bm()",
         call. = FALSE)
  }

  if (!by_rep) {
    out <- bm_data(ev, est, cols)
  } else {
    if (is.null(x$imputed))
      stop("by_rep needs the estimates of each imputation: fit with fit_bm(reps = )",
           call. = FALSE)
    imp <- x$imputed
    out <- bind_rows(lapply(sort(unique(imp$rep)), function(k) {
      o <- bm_data(ev, imp[imp$rep == k, ], rcols)
      o$rep <- k
      o
    }))
    out <- out[, c("rep", setdiff(names(out), "rep"))]
  }
  if (inherits(x, "hmm_mi")) {
    p <- as.matrix(as.data.frame(out)[, cols, drop = FALSE])
    out$state <- NA_integer_
    has <- stats::complete.cases(p)
    out$state[has] <- max.col(p[has, , drop = FALSE], ties.method = "first")
  }
  out
}
