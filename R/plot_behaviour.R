## Plot methods for the behavioural models: fit_mpm() (class mpm_df; also
##  used by plot.ssm_df() for mp fits), and fit_bm(model = "mrm")
##  (mrm_mi) and fit_bm(model = "hmm") (hmm_mi). They share the helpers below.

##' @title Plot move persistence estimates from [fit_mpm()]
##'
##' @description Plots of move persistence (g) from [fit_mpm()], with or
##' without multiple imputation (`reps`):
##' 1. `type = "timeseries"`: g against time for each track, with its 95%
##' interval;
##' 2. `type = "track"`: tracks with locations coloured by g.
##'
##' The same plots are drawn for `mp` model fits from [fit_ssm()] by
##' `plot(x, type = 3)` (time series) and `plot(x, type = 4)` (tracks); see
##' [plot.ssm_df()].
##'
##' @param x an `mpm_df` object from [fit_mpm()], including fits from earlier
##' versions of aniMotum
##' @param y only for `fit_mpm()` output from aniMotum before 1.3-1, which
##' holds no locations: the `ssm_df` fit that `x` was fitted to, required for
##' `type = "track"`. Ignored for current `fit_mpm()` output, which holds its
##' locations
##' @param type `"timeseries"` or `"track"`. If NULL (default), `"track"` when
##' `y` is supplied, otherwise `"timeseries"`. Tracks need longitude and
##' latitude: they are missing when `fit_mpm()` was fitted to a data frame of
##' projected coordinates
##' @param pages for `type = "timeseries"`, one plot per track on separate
##' pages (`pages = 0`, default) or all plots on one page (`pages = 1`); for
##' `type = "track"`, one map per track (`pages = 0`) or all tracks on one map
##' (`pages = 1`)
##' @param ncol number of columns when `pages = 1` (time series)
##' @param ask logical; if TRUE (default) and `pages = 0`, the user is asked
##' before each plot is rendered (not for a single track, which is drawn
##' directly). Set to FALSE to return the ggplot objects
##' @param pal [grDevices::hcl.colors] palette (default "Plasma"; see
##' [grDevices::hcl.pals])
##' @param rev logical; reverse the colour palette
##' @param land logical; add land polygons to track plots (default TRUE;
##' requires the `rnaturalearth` data)
##' @param se logical; for `type = "track"`, scale point size by the inverse
##' squared standard error of logit(g) (default FALSE; indicative only)
##' @param normalise logical; rescale g to the range 0 to 1 (default FALSE).
##' The interval is rescaled with g and truncated to 0 to 1
##' @param group logical; with `normalise = TRUE`, rescale g over all tracks
##' together (TRUE) or over each track separately (FALSE, default)
##' @param ... unused
##'
##' @return a list of ggplot objects (`pages = 0`, `ask = FALSE`), a patchwork
##' or ggplot object (`pages = 1`), or plots rendered one at a time
##' (`pages = 0`, `ask = TRUE`)
##'
##' @details The 95% interval is the pooled (Rubin's rules) interval for an
##' fit by multiple imputation (`fit_mpm(reps = )`), and
##' `plogis(logit_g -/+ 1.96 * logit_g.se)` otherwise.
##'
##' @examples
##' ## move persistence model fitted to ssm-estimated locations
##' xs <- fit_ssm(sese2, spdf = FALSE, model = "rw", time.step = 72,
##'               control = ssm_control(verbose = 0))
##' xm <- fit_mpm(xs, model = "jmpm")
##' plot(xm, pages = 1)
##' plot(xm, xs, type = "track", pages = 1)
##'
##' \dontrun{
##' ## with multiple imputation of the locations
##' m <- fit_mpm(xs, model = "jmpm", reps = 20)
##' plot(m, pages = 1)
##' plot(m, type = "track", pages = 1)
##' }
##'
##' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point geom_path
##' @importFrom ggplot2 scale_colour_gradientn labs xlab ylab ylim theme_minimal
##' @importFrom grDevices hcl.colors hcl.pals
##' @importFrom stats plogis qlogis qnorm
##' @method plot mpm_df
##' @export
##' @md
plot.mpm_df <- function(x,
                        y = NULL,
                        type = NULL,
                        pages = 0,
                        ncol = 1,
                        ask = TRUE,
                        pal = "Plasma",
                        rev = FALSE,
                        land = TRUE,
                        se = FALSE,
                        normalise = FALSE,
                        group = FALSE,
                        ...) {
  if (length(list(...)) > 0) warning("additional arguments ignored", call. = FALSE)
  behav_check_pal(pal)
  ## default type: track plots when the ssm fit is supplied (as in earlier
  ##  versions of plot() for fit_mpm() output)
  type <- if (is.null(type)) {
    if (is.null(y)) "timeseries" else "track"
  } else {
    match.arg(type, c("timeseries", "track"))
  }
  current <- mpm_current(x)
  if (current && !is.null(y)) {
    warning("`y` ignored: fit_mpm() output holds its own locations", call. = FALSE)
    y <- NULL
  }
  if (type == "track" && !current && is.null(y))
    stop("type = \"track\" needs the ssm fit for the locations: plot(x, y = <ssm fit>, type = \"track\")",
         call. = FALSE)
  mpm_plot(mpm_plot_data(x, y), type, pages, ncol, ask, pal, rev, land,
           se = se, normalise = normalise, group = group)
}

## TRUE for fit_mpm() output from aniMotum 1.3-1 on (a list holding the
##  fitted tibble with locations); FALSE for earlier output (a tibble of fits)
mpm_current <- function(x) inherits(x, "mpm_df") && !is.data.frame(x)

## move persistence estimates in one layout for plotting: id, date, g, its
##  standard error (g.se, or logit_g.se for fits before 1.3-1), and lon, lat
##  when the locations are available
mpm_plot_data <- function(x, y = NULL) {
  if (mpm_current(x)) return(as.data.frame(x$fitted))
  if (!inherits(x, c("mpm_df", "fG_mpm")))
    stop("x must be a fit_mpm() object", call. = FALSE)
  if (is.null(y)) {
    d <- as.data.frame(grab(x, "fitted"))
    ## earlier layout: g and g.se, with g.se on the logit scale
    if (!"logit_g" %in% names(d)) {
      d$logit_g <- qlogis(pmin(pmax(d$g, 0.001), 0.999))
      d$logit_g.se <- d$g.se
    }
    return(d)
  }
  if (!inherits(y, "ssm_df"))
    stop("y must be the ssm fit (class `ssm_df`) that x was fitted to", call. = FALSE)
  ## x may have been fitted to the predicted or the fitted locations
  nx <- nrow(grab(x, "fitted"))
  np <- tryCatch(nrow(grab(y, "predicted")), error = function(e) NA)
  what.ssm <- if (isTRUE(np == nx)) {
    "predicted"
  } else if (nrow(grab(y, "fitted")) == nx) {
    "fitted"
  } else {
    stop("x and y have unequal numbers of estimated values", call. = FALSE)
  }
  as.data.frame(join(y, x, what.ssm = what.ssm, as_sf = FALSE))
}

## draw the move persistence plots from data in the mpm_plot_data() layout;
##  shared by plot.mpm_df() and plot.ssm_df() (types 3 and 4)
mpm_plot <- function(d, type, pages, ncol, ask, pal, rev, land,
                     se = FALSE, normalise = FALSE, group = FALSE) {
  d <- as.data.frame(d)
  ## 95% interval, unless already given (fit_mpm() output): from the
  ##  standard error of g, or of logit(g) for fits made before 1.3-1
  if (!all(c("lower", "upper") %in% names(d))) {
    if ("logit_g.se" %in% names(d)) {
      d$lower <- plogis(d$logit_g - qnorm(0.975) * d$logit_g.se)
      d$upper <- plogis(d$logit_g + qnorm(0.975) * d$logit_g.se)
    } else {
      d$lower <- d$g - qnorm(0.975) * d$g.se
      d$upper <- d$g + qnorm(0.975) * d$g.se
    }
  }
  ## g can stray slightly outside 0 to 1 (its bounds are soft): show it, and
  ##  its interval, within 0 to 1
  for (v in c("g", "lower", "upper")) d[[v]] <- pmin(pmax(d[[v]], 0), 1)
  if (normalise) d <- mpm_normalise(d, group)
  label <- if (normalise) "move persistence (g), normalised" else "move persistence (g)"
  scale <- scale_colour_gradientn(colours = hcl.colors(100, pal, rev = rev),
                                  limits = c(0, 1), breaks = seq(0, 1, 0.25),
                                  name = "g", na.value = "grey50")
  if (type == "timeseries") {
    p <- behav_timeseries(d, "g", scale, ylab = label, limits = c(0, 1))
    behav_output(p, pages, ncol, ask)
  } else {
    if (all(is.na(d$lon)))
      stop("no longitude and latitude to map: the move persistence model was fitted to projected coordinates",
           call. = FALSE)
    ## point size proportional to SE^-2 (indicative only)
    if (se) d$se_weight <- (if ("g.se" %in% names(d)) d$g.se else d$logit_g.se)^-2
    behav_tracks(d, "g", scale, pages, ask, land,
                 size = if (se) "se_weight" else NULL)
  }
}

## rescale g, and its interval, to 0 to 1 over each track (group = FALSE) or
##  over all tracks (group = TRUE)
mpm_normalise <- function(d, group) {
  rescale <- function(s) {
    lo <- min(s$g, na.rm = TRUE)
    hi <- max(s$g, na.rm = TRUE)
    for (v in c("g", "lower", "upper")) s[[v]] <- pmin(pmax((s[[v]] - lo) / (hi - lo), 0), 1)
    s
  }
  if (group) rescale(d) else do.call(rbind, lapply(split(d, d$id), rescale))
}

##' @title Plot movement rate estimates from [fit_bm()]
##'
##' @description Plots of the movement rate from `fit_bm(model = "mrm")`, shown as a
##' speed (km/h) at the fit's time step: the root-mean-square net
##' displacement over one time step, divided by the time step (see
##' [fit_bm()]):
##' 1. `type = "timeseries"`: speed against time for each track, with the
##' pooled 95% interval;
##' 2. `type = "track"`: tracks (point-estimate locations) with locations
##' coloured by speed.
##'
##' @inheritParams plot.mpm_df
##' @param x an `mrm_mi` object from `fit_bm(model = "mrm")`
##' @param type `"timeseries"` (default) or `"track"`
##' @param log logical; plot the speed on a log scale (default TRUE), on which
##' its intervals are symmetric
##'
##' @return as for [plot.mpm_df()]
##'
##' @examples
##' \dontrun{
##' fit <- fit_ssm(ellie, model = "crw", time.step = 12)
##' m <- fit_bm(fit, model = "mrm", reps = 20)
##' plot(m)
##' plot(m, type = "track")
##' }
##'
##' @importFrom ggplot2 scale_y_log10
##' @method plot mrm_mi
##' @export
##' @md
plot.mrm_mi <- function(x,
                        type = c("timeseries", "track"),
                        pages = 0,
                        ncol = 1,
                        ask = TRUE,
                        pal = "Plasma",
                        rev = FALSE,
                        land = TRUE,
                        log = TRUE,
                        ...) {
  type <- match.arg(type)
  behav_check_pal(pal)
  d <- as.data.frame(x$fitted)
  ## plot the speed (km/h) at the fit's time step; fits made before the speed
  ##  columns existed get them from the rate
  if (!"speed" %in% names(d)) {
    to_speed <- mrm_speed_factor(x$time.step)
    d$speed <- d$rate * to_speed
    d$speed_lower <- d$lower * to_speed
    d$speed_upper <- d$upper * to_speed
  }
  d$lower <- d$speed_lower
  d$upper <- d$speed_upper
  label <- paste0("speed (km/h)\nnet displacement over ", format(x$time.step), " h")
  ## one colour scale for all tracks, so a colour means the same speed in
  ##  every panel and the legends combine
  scale <- scale_colour_gradientn(colours = hcl.colors(100, pal, rev = rev),
                                  trans = if (log) "log10" else "identity",
                                  limits = range(d$speed, na.rm = TRUE),
                                  name = label)
  if (type == "timeseries") {
    p <- behav_timeseries(d, "speed", scale, ylab = label, log = log)
    behav_output(p, pages, ncol, ask)
  } else {
    behav_tracks(d, "speed", scale, pages, ask, land)
  }
}

##' @title Plot hidden Markov model results from [fit_bm()]
##'
##' @description Plots of a hidden Markov model fitted by `fit_bm(model = "hmm")`:
##' 1. `type = "densities"`: histograms of the step lengths and turning angles
##' of the point-estimate tracks, with the fitted state-dependent densities
##' (pooled across imputations) weighted by the proportion of time in each
##' state, and their sum;
##' 2. `type = "track"`: tracks (point-estimate locations) with locations
##' coloured by their most probable state.
##'
##' @inheritParams plot.mpm_df
##' @param x an `hmm_mi` object from `fit_bm(model = "hmm")`
##' @param type `"densities"` (default) or `"track"`
##' @param pages for `type = "track"`, one map per track (`pages = 0`,
##' default) or all tracks on one map (`pages = 1`). Ignored for
##' `type = "densities"`, which is one plot of all tracks
##' @param bins number of histogram bins (default 40)
##'
##' @return for `type = "densities"`, a patchwork of the step-length and
##' turning-angle panels; for `type = "track"`, as for [plot.mpm_df()]
##'
##' @details States are numbered by mean step length (state 1 has the
##' shortest steps). The state proportions used to weight the densities are
##' the mean state probabilities over all locations.
##'
##' @examples
##' \dontrun{
##' fit <- fit_ssm(ellie, model = "crw", time.step = 12)
##' h <- fit_bm(fit, model = "hmm", reps = 20)
##' plot(h)
##' plot(h, type = "track")
##' }
##'
##' @importFrom ggplot2 geom_histogram after_stat scale_colour_manual
##' @importFrom patchwork wrap_plots plot_annotation
##' @method plot hmm_mi
##' @export
##' @md
plot.hmm_mi <- function(x,
                        type = c("densities", "track"),
                        pages = 0,
                        ask = TRUE,
                        pal = "Plasma",
                        rev = FALSE,
                        land = TRUE,
                        bins = 40,
                        ...) {
  type <- match.arg(type)
  behav_check_pal(pal)
  n <- x$n_states
  ## discrete state colours, avoiding the palette's pale end
  cols <- hcl.colors(n + 1, pal, rev = rev)[seq_len(n)]
  names(cols) <- paste("state", seq_len(n))
  d <- as.data.frame(x$states)

  if (type == "track") {
    d$state_label <- factor(paste("state", d$state), levels = names(cols))
    scale <- scale_colour_manual(values = cols, name = NULL, drop = FALSE)
    return(behav_tracks(d, "state_label", scale, pages, ask, land))
  }

  if (is.null(x$par_summary))
    stop("no pooled parameters to plot: every imputation had non-finite standard errors",
         call. = FALSE)
  est <- stats::setNames(x$par_summary$estimate, x$par_summary$parameter)
  weight <- colMeans(as.matrix(d[, paste0("p", seq_len(n)), drop = FALSE]))

  ## step lengths: gamma, parameterised by mean and SD
  step <- d$step[is.finite(d$step) & d$step > 0]
  grid <- seq(0, stats::quantile(step, 0.995), length.out = 300)
  sd_step <- do.call(rbind, lapply(seq_len(n), function(k) {
    m <- est[paste0("step_mean_", k)]
    s <- est[paste0("step_sd_", k)]
    data.frame(x = grid, density = weight[k] * stats::dgamma(grid, shape = m^2 / s^2, rate = m / s^2),
               state = names(cols)[k])
  }))
  ## turning angles: von Mises
  angle <- d$angle[is.finite(d$angle)]
  agrid <- seq(-pi, pi, length.out = 300)
  sd_angle <- do.call(rbind, lapply(seq_len(n), function(k) {
    mu <- est[paste0("angle_mu_", k)]
    kappa <- est[paste0("angle_kappa_", k)]
    data.frame(x = agrid,
               density = weight[k] * exp(kappa * cos(agrid - mu)) / (2 * pi * besselI(kappa, 0)),
               state = names(cols)[k])
  }))

  panel <- function(obs, dens, xlab, xlim) {
    total <- stats::aggregate(density ~ x, data = dens, FUN = sum)
    total$state <- "total"
    ggplot() +
      geom_histogram(aes(x = obs, y = after_stat(density)), data = data.frame(obs = obs),
                     bins = bins, boundary = xlim[1], fill = "grey85", colour = "white") +
      geom_line(aes(x, density, colour = state), data = dens, linewidth = 0.8) +
      geom_line(aes(x, density), data = total, linetype = 2, linewidth = 0.6) +
      scale_colour_manual(values = cols, name = NULL) +
      labs(x = xlab, y = "density") +
      theme_minimal()
  }
  p_step <- panel(step, sd_step, paste0("step length (km per ", format(x$time.step), " h)"),
                  c(0, max(grid)))
  p_angle <- panel(angle, sd_angle, "turning angle (radians)", c(-pi, pi))
  wrap_plots(p_step, p_angle, ncol = 2, guides = "collect") +
    plot_annotation(caption = "dashed line: sum of the state densities, each weighted by the proportion of time in that state")
}

## ---- shared helpers ---------------------------------------------------------

behav_check_pal <- function(pal) {
  if (!pal %in% hcl.pals())
    stop("palette is not an hcl.palette, type 'hcl.pals()' to see the list of palettes",
         call. = FALSE)
}

## time series of an estimate with its pooled 95% interval, one plot per id;
##  lines are broken between track segments
behav_timeseries <- function(d, value, scale, ylab, limits = NULL, log = FALSE) {
  if (!"segment" %in% names(d)) d$segment <- 1L
  lapply(split(d, d$id), function(s) {
    p <- ggplot(s, aes(date, .data[[value]], group = segment)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey50", alpha = 0.25) +
      geom_line(colour = "grey40", linewidth = 0.3) +
      geom_point(aes(colour = .data[[value]]), size = 1.25) +
      scale +
      labs(title = paste("id:", s$id[1]), x = NULL, y = ylab) +
      theme_minimal()
    if (!is.null(limits)) p <- p + ylim(limits[1], limits[2])
    if (log) p <- p + scale_y_log10()
    p
  })
}

## tracks with locations coloured by value: one map per id, or all ids on
##  one map (pages = 1); size optionally names a column for point size
behav_tracks <- function(d, value, scale, pages, ask, land, size = NULL) {
  map_one <- function(s, title) {
    p <- ggplot()
    if (land) {
      wm <- behav_land()
      if (!is.null(wm)) p <- p + ggplot2::geom_sf(data = wm, fill = "grey85", colour = NA)
    }
    pad <- 0.05 * max(diff(range(s$lon)), diff(range(s$lat)), 0.1)
    p + geom_path(aes(lon, lat, group = interaction(id, segment)), data = s,
                  colour = "grey40", linewidth = 0.25) +
      (if (is.null(size)) {
        geom_point(aes(lon, lat, colour = .data[[value]]), data = s, size = 1)
      } else {
        list(geom_point(aes(lon, lat, colour = .data[[value]], size = .data[[size]]), data = s),
             ggplot2::scale_size(range = c(0.1, 2.5), guide = "none"))
      }) +
      scale +
      ggplot2::coord_sf(xlim = range(s$lon) + c(-pad, pad), ylim = range(s$lat) + c(-pad, pad),
                        crs = 4326, default_crs = 4326, expand = FALSE) +
      labs(title = title, x = NULL, y = NULL) +
      theme_minimal()
  }
  if (!"segment" %in% names(d)) d$segment <- 1L
  if (pages) return(map_one(d, NULL))
  p <- lapply(split(d, d$id), function(s) map_one(s, paste("id:", s$id[1])))
  behav_output(p, pages = 0, ncol = 1, ask = ask)
}

## land polygons (longitude and latitude), or NULL if unavailable
behav_land <- function() {
  wm <- try(suppressMessages(suppressWarnings(
    rnaturalearth::ne_countries(scale = if (requireNamespace("rnaturalearthhires", quietly = TRUE)) 10 else 50,
                                returnclass = "sf"))), silent = TRUE)
  if (inherits(wm, "try-error")) NULL else wm
}

## return or render a list of plots; a single plot
##  (one track) is drawn without asking for input
behav_output <- function(p, pages, ncol, ask) {
  if (!pages) {
    if (ask && length(p) > 1) {
      grDevices::devAskNewPage(ask = TRUE)
      print(p)
      grDevices::devAskNewPage(ask = FALSE)
      invisible(p)
    } else if (ask) {
      print(p[[1]])
      invisible(p)
    } else {
      p
    }
  } else {
    wrap_plots(p, ncol = ncol, byrow = TRUE, guides = "collect")
  }
}
