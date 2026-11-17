## Plot methods for the behavioural models fitted by multiple imputation:
##  fit_mpm(reps = ) (class mpm_mi), fit_mrm() (mrm_mi) and fit_hmm() (hmm_mi).
##  They share the arguments of plot.mpm_df() and the helpers below.

##' @title Plot move persistence estimates from [fit_mpm()] with multiple
##' imputation
##'
##' @description Plots of move persistence (g) from `fit_mpm(reps = )`:
##' 1. `type = "timeseries"`: g against time for each track, with the pooled
##' 95% interval;
##' 2. `type = "track"`: tracks (point-estimate locations) with locations
##' coloured by g.
##'
##' @param x an `mpm_mi` object from `fit_mpm(reps = )`
##' @param type `"timeseries"` (default) or `"track"`
##' @param pages for `type = "timeseries"`, one plot per track on separate
##' pages (`pages = 0`, default) or all plots on one page (`pages = 1`); for
##' `type = "track"`, one map per track (`pages = 0`) or all tracks on one map
##' (`pages = 1`)
##' @param ncol number of columns when `pages = 1` (time series)
##' @param ask logical; if TRUE (default) and `pages = 0`, the user is asked
##' before each plot is rendered. Set to FALSE to return the ggplot objects
##' @param pal [grDevices::hcl.colors] palette (default "Plasma"; see
##' [grDevices::hcl.pals])
##' @param rev logical; reverse the colour palette
##' @param land logical; add land polygons to track plots (default TRUE;
##' requires the `rnaturalearth` data)
##' @param ... unused
##'
##' @return a list of ggplot objects (`pages = 0`, `ask = FALSE`), a patchwork
##' or ggplot object (`pages = 1`), or plots rendered one at a time
##' (`pages = 0`, `ask = TRUE`)
##'
##' @examples
##' \dontrun{
##' xs <- fit_ssm(sese2, model = "rw", time.step = 72, control = ssm_control(verbose = 0))
##' m <- fit_mpm(xs, model = "jmpm", reps = 20)
##' plot(m, pages = 1)
##' plot(m, type = "track", pages = 1)
##' }
##'
##' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point geom_path
##' @importFrom ggplot2 scale_colour_gradientn labs xlab ylab ylim theme_minimal
##' @importFrom grDevices hcl.colors hcl.pals
##' @method plot mpm_mi
##' @export
##' @md
plot.mpm_mi <- function(x,
                        type = c("timeseries", "track"),
                        pages = 0,
                        ncol = 1,
                        ask = TRUE,
                        pal = "Plasma",
                        rev = FALSE,
                        land = TRUE,
                        ...) {
  type <- match.arg(type)
  behav_check_pal(pal)
  d <- as.data.frame(x$fitted)
  scale <- scale_colour_gradientn(colours = hcl.colors(100, pal, rev = rev),
                                  limits = c(0, 1), name = "g")
  if (type == "timeseries") {
    p <- behav_timeseries(d, "g", scale, ylab = "move persistence (g)", limits = c(0, 1))
    behav_output(p, pages, ncol, ask)
  } else {
    behav_tracks(d, "g", scale, pages, ask, land)
  }
}

##' @title Plot movement rate estimates from [fit_mrm()]
##'
##' @description Plots of the movement rate from [fit_mrm()]:
##' 1. `type = "timeseries"`: movement rate against time for each track, with
##' the pooled 95% interval;
##' 2. `type = "track"`: tracks (point-estimate locations) with locations
##' coloured by movement rate.
##'
##' @inheritParams plot.mpm_mi
##' @param x an `mrm_mi` object from [fit_mrm()]
##' @param log logical; plot the movement rate on a log scale (default TRUE),
##' on which its intervals are symmetric
##'
##' @return as for [plot.mpm_mi()]
##'
##' @examples
##' \dontrun{
##' fit <- fit_ssm(ellie, model = "crw", time.step = 12)
##' m <- fit_mrm(fit, reps = 20)
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
  ## one colour scale for all tracks, so a colour means the same rate in every
  ##  panel and the legends combine
  scale <- scale_colour_gradientn(colours = hcl.colors(100, pal, rev = rev),
                                  trans = if (log) "log10" else "identity",
                                  limits = range(d$rate, na.rm = TRUE),
                                  name = expression(paste("rate (km ", h^{-1/2}, ")")))
  if (type == "timeseries") {
    p <- behav_timeseries(d, "rate", scale,
                          ylab = expression(paste("movement rate (km ", h^{-1/2}, ")")),
                          log = log)
    behav_output(p, pages, ncol, ask)
  } else {
    behav_tracks(d, "rate", scale, pages, ask, land)
  }
}

##' @title Plot hidden Markov model results from [fit_hmm()]
##'
##' @description Plots of a hidden Markov model fitted by [fit_hmm()]:
##' 1. `type = "densities"`: histograms of the step lengths and turning angles
##' of the point-estimate tracks, with the fitted state-dependent densities
##' (pooled across imputations) weighted by the proportion of time in each
##' state, and their sum;
##' 2. `type = "track"`: tracks (point-estimate locations) with locations
##' coloured by their most probable state.
##'
##' @inheritParams plot.mpm_mi
##' @param x an `hmm_mi` object from [fit_hmm()]
##' @param type `"densities"` (default) or `"track"`
##' @param pages for `type = "track"`, one map per track (`pages = 0`,
##' default) or all tracks on one map (`pages = 1`). Ignored for
##' `type = "densities"`, which is one plot of all tracks
##' @param bins number of histogram bins (default 40)
##'
##' @return for `type = "densities"`, a patchwork of the step-length and
##' turning-angle panels; for `type = "track"`, as for [plot.mpm_mi()]
##'
##' @details States are numbered by mean step length (state 1 has the
##' shortest steps). The state proportions used to weight the densities are
##' the mean state probabilities over all locations.
##'
##' @examples
##' \dontrun{
##' fit <- fit_ssm(ellie, model = "crw", time.step = 12)
##' h <- fit_hmm(fit, reps = 20)
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
  p_step <- panel(step, sd_step, "step length (km)", c(0, max(grid)))
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
##  one map (pages = 1)
behav_tracks <- function(d, value, scale, pages, ask, land) {
  map_one <- function(s, title) {
    p <- ggplot()
    if (land) {
      wm <- behav_land()
      if (!is.null(wm)) p <- p + ggplot2::geom_sf(data = wm, fill = "grey85", colour = NA)
    }
    pad <- 0.05 * max(diff(range(s$lon)), diff(range(s$lat)), 0.1)
    p + geom_path(aes(lon, lat, group = interaction(id, segment)), data = s,
                  colour = "grey40", linewidth = 0.25) +
      geom_point(aes(lon, lat, colour = .data[[value]]), data = s, size = 1) +
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

## return or render a list of plots, as plot.mpm_df() does
behav_output <- function(p, pages, ncol, ask) {
  if (!pages) {
    if (ask) {
      grDevices::devAskNewPage(ask = TRUE)
      print(p)
      grDevices::devAskNewPage(ask = FALSE)
      invisible(p)
    } else {
      p
    }
  } else {
    wrap_plots(p, ncol = ncol, byrow = TRUE, guides = "collect")
  }
}
