## Multiple-imputation fits of the move persistence model, called by
##  fit_mpm(reps = ). Tracks are imputed with impute_tracks(), the mpm or jmpm
##  is fitted to each imputation, and logit(g) at each time and the log
##  parameters are pooled with Rubin's rules.
fit_mpm_mi <- function(x, model, reps, time.step, max_gap, kineis,
                       control, inner.control) {

  imp <- impute_tracks(x, reps = reps, time.step = time.step, max_gap = max_gap,
                       kineis = kineis)
  source <- attr(imp, "source")
  time.step <- attr(imp, "time.step")
  imp <- as.data.frame(imp)
  estimate <- imp[imp$rep == 0, ]

  ## rescale x and y for the optimiser by one common constant, as fit_mpm()
  ##  does, so sigma_x and sigma_y are in the same units, and by the same
  ##  constant in every imputation, so the sigma estimates can be pooled
  scale <- max(abs(c(estimate$x, estimate$y)), na.rm = TRUE)
  imp$x <- imp$x / scale
  imp$y <- imp$y / scale

  ## track units: each segment of each individual (a segment needs >= 3
  ##  locations for the move persistence model)
  imp$track <- paste(imp$id, imp$segment, sep = "_segment_")
  n.track <- table(imp$track[imp$rep == 0])
  short <- names(n.track)[n.track < 3]
  if (length(short) > 0) {
    message(length(short), " track segment(s) with fewer than 3 locations dropped")
    imp <- imp[!imp$track %in% short, ]
    estimate <- estimate[!paste(estimate$id, estimate$segment, sep = "_segment_") %in% short, ]
  }

  fit_one <- function(d) {
    ## mpmf() prints optimiser errors from its own try(); silence them, the
    ##  failed fit is caught and counted below
    old <- options(try.outFile = nullfile())
    on.exit(options(old))
    f <- try(suppressWarnings({
      utils::capture.output(r <- mpmf(d, model = model, control = control,
                                      inner.control = inner.control))
      r
    }), silent = TRUE)
    if (inherits(f, "try-error") || !inherits(f$opt, "list") || f$opt$convergence != 0 ||
        inherits(f$rep, "try-error") || !isTRUE(f$rep$pdHess)) return(NULL)
    fx <- summary(f$rep, "fixed")
    list(fitted = as.data.frame(f$fitted), par = fx[, 1:2, drop = FALSE])
  }

  ## one fit per imputation (jmpm) or per imputation and track segment (mpm)
  units <- if (model == "jmpm") "all" else unique(imp$track)
  ## progress bar over the imputation fits; the per-iteration parameter trace
  ##  of mpmf() is switched off, as it would overwrite the bar
  pb <- progress_bar(length(units) * reps, control$verbose >= 1)
  if (control$verbose == 1) control$verbose <- 0
  fits <- lapply(units, function(u) {
    lapply(seq_len(reps), function(k) {
      d <- imp[imp$rep == k & (u == "all" | imp$track == u), ]
      d <- data.frame(id = d$id, tid = if (model == "jmpm") d$segment else NA,
                      date = d$date, x = d$x, y = d$y)
      f <- fit_one(d)
      pb$tick()
      f
    })
  })
  pb$close()
  names(fits) <- units
  n_failed <- sum(vapply(unlist(fits, recursive = FALSE), is.null, logical(1)))

  fitted <- list()
  par <- list()
  for (u in units) {
    f <- Filter(Negate(is.null), fits[[u]])
    if (length(f) == 0) {
      warning("the move persistence model failed for every imputation of ",
              if (u == "all") "the joint fit" else u, call. = FALSE)
      next
    }
    e <- if (u == "all") estimate else
      estimate[paste(estimate$id, estimate$segment, sep = "_segment_") == u, ]
    e <- e[order(e$id, e$segment, e$date), ]
    ## mpmf() orders rows by id, tid, date: align each imputation to e
    key <- function(z) paste(z$id, z$date)
    lg <- do.call(cbind, lapply(f, function(z) z$fitted$logit_g[match(key(e), key(z$fitted))]))
    lgse <- do.call(cbind, lapply(f, function(z) z$fitted$logit_g.se[match(key(e), key(z$fitted))]))
    pr <- pool_rubin(lg, lgse)
    fitted[[u]] <- tibble(id = e$id, segment = e$segment, date = e$date,
                          lon = e$lon, lat = e$lat,
                          logit_g = pr$estimate, logit_g.se = pr$se,
                          g = stats::plogis(pr$estimate),
                          lower = stats::plogis(pr$lower), upper = stats::plogis(pr$upper),
                          fraction_missing = pr$fraction_missing)
    pp <- pool_rubin(do.call(cbind, lapply(f, function(z) z$par[, 1])),
                     do.call(cbind, lapply(f, function(z) z$par[, 2])))
    nm <- rownames(f[[1]]$par)
    nm[nm == "log_sigma"] <- c("log_sigma_x", "log_sigma_y")
    par[[u]] <- tibble(unit = if (u == "all") "joint" else u,
                       parameter = sub("^log_", "", nm),
                       estimate = exp(pp$estimate),
                       lower = exp(pp$lower), upper = exp(pp$upper),
                       imputations = length(f))
  }
  if (length(fitted) == 0) stop("the move persistence model failed for every track", call. = FALSE)

  out <- list(fitted = bind_rows(fitted),
              par = bind_rows(par),
              model = model,
              reps = reps,
              n_failed = n_failed,
              source = source,
              time.step = time.step)
  class(out) <- append("mpm_mi", class(out))
  out
}
