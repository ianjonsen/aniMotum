##' @title Fit a time-varying movement rate model, by multiple imputation
##'
##' @description Estimates a time-varying movement rate along each track. The
##' model is fitted to each of `reps` imputed tracks (see [impute_tracks()]),
##' and the estimates are pooled across imputations with Rubin's rules, so the
##' results carry location uncertainty. Tracks are imputed from an SSM fit, or
##' directly from Kinéis locations using their error radii.
##'
##' @param x an `ssm_df` fit object (`rw`, `crw` or `mp`) with regular predicted
##' locations, or a data frame of Kinéis locations (see [impute_tracks()]).
##' @param reps number of imputed tracks (default 20).
##' @param time.step for Kinéis locations, the time step (h) of the regular grid
##' (see [impute_tracks()]).
##' @param max_gap for Kinéis locations, the longest gap (h) interpolated
##' across (see [impute_tracks()]).
##' @param kineis for Kinéis locations, the Kinéis error model (see
##' [impute_tracks()]).
##' @param control a list from [mpm_control()] (optimiser settings). With
##' `verbose = 1` (the default) a progress bar is shown over the imputation
##' fits; `verbose = 0` is silent.
##'
##' @return a list with class `mrm_mi`:
##' 1. `fitted`: a tibble with, for each id and time, the point-estimate track
##' (`lon`, `lat`), the pooled log movement rate (`log_rate`), its total
##' standard error (`log_rate.se`), the movement rate (`rate`, km h^-1/2) and
##' its 95% interval (`lower`, `upper`), the speed (`speed`, km/h; see
##' details) and its 95% interval (`speed_lower`, `speed_upper`), and the
##' fraction of missing information (`fraction_missing`: the share of the
##' variance due to location uncertainty);
##' 2. `par`: pooled estimates of `sigma_rate` (SD of the random walk on the
##' log rate, per h^1/2) and `rho_p` (correlation between east and north
##' displacements), by id;
##' 3. `reps`, `n_failed` (imputed-track fits that failed or did not
##' converge), `source` (`"ssm"` or `"kineis"`) and `time.step`.
##'
##' @details Displacements between consecutive locations follow a Brownian
##' random walk whose rate varies in time:
##' `d(t) ~ N(0, exp(2 * log_rate(t)) * dt * R)`, with `R` the unit-diagonal
##' correlation matrix of east and north displacements (km, on the ground), and
##' `log_rate` a continuous-time random walk with standard deviation
##' `sigma_rate * sqrt(dt)`. `rate` is the standard deviation of displacement
##' along each axis per square root of hour; `rate * sqrt(24)` is the SD of a
##' 24 h displacement. Low rates indicate area-restricted movement, high rates
##' travel. The rate does not separate speed from tortuosity.
##'
##' The rate is not a speed: under a Brownian random walk, displacement grows
##' with the square root of time. `speed` converts it to km/h at the time step
##' of the fit, `time.step` (h): the root-mean-square net displacement over
##' one time step, divided by the time step, `rate * sqrt(2 / time.step)`.
##' This holds for any correlation between east and north displacements. The
##' speed depends on the time step (halving it raises the speed by a factor
##' of sqrt(2)), so speeds from fits at different time steps are not
##' comparable. It is a net-displacement speed, not a path speed.
##'
##' Within each imputation the locations are treated as data, which keeps the
##' likelihood bounded. Each individual is fitted separately. Rubin's rules
##' pool the log rate at each time and the log of each parameter, combining
##' the within-imputation (Laplace) variance and the between-imputation
##' variance.
##'
##' The time step should be long enough that movement between locations is
##' large relative to location error (e.g. 6-24 h for Kinéis or Argos data).
##'
##' @examples
##' \dontrun{
##' fit <- fit_ssm(ellie, model = "crw", time.step = 12)
##' m <- fit_mrm(fit, reps = 20)
##' m$fitted
##' plot(m)
##'
##' ## directly from Kinéis locations, without an SSM
##' m <- fit_mrm(kineis_locations, time.step = 12)
##' }
##'
##' @importFrom TMB MakeADFun sdreport
##' @importFrom dplyr tibble bind_rows
##' @export
##' @md
fit_mrm <- function(x,
                    reps = 20,
                    time.step = NULL,
                    max_gap = NULL,
                    kineis = ssm_control()$kineis,
                    control = mpm_control()) {

  imp <- impute_tracks(x, reps = reps, time.step = time.step, max_gap = max_gap,
                       kineis = kineis)
  estimate <- imp[imp$rep == 0, ]
  ids <- unique(estimate$id)

  ## fit every imputed track of every individual
  pb <- progress_bar(length(ids) * reps, control$verbose >= 1)
  fits <- lapply(ids, function(i) {
    lapply(seq_len(reps), function(k) {
      f <- mrm_fit_one(imp[imp$rep == k & imp$id == i, ], control)
      pb$tick()
      f
    })
  })
  pb$close()
  names(fits) <- ids
  n_failed <- sum(vapply(unlist(fits, recursive = FALSE), is.null, logical(1)))

  fitted <- list()
  par <- list()
  for (i in ids) {
    f <- Filter(Negate(is.null), fits[[i]])
    if (length(f) == 0) {
      warning("the movement rate model failed for every imputation of ", i, call. = FALSE)
      next
    }
    e <- estimate[estimate$id == i, ]
    lr <- pool_rubin(do.call(cbind, lapply(f, function(z) z$log_rate)),
                     do.call(cbind, lapply(f, function(z) z$log_rate.se)))
    ## speed (km/h): root-mean-square net displacement over one time step,
    ##  divided by the time step: rate * sqrt(2 * dt) / dt
    to_speed <- mrm_speed_factor(attr(imp, "time.step"))
    fitted[[i]] <- tibble(id = i, date = e$date, lon = e$lon, lat = e$lat,
                          log_rate = lr$estimate, log_rate.se = lr$se,
                          rate = exp(lr$estimate), lower = exp(lr$lower), upper = exp(lr$upper),
                          speed = exp(lr$estimate) * to_speed,
                          speed_lower = exp(lr$lower) * to_speed,
                          speed_upper = exp(lr$upper) * to_speed,
                          fraction_missing = lr$fraction_missing)
    p <- pool_rubin(do.call(cbind, lapply(f, function(z) z$par[, 1])),
                    do.call(cbind, lapply(f, function(z) z$par[, 2])))
    ## back-transform: sigma_rate from the log scale; rho_p from its link
    link <- function(v, name) if (name == "l_sigma_rate") exp(v) else 2 / (1 + exp(-v)) - 1
    nm <- rownames(f[[1]]$par)
    par[[i]] <- tibble(id = i,
                       parameter = sub("^l_", "", nm),
                       estimate = mapply(link, p$estimate, nm),
                       lower = mapply(link, p$lower, nm),
                       upper = mapply(link, p$upper, nm),
                       imputations = length(f))
  }
  if (length(fitted) == 0) stop("the movement rate model failed for every track", call. = FALSE)

  out <- list(fitted = bind_rows(fitted),
              par = bind_rows(par),
              reps = reps,
              n_failed = n_failed,
              source = attr(imp, "source"),
              time.step = attr(imp, "time.step"))
  class(out) <- append("mrm_mi", class(out))
  out
}

## fit the movement rate model to one imputed track of one individual;
##  returns NULL if the fit fails or does not converge
mrm_fit_one <- function(s, control) {
  s <- s[order(s$date), ]
  n <- nrow(s)
  if (n < 3) return(NULL)
  earth <- 6371.0088
  rad <- pi / 180
  ## ground displacements (km) between consecutive locations
  dlon <- ((c(0, diff(s$lon)) + 180) %% 360 - 180) * rad
  dlat <- c(0, diff(s$lat)) * rad
  latm <- (s$lat + c(s$lat[1], s$lat[-n])) / 2 * rad
  d <- cbind(earth * dlon * cos(latm), earth * dlat)
  dt <- c(1, diff(as.numeric(s$date)) / 3600)
  start <- as.integer(c(TRUE, s$segment[-1] != s$segment[-n]))
  if (sum(start == 0) < 2) return(NULL)

  move <- start == 0
  rate0 <- sqrt(mean((d[move, 1]^2 + d[move, 2]^2) / (2 * dt[move])))
  parameters <- list(l_rate = rep(log(max(rate0, 1e-4)), n),
                     l_sigma_rate = log(0.1),
                     l_rho_p = 0)
  obj <- try(MakeADFun(data = list(model_name = "mrm", d = d, dt = dt, start = start),
                       parameters = parameters,
                       random = "l_rate",
                       DLL = "aniMotum",
                       silent = TRUE,
                       inner.control = list(smartsearch = TRUE)),
             silent = TRUE)
  if (inherits(obj, "try-error")) return(NULL)
  opt <- suppressWarnings(try(nlminb(obj$par, obj$fn, obj$gr,
                                     control = control$control,
                                     lower = c(l_sigma_rate = -10, l_rho_p = -Inf),
                                     upper = c(l_sigma_rate = 5, l_rho_p = Inf)),
                              silent = TRUE))
  if (inherits(opt, "try-error") || opt$convergence != 0) return(NULL)
  rep <- suppressWarnings(try(sdreport(obj), silent = TRUE))
  if (inherits(rep, "try-error") || !isTRUE(rep$pdHess)) return(NULL)
  rdm <- summary(rep, "random")
  fxd <- summary(rep, "fixed")
  list(log_rate = rdm[, 1], log_rate.se = rdm[, 2], par = fxd[, 1:2, drop = FALSE])
}

## multiplier converting the movement rate (km h^-1/2) to a speed (km/h) at
##  time step dt (h): root-mean-square net displacement over dt, divided by dt
mrm_speed_factor <- function(dt) sqrt(2 / dt)
