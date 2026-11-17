##' @title fit a **Move Persistence Model** (`mpm`)
##' 
##' @description fit a random walk with time-varying move persistence to 
##' temporally regular or irregular location data
##' 
##' @param x a `ssm_df` fit object or a data frame of observations (see details).
##' With `reps`, an `ssm_df` fit object (`rw`, `crw` or `mp`) with regular
##' predicted locations, or a data frame of Kinéis locations (see
##' [impute_tracks()])
##' @param what if a `ssm_df` fit object is supplied then `what` determines
##'  whether fitted, predicted (default), or rerouted values are mapped; ignored if 
##'  `x` is a data frame
##' @param model mpm model to fit; either `mpm` with unpooled random walk 
##' variance parameters (`sigma_(g,i)`) or `jmpm` with a single, 
##' pooled random variance parameter (`sigma_g`)
##' @param coords column numbers of the location coordinates (default = 3:4)
##' @param control list of control settings for the outer optimizer 
##' (see [aniMotum::mpm_control] for details). With `model = "mpm"` and more than one
##' individual, `verbose = 1` shows a progress bar over individuals instead of
##' the parameter trace; with `reps`, a progress bar over the imputation fits
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
##' rules, so the results carry location uncertainty (see details).
##' @param time.step with `reps` and Kinéis locations, the time step (h) of
##' the regular grid (see [impute_tracks()])
##' @param max_gap with `reps` and Kinéis locations, the longest gap (h)
##' interpolated across (see [impute_tracks()])
##' @param kineis with `reps` and Kinéis locations, the Kinéis error model (see
##' [impute_tracks()])
##' 
##' @return a list with components
##' * `fitted` a dataframe of fitted locations
##' * `par` model parameter summary
##' * `data` input data.frame
##' * `tmb` the `TMB` object
##' * `opt` the object returned by the optimizer
##'
##' With `reps`, a list with class `mpm_mi`:
##' * `fitted` a tibble with, for each id, track segment and time, the
##' point-estimate track (`lon`, `lat`), the pooled `logit_g` and its total
##' standard error (`logit_g.se`), move persistence `g` and its 95% interval
##' (`lower`, `upper`), and the fraction of missing information
##' (`fraction_missing`: the share of the variance due to location
##' uncertainty)
##' * `par` pooled estimates of `sigma_g`, `sigma_x` and `sigma_y` (`sigma_x`,
##' `sigma_y` in coordinates divided by the largest absolute x or y), by
##' track segment (`mpm`) or joint
##' (`jmpm`)
##' * `model`, `reps`, `n_failed` (imputed-track fits that failed or did not
##' converge), `source` (`"ssm"` or `"kineis"`) and `time.step`
##'
##' @details With `reps`, the move persistence model is fitted to each imputed
##' track, treating its locations as data, and Rubin's rules pool `logit_g` at
##' each time and the log of each parameter. This replaces treating the SSM
##' point estimates as data, which ignores location uncertainty. Imputed
##' tracks are split into segments at long data gaps (Kinéis locations); with
##' `mpm` each segment is fitted separately, and with `jmpm` each segment is a
##' track.
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
##'
##' ## with location uncertainty, by multiple imputation
##' \dontrun{
##' fmi <- fit_mpm(xs, model = "jmpm", reps = 20)
##' plot(fmi)
##' }
##' 
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
                    kineis = ssm_control()$kineis
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
  
  
  if(control$verbose == 1)
    cat(paste0("fitting ", model, "...\n"))

  ## multiple imputation: fit to imputed tracks and pool
  if (!is.null(reps)) {
    if (!what %in% c("predicted", "p"))
      stop("with `reps`, imputed tracks are at the predicted (regular) times; use what = \"predicted\"",
           call. = FALSE)
    return(fit_mpm_mi(x, model = model, reps = reps, time.step = time.step,
                      max_gap = max_gap, kineis = kineis,
                      control = control, inner.control = inner.control))
  }
  
  if(inherits(x, "ssm_df")) {
    x <- grab(x, what = what, as_sf = FALSE)[, c(1:2, coords[1], coords[2])]
  } else {
    x <- x[, c(1:2, coords[1], coords[2])]
  }
  
  if(all(c("x","y") %in% names(x))) {
    # rescale x,y in km for better optimisation
    # Rescale in place, so that the id and date columns are kept. Divide by
    # the largest absolute value: projected coordinates are often negative,
    # and max() of an all-negative column would reverse the sign of the axis.
    # Divide x and y by one common constant, so that sigma_x and sigma_y are
    # in the same units and can be compared (e.g. at longitude 135, x is about
    # 15,000 km and y about -1,100 km, so separate constants made sigma_x 13
    # times smaller relative to sigma_y). g is unaffected either way: each
    # axis has its own sigma and the model is linear within each axis, so a
    # per-axis rescaling is absorbed exactly by that axis's sigma.
    scale <- max(abs(c(x$x, x$y)), na.rm = TRUE)
    x$x <- x$x / scale
    x$y <- x$y / scale
  } else {
    # standardise coord names to x,y 
    names(x)[3:4] <- c("x","y")
  }

  switch(model,
         mpm = {
           ## progress bar over individuals when verbose = 1; the
           ##  per-iteration parameter trace is switched off, as it would
           ##  overwrite the bar
           xs <- split(x, x$id)
           show <- control$verbose == 1 && length(xs) > 1
           ctl <- control
           if (show) ctl$verbose <- 0
           pb <- progress_bar(length(xs), show)
           fit <- lapply(xs, function(x) {
             f <- try({
               if (show) {
                 utils::capture.output(r <- mpmf(x, model = model, control = ctl,
                                                 inner.control = inner.control))
                 r
               } else {
                 mpmf(x, model = model, control = ctl, inner.control = inner.control)
               }
             }, silent = TRUE)
             pb$tick()
             f
           })
           pb$close()
          fit <- tibble(id = names(fit), mpm = fit) 
          fit <- with(fit, tibble(fit,
                                  converged = sapply(mpm, function(x) 
                                    if(length(x) == 8) {
                                      x$opt$convergence == 0
                                    } else if(length(x) < 8) {
                                      FALSE
                                    }),
                                  model = model)
                      )
         },
         jmpm = {
           fit <- try(mpmf( 
               x = x,
               model = model,
               control = control,
               inner.control = inner.control
             ), silent = TRUE)
           
           fit <- tibble(mpm = list(fit)) 
           fit <- with(fit, 
                       tibble(fit, 
                              converged = 
                                    ifelse(length(mpm[[1]]) == 8, 
                                           mpm[[1]]$opt$convergence == 0, 
                                                          FALSE),
                                  model = model)
                       )
         })

  class(fit) <- append("mpm_df", class(fit))  

return(fit)
  
}
