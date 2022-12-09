##' @title fit a **Move Persistence Model** (`mpm`)
##' 
##' @description fit a random walk with time-varying move persistence to 
##' temporally regular or irregular location data
##' 
##' @param x a `ssm_df` fit object or a data frame of observations (see details)
##' @param what if a `ssm_df` fit object is supplied then `what` determines
##'  whether fitted, predicted (default), or rerouted values are mapped; ignored if 
##'  `x` is a data frame
##' @param model mpm model to fit; either `mpm` with unpooled random walk 
##' variance parameters (`sigma_(g,i)`) or `jmpm` with a single, 
##' pooled random variance parameter (`sigma_g`)
##' @param coords column numbers of the location coordinates (default = 3:4)
##' @param control list of control settings for the outer optimizer 
##' (see [aniMotum::mpm_control] for details)
##' @param inner.control list of control parameters for the inner optimization
##' @param verbose is deprecated, use ssm_control(verbose = 1) instead, 
##' see [aniMotum::ssm_control] for details
##' @param optim is deprecated, use ssm_control(optim = "optim") instead, 
##' see [aniMotum::ssm_control] for details
##' @param optMeth is deprecated, use ssm_control(method = "L-BFGS-B") instead, 
##' see [ssm_control] for details
##' 
##' @return a list with components
##' * `fitted` a dataframe of fitted locations
##' * `par` model parameter summary
##' * `data` input data.frame
##' * `tmb` the `TMB` object
##' * `opt` the object returned by the optimizer
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
                    verbose = NULL
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
  
  if(inherits(x, "ssm_df")) {
    x <- grab(x, what = what, as_sf = FALSE)[, c(1:2, coords[1], coords[2])]
  } else {
    x <- x[, c(1:2, coords[1], coords[2])]
  }
  
  if(all(c("x","y") %in% names(x))) {
    # rescale x,y in km for better optimisation
    x <- with(x, data.frame(x = x/max(x), y = y/max(y)))
  } else {
    # standardise coord names to x,y 
    names(x)[3:4] <- c("x","y")
  }

  switch(model,
         mpm = {
           fit <- lapply(split(x, x$id), function(x) {
             try(mpmf(x, 
                      model = model,
                      control = control,
                      inner.control = inner.control
                      ), silent = TRUE)
           })
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
