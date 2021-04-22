##' @title fit a a Move Persistence Model (mpm)
##' @description fit a random walk with time-varying move persistence to 
##' temporally regular or irregular location data
##' @param x a `fG_ssm` fit object or a data frame of observations (see details)
##' @param what if a `fG_ssm` fit object is supplied then \code{what} determines
##'  whether fitted or predicted (default) values are mapped; ignored if 
##'  \code{x} is a data frame
##' @param model mpm model to fit; either \code{mpm} with unpooled random walk 
##' variance parameters (\code{sigma_(g,i)}) or \code{jmpm} with a single, 
##' pooled random variance parameter (\code{sigma_g})
##' @param coords column numbers of the location coordinates (default = 3:4)
##' @param control list of control settings for the outer optimizer (see \code{mpm_control} for details)
##' @param inner.control list of control parameters for the inner optimization
##' @param verbose `r lifecycle::badge("deprecated")` use ssm_control(verbose = 1) instead, see \code{ssm_control} for details
##' @param optim `r lifecycle::badge("deprecated")` use ssm_control(optim = "optim") instead, see \code{ssm_control} for details
##' @param optMeth `r lifecycle::badge("deprecated")` use ssm_control(method = "L-BFGS-B") instead, see \code{ssm_control} for details
##' 
##' @return a list with components
##' \item{\code{fitted}}{a dataframe of fitted locations}
##' \item{\code{par}}{model parameter summary}
##' \item{\code{data}}{input dataframe}
##' \item{\code{tmb}}{the tmb object}
##' \item{\code{opt}}{the object returned by the optimizer}
##' 
##' @examples
##' ## fit jmpm to two southern elephant seal tracks
##' data(xs)
##' fmpm <- fit_mpm(xs, model = "jmpm")
##' 
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom dplyr "%>%" mutate select
##' @importFrom purrr map
##' @importFrom lifecycle deprecate_warn
##' @export
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
    deprecate_warn("0.7-5", "fit_ssm(verbose)", 
                   details = "use `control = ssm_control(verbose)` instead")
    control$verbose <- verbose
  }
  if(!is.null(optim)) {
    deprecate_warn("0.7-5", "fit_ssm(optim)", 
                   details = "use `control = ssm_control(optim)` instead")
    if(optim %in% c("nlminb", "optim")) control$optim <- optim
    else stop("invalid optimiser specified, see ?ssm_control for options")
  }
  if(!is.null(optMeth)) {
    deprecate_warn("0.7-5", "fit_ssm(optMeth)", 
                   details = "use `control = ssm_control(method)` instead")
    if(optMeth %in% c("L-BFGS-B", "BFGS", "Nelder-Mead", "CG", "SANN", "Brent"))
      control$method <- optMeth
    else stop("invalid optimisation method specified, see ?ssm_control for options")
  }
  
  
  if(control$verbose == 1)
    cat(paste0("fitting ", model, "...\n"))

  if(inherits(x, "fG_ssm")) {
    x <- grab(x, what = what, as_sf = FALSE) %>%
      select(id, date, coords[1], coords[2])
  } else {
    x <- x %>% select(id, date, coords[1], coords[2])
  }
  
  if(all(c("x","y") %in% names(x))) {
    # rescale x,y in km for better optimisation
    xm <- max(x$x)
    ym <- max(x$y)
    x <- x %>% mutate(x = x/xm, y = y/ym)
  } else {
    # standardise coord names to x,y 
    names(x)[3:4] <- c("x","y")
  }

  switch(model,
         mpm = {
           fit <- split(x, x$id) %>%
             map(~ try(mpmf(.x, 
                        model = model,
                        control = control,
                        inner.control = inner.control
             ), silent = TRUE)
             )
           
          fit <- tibble(id = names(fit), mpm = fit) %>%
            mutate(converged = sapply(.$mpm, function(x) 
              if(length(x) == 8) {
                x$opt$convergence == 0
              } else if(length(x) < 8) {
                FALSE
              })) %>%
            mutate(model = model)
         },
         jmpm = {
           fit <- try(mpmf( 
               x = x,
               model = model,
               control = control,
               inner.control = inner.control
             ), silent = TRUE)
           
           fit <- tibble(mpm = list(fit)) %>%
             mutate(converged = ifelse(length(.$mpm[[1]]) == 8, 
                                       .$mpm[[1]]$opt$convergence == 0, 
                                       FALSE)) %>%
             mutate(model = model)
         })

  class(fit) <- append("fG_mpm", class(fit))  

return(fit)
  
}
