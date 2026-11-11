##' \code{ssm_control} selects the numerical minimizer, method, associated
##' control parameters, and parameter bounds used by \code{fit_ssm}.
##'
##' The optimizer used to minimize the objective function is
##' selected by the \code{optim} argument.  Additional control
##' parameters specific to the chosen optimizer are specified via the
##' dots argument.  See \code{\link{nlminb}} and \code{\link{optim}}
##' for available options. Adapted from S. Wotherspoon
##' \url{https://github.com/SWotherspoon/RWalc/blob/master/R/RWalc.R}
##'
##' @title Control Values for \code{fit_ssm}.
##' @param optim the numerical optimizer used in the fit
##' @param method if optim = "optim" then the optimization method to be used 
##' can be one of "BFGS", "L-BFGS-B", "Nelder-Mead", "CG", "SANN", or "Brent"
##' see \code{\link{optim}} for details
##' @param lower a list named parameter lower bounds, if NULL then built in
##' defaults are used when \code{method = "L-BFGS-B"}. Possible parameter names are:
##' \code{l_sigma} a vector of length 2, log scale; \code{l_rho_p} a scalar, logit scale;
##' \code{l_D} a scalar, log scale; \code{l_psi} a scalar, log scale;
##' \code{l_tau} a vector of length 2, log scale; \code{l_rho_o} a scalar, logit scale
##' @param upper a list of named parameter upper bounds, if NULL then built in
##' defaults are used when \code{method = "L-BFGS-B"}. Possible parameter names are same as \code{lower}
##' @param verbose integer; report progress during minimization: 0 = silent;
##' 1 = parameter trace (default); 2 = optimizer trace
##' @param se logical; should standard errors for speed estimates be calculated (default = FALSE). 
##' Turning this on will slow down computation time but provide SE's for speed-along-track calculations
##' @param gap.thresh gap threshold in hours; transitions where dt > gap.thresh 
##' have velocity innovation reset to marginal (suppresses looping). Inf = no gap 
##' flagging (default).
##' @param ho_scale haulout process variance scale factor (0, 1];
##' 0.01 = nearly stationary; 1 = no constraint
##' @param kineis a list controlling the Kinéis (KI) observation model, used
##' only for locations supplied with an error radius (see `format_data()`).
##' Each KI location has ground standard deviations
##' `scale[1] * r^exponent` (east) and `scale[2] * r^exponent` (north), with
##' `r` the error radius in km. Elements:
##' \code{scale} the east and north error scales relative to the error radius
##' (default \code{c(1.061, 0.739)});
##' \code{exponent} the exponent on the error radius (default 1);
##' \code{estimate} logical; estimate the two scales (default FALSE, fixed at
##' \code{scale});
##' \code{estimate.exponent} logical; estimate the exponent (default FALSE).
##' The default scales are provisional: they are the root mean square east and
##' north errors, relative to the error radius, of 1,700 Kinéis locations
##' paired with Fastloc GPS fixes (21 olive ridley turtles, IMOS campaign
##' tu126, 2026). Kinéis errors are serially correlated through CLS Kalman
##' smoothing, which is not yet modelled, so estimating the scales without
##' concurrent GPS locations is not advised.
##' @param ... control parameters for the chosen optimizer
##' @return Returns a list with components
##'   \item{\code{optim}}{the name of the numerical optimizer as a
##'   string, "nlminb" or "optim"}
##'   \item{\code{method}}{optimization method to be used}
##'   \item{\code{lower}}{named list of lower parameter bounds}
##'   \item{\code{upper}}{named list of upper parameter bounds}
##'   \item{\code{verbose}}{level of tracing information to be reported}
##'   \item{\code{control}}{list of control parameters for the optimizer}
##' @seealso \code{\link{nlminb}}, \code{\link{optim}}.
##' @examples
##' fit <- fit_ssm(ellie,
##' vmax = 4,
##' model = "crw",
##' time.step = 72,
##' control = ssm_control(
##'     optim = "nlminb",
##'     eval.max = 2000)
##'     )
##' @export

ssm_control <-
  function(optim = c("nlminb", "optim"),
           method = c("L-BFGS-B", "BFGS", "Nelder-Mead", "CG", "SANN", "Brent"),
           lower = NULL,
           upper = NULL,
           verbose = 1,
           se = FALSE,
           gap.thresh = Inf,    
           ho_scale   = 0.01,   
           kineis = list(scale = c(1.061, 0.739),
                         exponent = 1,
                         estimate = FALSE,
                         estimate.exponent = FALSE),
           ...) {
    optim <- match.arg(optim)
    method <- match.arg(method)
    
    # check for valid args
    if (!is.null(lower) & !inherits(lower, "list"))
      stop("\nlower parameter bounds must be specified as a named list")
    if (!is.null(upper) & !inherits(upper, "list"))
      stop("\nupper parameter bounds must be specified as a named list")
    if ((!is.null(lower) | !is.null(upper)) & (length(lower) > 7 | length(upper) > 7))
      stop("\nthe number of parameters must be <= 7")
    
    if(!optim %in% c("nlminb", "optim")) 
      stop("optimiser can only be either `nlminb` or `optim`")
    if(!method %in% c("L-BFGS-B", "BFGS", "Nelder-Mead", "CG", "SANN", "Brent")) 
      stop("optMeth can only be `L-BFGS-B`, `BFGS`, `Nelder-Mead`, `CG`, `SANN`, or `Brent` - see ?optim")
    if(!(is.numeric(verbose) & verbose %in% c(0,1,2))) 
      stop("verbose must be a numeric value of 0 = `be silent`, 1 = `show parameter trace` (default), or 2 = `show optimisere trace`")
    
    ## Kinéis observation model: fill in any elements not supplied
    kineis.default <- list(scale = c(1.061, 0.739), exponent = 1,
                           estimate = FALSE, estimate.exponent = FALSE)
    if (!is.list(kineis)) stop("kineis must be a named list, see ?ssm_control")
    kineis.default[names(kineis)] <- kineis
    kineis <- kineis.default
    if (!(is.numeric(kineis$scale) && length(kineis$scale) == 2 && all(kineis$scale > 0)))
      stop("kineis$scale must be 2 positive values (east, north)")
    if (!(is.numeric(kineis$exponent) && length(kineis$exponent) == 1))
      stop("kineis$exponent must be a single numeric value")
    if (!is.logical(kineis$estimate) || !is.logical(kineis$estimate.exponent))
      stop("kineis$estimate and kineis$estimate.exponent must be TRUE or FALSE")

    dots <- list(...)
    
    ## Set default control values
    pars <- switch(
      optim,
      nlminb = {
          list(
            eval.max = 3000,
            iter.max = 2000,
            rel.tol = 1.0e-3,
            x.tol = 1.5e-2
          )
      },
      optim = list(
        method = "L-BFGS-B", 
        maxit = 2000, 
        reltol = 1.0e-3
        )
    )
    ## Override control parameters
    pars[names(dots)] <- dots
    list(optim = optim,
         method = method,
         lower = lower,
         upper = upper,
         verbose = verbose,
         se = se,
         gap.thresh = gap.thresh,
         ho_scale = ho_scale,
         kineis = kineis,
         control = pars)
  }
