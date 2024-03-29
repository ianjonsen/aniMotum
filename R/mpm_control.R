##' \code{mpm_control} selects the numerical minimizer, method, associated
##' control parameters, and parameter bounds used by \code{fit_mpm}.
##'
##' The optimizer used to minimize the objective function is
##' selected by the \code{optim} argument.  Additional control
##' parameters specific to the chosen optimizer are specified via the
##' dots argument.  See \code{\link{nlminb}} and \code{\link{optim}}
##' for available options. Adapted from S. Wotherspoon
##' \url{https://github.com/SWotherspoon/RWalc/blob/master/R/RWalc.R}
##'
##' @title Control Values for \code{fit_mpm}.
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
##' 1 = optimizer trace; 2 = parameter trace (default))
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
##' @export

mpm_control <-
  function(optim = c("nlminb", "optim"),
           method = c("L-BFGS-B", "BFGS", "Nelder-Mead", "CG", "SANN", "Brent"),
           lower = NULL,
           upper = NULL,
           verbose = 1,
           ...) {
    optim <- match.arg(optim)
    method <- match.arg(method)
    
    # check for valid args
    if (!is.null(lower) & !inherits(lower, "list"))
      stop("\nlower parameter bounds must be specified as a named list")
    if (!is.null(upper) & !inherits(upper, "list"))
      stop("\nupper parameter bounds must be specified as a named list")
    if ((!is.null(lower) | !is.null(upper)) & (length(lower) > 2 | length(upper) > 2))
      stop("\nthe number of parameters must be <= 2")
    
    if(!optim %in% c("nlminb", "optim")) 
      stop("optimiser can only be either `nlminb` or `optim`")
    if(!method %in% c("L-BFGS-B", "BFGS", "Nelder-Mead", "CG", "SANN", "Brent")) 
      stop("optMeth can only be `L-BFGS-B`, `BFGS`, `Nelder-Mead`, `CG`, `SANN`, or `Brent` - see ?optim")
    if(!(is.numeric(verbose) & verbose %in% c(0,1,2))) 
      stop("verbose must be a numeric value of 0 = `be silent`, 1 = `show parameter trace` (default), or 2 = `show optimisere trace`")
    
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
         control = pars)
  }
