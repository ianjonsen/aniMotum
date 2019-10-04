##' @title fit the move persistence model to regularized location data
##'
##' @description generates initial values for model parameters and unobserved gamma's;
##' structures data and initial values for C++ \code{TMB} template;
##' fits move persistence model; minimizes the joint log-likelihood via the selected
##' optimizer (\code{nlminb} or \code{optim}); structures and passes output
##' object to \code{fit_mpm}
##'
##' @details called by \code{fit_mpm}, see ?fit_mpm.
##'
##' @param x temporally regularized location data, eg. output from \code{fit_ssm}
##' @param model specify whether MPM is to be fit with unpooled ("mpm") or pooled ("jmpm") RW variance(s). 
##' @param optim numerical optimizer to be used ("nlminb" or "optim")
##' @param verbose report progress during minimization
##' @param control list of control parameters for the outer optimization (type ?nlminb or ?optim for details)
##' @param inner.control list of control settings for the inner optimization
##' (see ?TMB::MakeADFUN for additional details)
##'
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom dplyr mutate filter select full_join arrange lag bind_cols "%>%"
##' @importFrom tibble tibble
##' @importFrom stats plogis
##'
##' @export

mpmf <-
  function(x,
           model = c("mpm", "jmpm"),
           optim = c("nlminb", "optim"),
           verbose = FALSE,
           control = NULL,
           inner.control = NULL) {
    
    call <- match.call()
    optim <- match.arg(optim)
    model <- match.arg(model)
    
    ## populate control list if any parameters specified...
    if (length(control)) {
      nms <- names(control)
      if (!is.list(control) || is.null(nms))
        stop("'control' argument must be a named list")
    }
   
    switch(model,
           jmpm = {
             A <- length(unique(x$id))
             idx <- c(0, cumsum(as.numeric(table(x$id))))
             
             data.tmb <- list(
               model_name = model,
               x = cbind(x$lon, x$lat),
               A = A,
               idx = idx
             )
             
           },
           mpm = {
             data.tmb <- list(
               model_name = model,
               x = cbind(x$lon, x$lat)
             )
           })
    
    parameters <- list(
      lg = rep(0, dim(x)[1]),
      l_sigma = c(0, 0),
      l_sigma_g = 0
    ) 
    
    ## TMB - create objective function
    if (is.null(inner.control) | !"smartsearch" %in% names(inner.control)) {
      inner.control <- list(smartsearch = TRUE)
    }
    
    obj <-
      MakeADFun(
        data = data.tmb,
        parameters = parameters,
        random = c("lg"),
        DLL = "foieGras",
        silent = !verbose,
        inner.control = inner.control
      )
    
    ## add par values to trace if verbose = TRUE
    myfn <- function(x) {
      print("pars:")
      print(x)
      obj$fn(x)
    }
    
    ## Minimize objective function
    opt <-
      suppressWarnings(switch(optim,
                              nlminb = try(nlminb(obj$par,
                                                  obj$fn,
                                                  #myfn,
                                                  obj$gr,
                                                  control = control
                              )),
                              optim = try(do.call(
                                optim,
                                args = list(
                                  par = obj$par,
                                  fn = obj$fn,
                                  gr = obj$gr,
                                  method = "L-BFGS-B",
                                  control = control
                                )
                              ))))
    
    
    ## Parameters, states and the fitted values
    rep <- suppressWarnings(try(sdreport(obj, getReportCovariance = TRUE)))
    fxd <- summary(rep, "report")
    fxd_log <- summary(rep, "fixed")
    rdm <- summary(rep, "random")
    
    lg <- rdm[rownames(rdm) %in% "lg", ]
    
    fitted <- tibble(
      id = x$id,
      date = x$date,
      g = plogis(lg[, 1]),
      g.se = lg[,2]      ## FIXME: rescale this to SE of prob
    )
    
    if (optim == "nlminb") {
      aic <- 2 * length(opt[["par"]]) + 2 * opt[["objective"]]
    } else if (optim == "optim") {
      aic <- 2 * length(opt[["par"]]) + 2 * opt[["value"]]
    }
    
    row.names(fxd)[(nrow(fxd)-1):nrow(fxd)] <- c("sigma_lon", "sigma_lat")
    
    out <- list(
      fitted = fitted,
      par = fxd,
      model = model,
      data = x,
      tmb = obj,
      opt = opt,
      rep = rep,
      aic = aic
    )   
    
    class(out) <- append("mpm", class(out))
    return(out)
  }