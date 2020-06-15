##' @title fit the move persistence model to regularized location data
##'
##' @description generates initial values for model parameters and unobserved gamma's;
##' structures data and initial values for C++ \code{TMB} template;
##' fits move persistence model; minimizes the joint log-likelihood via the selected
##' optimizer (\code{nlminb} or \code{optim}); structures and passes output
##' object to \code{fit_mpm}
##'
##' @details called by \code{fit_mpm}, not intended for general use. see ?fit_mpm.
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
##' @importFrom dplyr mutate arrange "%>%" count
##' @importFrom tibble tibble
##' @importFrom stats plogis median
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
   
    # Create a tid column if there is none specified
    if(all(colnames(x) != "tid")){
      x$tid <- NA
    }
    
    # ordering the data to make sure we have continuous tracks and ids are ordered
    x <- x %>% arrange(id, tid, date)
    
    # get index of start and end of tracks
    x <- x %>% mutate(idtid = paste(id, tid, sep=""))
    idx <- x$idtid %>%
      table() %>%
      as.numeric() %>%
      cumsum() %>%
      c(0, .)
    
    # Create dt vector
    # dt = t_i - t_{i-1} and include in data.tmb
    x$dt <- c(NA, diff(x$date))
    x$dt[idx[1:(length(idx)-1)] + 1] <- NA
    # Scale to median
    x$dt <- x$dt / median(x$dt, na.rm=TRUE)

        switch(model,
           jmpm = {
             # Number of tracks (or individual if only one track per individual)
             A <- nrow(count(x, id, tid))
             data.tmb <- list(
               model_name = model,
               x = cbind(x$lon, x$lat),
               dt = x$dt,
               A = as.integer(A),
               idx = as.integer(idx)
             )
           },
           mpm = {
             data.tmb <- list(
               model_name = model,
               x = cbind(x$lon, x$lat),
               dt = x$dt
             )
           })
  
    parameters <- switch(model, 
                         jmpm = {
                           list(
                             lg = rep(0, dim(x)[1]),
                             log_sigma = c(0, 0),
                             log_sigma_g = 0
                             )
                           },
                           mpm = {
                             list(
                               lg = rep(0, dim(x)[1]),
                               log_sigma = c(0, 0),
                               log_sigma_g = 0
                             )
                           })
    
##    rnd <- switch(model, mpm = "lg", jmpm = c("lg","log_sigma_g"))
    
    ## TMB - create objective function
    if (is.null(inner.control) | !"smartsearch" %in% names(inner.control)) {
      inner.control <- list(smartsearch = TRUE)
    }
    
    obj <-
      MakeADFun(
        data = data.tmb,
        parameters = parameters,
        random = "lg",
        DLL = "foieGras",
        silent = !verbose,
        inner.control = inner.control
      )
    
    ## add par values to trace if verbose = TRUE
    myfn <- function(x) {
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
                                  #fn = myfn,
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
    
    lgs <- rdm[rownames(rdm) %in% "lg", ]
        
    if(all(is.na(x$tid))) {
      fitted <- tibble(
        id = x$id,
        date = x$date,
        g = plogis(lgs[, 1]),
        g.se = lgs[, 2]      
      )
    } else {
      fitted <- tibble(
        id = x$id,
        tid = x$tid,
        date = x$date,
        g = plogis(lgs[, 1]),
        g.se = lgs[, 2]     
      )
    }
    
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