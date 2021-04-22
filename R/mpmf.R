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
##' @param control list of control settings for the outer optimizer (see \code{mpm_control} for details)
##' @param inner.control list of control settings for the inner optimization
##' (see ?TMB::MakeADFUN for additional details)
##' @param verbose `r lifecycle::badge("deprecated")` use ssm_control(verbose = 1) instead, see \code{ssm_control} for details
##' @param optim `r lifecycle::badge("deprecated")` use ssm_control(optim = "optim") instead, see \code{ssm_control} for details
##' @param optMeth `r lifecycle::badge("deprecated")` use ssm_control(method = "L-BFGS-B") instead, see \code{ssm_control} for details
##'
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom dplyr mutate arrange "%>%" count
##' @importFrom tibble tibble
##' @importFrom stats plogis median
##' @keywords internal

mpmf <-
  function(x,
           model = c("jmpm", "mpm"),
           control = mpm_control(),
           inner.control = NULL) {
    
    call <- match.call()
    model <- match.arg(model)
   
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
               x = cbind(x$x, x$y),
               dt = x$dt,
               A = as.integer(A),
               idx = as.integer(idx)
             )
           },
           mpm = {
             data.tmb <- list(model_name = model,
                              x = cbind(x$x, x$y),
                              dt = x$dt)
           })
    
    parameters <- list(
      lg = rep(0, dim(x)[1]),
      log_sigma = c(0, 0),
      log_sigma_g = 0
    )
    
    ## TMB - create objective function
    if (is.null(inner.control) | !"smartsearch" %in% names(inner.control)) {
      inner.control <- list(smartsearch = TRUE)
    }
    
    rnd <- switch(model, jmpm = "lg", mpm = "lg")

    obj <-
      MakeADFun(
        data = data.tmb,
        parameters = parameters,
        random = rnd,
        method = control$method,
        DLL = "foieGras",
        silent = !ifelse(control$verbose == 2, TRUE, FALSE),
        inner.control = inner.control
      )
    
    obj$env$tracemgc <- ifelse(control$verbose == 2, TRUE, FALSE)
    
    ## add par values to trace if verbose = TRUE
    myfn <- function(x) {
      cat("\r", "pars:  ", round(x, 5), "     ")
      flush.console()
      obj$fn(x)
    }
    
    ## Set parameter bounds - most are -Inf, Inf
    L = c(log_sigma=c(-10,-10),
          log_sigma_g=-10
          )
    U = c(log_sigma=c(50,50),
          log_sigma_g=50
          )
    if(any(!is.null(control$lower))) {
      L[which(names(L) %in% names(control$lower))] <- unlist(control$lower)
    }
    if(any(!is.null(control$upper))) {
      U[which(names(U) %in% names(control$upper))] <- unlist(control$upper)
    } 
    
    names(L) <- c("log_sigma", "log_sigma", "log_sigma_g")
    names(U) <- c("log_sigma", "log_sigma", "log_sigma_g")
    
#    if(model == "jmpm") map <- list(log_sigma_g = factor(NA))
    
    # Remove inactive parameters from bounds
    L <- L[!names(L) %in% names(map)]
    U <- U[!names(U) %in% names(map)]
    
    ## Minimize objective function
    oldw <- getOption("warn")
    options(warn = -1)  ## turn warnings off but check if optimizer crashed & return warning at end
    opt <-
      switch(control$optim,
             nlminb = try(nlminb(obj$par,
                                 ifelse(control$verbose == 1, myfn, obj$fn),
                                 obj$gr,
                                 control = control$control,
                                 lower = L,
                                 upper = U
             )), 
             optim = try(do.call(
               optim,
               args = list(
                 par = obj$par,
                 fn = ifelse(control$verbose == 1, myfn, obj$fn),
                 gr = obj$gr,
                 method = control$method,
                 control = control$control,
                 lower = L,
                 upper = U
               )
             ), silent = TRUE))
    cat("\n")
    
    ## if error then exit with limited output to aid debugging
    ## check if pdHess is FALSE at end and return warning
    rep <- try(sdreport(obj))
    
    options(warn = oldw) ## turn warnings back on

    ## Parameters, states and the fitted values
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
    
    if (control$optim == "nlminb") {
      aic <- 2 * length(opt[["par"]]) + 2 * opt[["objective"]]
    } else if (control$optim == "optim") {
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