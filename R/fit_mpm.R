##' @title fit a a Move Persistence Model (mpm)
##' @description fit a random walk with time-varying move persistence to location data (e.g., output from \code{fit_ssm})
##' @param x a data frame of observations (see details)
##' @param optim numerical optimizer
##' @param verbose report progress during minimization
##' @param control list of control parameters for the outer optimization (type ?nlminb or ?optim for details)
##' @param inner.control list of control parameters for the inner optimization
##' @return a list with components
##' \item{\code{fitted}}{a dataframe of fitted locations}
##' \item{\code{par}}{model parameter summmary}
##' \item{\code{data}}{input dataframe}
##' \item{\code{tmb}}{the tmb object}
##' \item{\code{opt}}{the object returned by the optimizer}
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom tibble data_frame
##' @export
fit_mpm <- function(x,
                    optim = c("nlminb", "optim"),
                    verbose = FALSE,
                    control = NULL,
                    inner.control = NULL) {
  
  optim <- match.arg(optim)
  
  A <- length(unique(x$id))
  idx <- c(0, cumsum(as.numeric(table(x$id))))
  
  data.tmb <- list(
    model_name = "mpm",
    x = cbind(x$lon, x$lat),
    A = A,
    idx = idx
  )
  
  parameters <- list(
    lg = rep(0, dim(x)[1]),
    log_sigma = c(0, 0),
    log_sigma_g = 0
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
  
  
  obj$env$tracemgc <- verbose
  
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
  rep <- suppressWarnings(try(sdreport(obj)))
  fxd <- summary(rep, "report")
  fxd_log <- summary(rep, "fixed")
  rdm <- summary(rep, "random")
  
  lg <- rdm[rownames(rdm) %in% "lg", ]
  
  fitted <- data_frame(
    id = x$id,
    date = x$date,
    g = plogis(lg[, 1]),
    g.se = lg[, 2]
  )
  
  if (optim == "nlminb") {
    aic <- 2 * length(opt[["par"]]) + 2 * opt[["objective"]]
  } else if (optim == "optim") {
    aic <- 2 * length(opt[["par"]]) + 2 * opt[["value"]]
  }
  row.names(fxd)[2:3] <- c("sigma_lon", "sigma_lat")
  
  list(
    fitted = fitted,
    par = fxd,
    data = x,
    tmb = obj,
    opt = opt,
    rep = rep,
    aic = aic
  )
  
}
