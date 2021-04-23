##' @title fit the state-space model to data after passing through \code{prefilter}
##'
##' @description generates initial values for model parameters and unobserved states;
##' structures data and initial values for C++ \code{TMB} template;
##' fits state-space model; minimizes the joint log-likelihood via the selected
##' optimizer (\code{nlminb} or \code{optim}); structures and passes output
##' object to \code{fit_ssm}
##'
##' @details called by \code{fit_ssm}, not intended for general use. \code{sfilter} can only fit to an
##' individual track, use \code{fit_ssm} to fit to multiple tracks (see ?fit_ssm).
##'
##' @param x Argos data passed through prefilter()
##' @param model specify which SSM is to be fit: "rw" or "crw"
##' @param time.step the regular time interval, in hours, to predict to.
##' Alternatively, a vector of prediction times, possibly not regular, must be
##' specified as a data.frame with id and POSIXt dates.
##' @param scale scale location data for more efficient optimization.
##' @param map a named list of parameters as factors that are to be fixed during estimation, e.g., list(psi = factor(NA))
##' @param parameters a list of initial values for all model parameters and
##' unobserved states, default is to let sfilter specify these. Only play with
##' this if you know what you are doing...
##' @param fit.to.subset fit the SSM to the data subset determined by prefilter
##' (default is TRUE)
##' @param control list of control parameters for the outer optimization (see \code{ssm_control} for details)
##' @param inner.control list of control settings for the inner optimization
##' (see ?TMB::MakeADFUN for additional details)
##' @param verbose `r lifecycle::badge("deprecated")` use ssm_control(verbose = 1) instead, see \code{ssm_control} for details
##' @param optim `r lifecycle::badge("deprecated")` use ssm_control(optim = "optim") instead, see \code{ssm_control} for details
##' @param optMeth `r lifecycle::badge("deprecated")` use ssm_control(method = "L-BFGS-B") instead, see \code{ssm_control} for details
##' @param lpsi `r lifecycle::badge("deprecated")` use ssm_control(lower = list(lpsi = -Inf)) instead, see \code{ssm_control} for details
##'
##' @importFrom TMB MakeADFun sdreport newtonOption FreeADFun
##' @importFrom stats approx cov sd predict nlminb optim na.omit
##' @importFrom utils flush.console
##' @importFrom dplyr mutate select full_join arrange lag bind_cols "%>%"
##' @importFrom tibble as_tibble
##' @importFrom sf st_crs st_coordinates st_geometry<- st_as_sf st_set_crs
##' @importFrom assertthat assert_that
##' @importFrom stringr str_split
##'
##'
##' @keywords internal

sfilter <-
  function(x,
           model = c("rw", "crw"),
           time.step = 6,
           scale = FALSE,
           parameters = NULL,
           map = NULL,
           fit.to.subset = TRUE,
           control = ssm_control(),
           inner.control = NULL) {

    st <- proc.time()
    call <- match.call()
    model <- match.arg(model)

    ## check args
    assert_that(inherits(x, c("sf","tbl_df")), msg = "x must be an sf-tibble produced by `prefilter()`")
    assert_that(model %in% c("rw","crw"), msg = "model can only be 1 of `rw` or `crw`")
    assert_that(any((is.numeric(time.step) & time.step > 0) | is.na(time.step) | is.data.frame(time.step)),
                msg = "time.step must be either: 1) a positive, non-zero value; 2) NA (to turn off predictions); or 3) a data.frame (see `?fit_ssm`)")
    assert_that(is.logical(scale), msg = "scale must be TRUE or FALSE to run on/off location scaling")
    assert_that(any(is.list(parameters) || is.null(parameters)),
                msg = "parameters must be a named list of parameter initial values or NULL")
    assert_that(any(is.list(map) || is.null(map)),
                msg = "map must be a named list of parameters to fix (turn off) in estimation or NULL")
    assert_that(is.logical(fit.to.subset), 
                msg = "fit.to.subset must be TRUE (fit to prefiltered observations) or FALSE (fit to all observations)")
    assert_that(any(is.list(inner.control) || is.null(inner.control)), msg = "inner.control must be a named list of valid newtonOptimiser control arguments or NULL")
    

    if(length(time.step) > 1 & !is.data.frame(time.step)) {
        stop("\ntime.step must be a data.frame with id's when specifying multiple prediction times")
    } else if(length(time.step) > 1 & is.data.frame(time.step)) {
        if(sum(!names(time.step) %in% c("id","date")) > 0) stop("\n time.step names must be `id` and `date`")
    }

    ## unlist x
   # x <- x[[1]]
    
    ## drop any records flagged to be ignored, if fit.to.subset is TRUE
    if(fit.to.subset) xx <- subset(x, keep)
    else xx <- x

    prj <- st_crs(xx)
    loc <- as.data.frame(st_coordinates(xx))
    names(loc) <- c("x","y")
    st_geometry(xx) <- NULL
    d <- cbind(xx, loc) %>%
      mutate(isd = TRUE)

    if (!inherits(time.step, "data.frame") & all(!is.na(time.step))) {
      ## prediction times - assume on time.step-multiple of the hour
      tsp <- time.step * 3600
      tms <- (as.numeric(d$date) - as.numeric(d$date[1])) / tsp
      index <- floor(tms)
      ts <-
        data.frame(date = seq(
          trunc(d$date[1], "hour"),
          by = tsp,
          length.out = max(index) + 2
        ))
      
    } else if (inherits(time.step, "data.frame") & all(!is.na(time.step))) {
      ts <- subset(time.step, id %in% unique(d$id)) %>% 
        select(date)
      
    } else if (inherits(time.step, "data.frame") & any(is.na(time.step))) {
      stop("NA's detected in user-supplied prediction times data.frame")
    }
    
    if (all(!is.na(time.step))) {
      ## add 1 s to observation time(s) that exactly match prediction time(s)
      if (sum(d$date %in% ts$date) > 0) {
        o.times <- which(d$date %in% ts$date)
        d[o.times, "date"] <- d[o.times, "date"] + 1
      }

    ## merge data and prediction times
    ## add is.data flag (distinguish obs from reg states)
    d.all <- full_join(d, ts, by = "date") %>%
      arrange(date) %>%
      mutate(isd = ifelse(is.na(isd), FALSE, isd)) %>%
      mutate(id = ifelse(is.na(id), na.omit(unique(id))[1], id))
    } else {
      d.all <- d
    }

    ## calc delta times in hours for observations & interpolation points (states)
    dt <- difftime(d.all$date, lag(d.all$date), units = "hours") %>%
      as.numeric()
    dt[1] <- 0.000001 # - 0 causes numerical issues in CRW model

    ## use approx & MA filter to obtain state initial values
    x.init1 <-
      approx(x = select(d, date, x),
             xout = d.all$date,
             rule = 2)$y
    x.init <-
      stats::filter(x.init1, rep(1, 5) / 5) %>% as.numeric()
    x.na <- which(is.na(x.init))
    x.init[x.na] <- x.init1[x.na]

    y.init1 <-
      approx(x = select(d, date, y),
             xout = d.all$date,
             rule = 2)$y
    y.init <-
      stats::filter(y.init1, rep(1, 5) / 5) %>% as.numeric()
    y.na <- which(is.na(y.init))
    y.init[y.na] <- y.init1[y.na]
    
    if(scale) {
      xs <- cbind(x.init = x.init - mean(x.init, na.rm = TRUE) / sd(x.init, na.rm = TRUE), 
                  y.init = y.init - mean(y.init, na.rm = TRUE) / sd(y.init, na.rm = TRUE)
                  )
    } else {
      xs <- cbind(x.init, y.init)
    }
    

    state0 <- c(xs[1,1], xs[1,2], 0 , 0)
    attributes(state0) <- NULL

    if (is.null(parameters)) {
      ## Estimate stochastic innovations
      es <- xs[-1,] - xs[-nrow(xs),]

      ## Estimate components of variance
      V <- cov(es)
      sigma <- sqrt(diag(V))
      rho <- V[1, 2] / prod(sigma)

      v <- cbind(c(0,diff(x.init)), c(0,diff(y.init))) / dt
      
      parameters <- list(
        l_sigma = log(pmax(1e-08, sigma)),
        l_rho_p = log((1 + rho) / (1 - rho)),
        X = t(xs),
        mu = t(xs),
        v = t(v),
        l_D = 0,
        l_psi = 0,
        l_tau = c(0, 0),
        l_rho_o = 0
      )
    }

    ## start to work out which obs_mod to use for each observation
    d <- d %>% mutate(obs.type = factor(obs.type, levels = c("LS","KF","GLS","GPS"), labels = c("LS","KF","GLS","GPS")))
    obst <- which(table(d$obs.type) > 0)
    
    automap <- switch(model, 
                     rw = {
                       list(
                         list(#l_dif = factor(NA),
                              l_psi = factor(NA)
                            ),
                         list(
                            #l_dif = factor(NA),
                            l_psi = factor(NA),
                            l_tau = factor(c(NA, NA)),
                            l_rho_o = factor(NA)
                            ),
                         list(
                            #l_dif = factor(NA),
                            l_psi = factor(NA),
                            l_tau = factor(c(NA, NA))
                            ),
                         list(#l_dif = factor(NA),
                              l_psi = factor(NA)
                            )
                         )
                       },
                     crw = {
                       list(
                         list(
                           l_psi = factor(NA)
                         ),
                         list(
                           l_tau = factor(c(NA, NA)),
                           l_rho_o = factor(NA)
                         ),
                         list(
                           l_tau = factor(c(NA, NA)),
                           l_psi = factor(NA)
                         ),
                         list(
                           l_psi = factor(NA)
                         )
                       )
                     })
    
    mm <- unlist(automap[obst], recursive = FALSE)
    mm[which(duplicated(names(mm)))] <- NULL
    automap <- mm
    
    if(!is.null(map)) {
      names(map) <- paste0("l_", names(map))
      map <- append(automap, map, after = 0)
    } else {
      map <- automap
    }

    ## TMB - data list
    ## convert from string to integer for C++
    obs_mod <- ifelse(d.all$obs.type %in% c("LS","GPS"), 0, 
                      ifelse(d.all$obs.type == "KF", 1, 2)
                      )
    ## where is.na(obs_mod) - prediction points - set to "LS" (obs_mod = 0) so
    ##  NA's don't create an int overflow situation in C++ code. This won't matter 
    ##  as isd makes likelihood contribution goes to 0 in C++ code
    obs_mod <- ifelse(is.na(obs_mod), 0, obs_mod)
       
    if(scale) {
      d.all.tmp <- d.all
      d.all <- d.all %>%
        mutate(x = x - mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE),
               y = y - mean(y, na.rm = TRUE) / sd(y, na.rm = TRUE))
    }
    
    
    data <- list(
      model_name = model,
      Y = rbind(d.all$x, d.all$y), 
      dt = dt,
      state0 = state0,
      isd = as.integer(d.all$isd),
      obs_mod = as.integer(obs_mod),
      m = d.all$smin,
      M = d.all$smaj,
      c = d.all$eor,
      K = cbind(d.all$emf.x, d.all$emf.y),
      GLerr = cbind(d.all$lonerr, d.all$laterr)
    )
    
    if(scale) d.all <- d.all.tmp
    
    ## TMB - create objective function
    if (is.null(inner.control) | !"smartsearch" %in% names(inner.control)) {
      inner.control <- list(smartsearch = TRUE)
    }
    rnd <- switch(model, rw = "X", crw = c("mu", "v"))

    obj <-
      MakeADFun(
        data,
        parameters,
        map = map,
        random = rnd,
        hessian = TRUE,
        method = control$method,
        DLL = "foieGras",
        silent = !ifelse(control$verbose == 2, TRUE, FALSE),
        inner.control = inner.control
      )

    obj$env$tracemgc <- ifelse(control$verbose == 2, TRUE, FALSE)

    ## add par values to trace if control$verbose = TRUE
    myfn <- function(x) {
      cat("\r", "pars:  ", round(x, 5), "     ")
      flush.console()
      obj$fn(x)
    }
    
    ## Set parameter bounds - most are -Inf, Inf
    L = c(l_sigma=c(-Inf,-Inf),
          l_rho_p=-7,
          l_D=-Inf,
          l_psi=-Inf,
          l_tau=c(-Inf,-Inf),
          l_rho_o=-7) ## using 2 / (1 + exp(-x)) - 1 transformation, this gives rho_o = -0.999, 0.999
    U = c(l_sigma=c(Inf,Inf),
          l_rho_p=7,
          l_D=Inf,
          l_psi=Inf,
          l_tau=c(Inf,Inf),
          l_rho_o=7)
    
    if(any(!is.null(control$lower))) {
      L[which(names(L) %in% names(control$lower))] <- unlist(control$lower)
    }
    if(any(!is.null(control$upper))) {
      U[which(names(U) %in% names(control$upper))] <- unlist(control$upper)
    } 
 
    names(L)[c(1:2,6:7)] <- c("l_sigma", "l_sigma", "l_tau", "l_tau")
    names(U)[c(1:2,6:7)] <- c("l_sigma", "l_sigma", "l_tau", "l_tau")

    # Remove inactive parameters from bounds
    L <- L[!names(L) %in% names(map)]
    U <- U[!names(U) %in% names(map)]
    if(model == "rw") {
      L <- L[names(L) != "l_D"] ## not sure why but l_D in automap is causing an error in MakeADFun, so remove here to get correct param bounds
      U <- U[names(U) != "l_D"]
    } else if(model == "crw") {
      L <- L[!names(L) %in% c("l_sigma","l_sigma","l_rho_p")] 
      U <- U[!names(U) %in% c("l_sigma","l_sigma","l_rho_p")]
    }

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
    
    FreeADFun(obj)
    
    ## if error then exit with limited output to aid debugging
    ## check if pdHess is FALSE at end and return warning
    rep <- try(sdreport(obj, skip.delta.method = !control$se)) #
   
    options(warn = oldw) ## turn warnings back on
    
    if (!inherits(opt, "try-error") & !inherits(rep, "try-error")) {

      ## Parameters, states and the fitted values
      fxd <- summary(rep, "report")
      fxd <- fxd[which(!rownames(fxd) %in% str_split(names(map), "_", simplify=TRUE)[,2]), ]
      if("sigma" %in% row.names(fxd)) {
        row.names(fxd)[which(row.names(fxd) == "sigma")] <- c("sigma_x","sigma_y")
      } 
      if("tau" %in% row.names(fxd)) {
        row.names(fxd)[which(row.names(fxd) == "tau")] <- c("tau_x","tau_y")
      }

      switch(model,
             rw = {
               tmp <- summary(rep, "random")
               rdm <- cbind(tmp[seq(1, nrow(tmp), by = 2), ],
                            tmp[seq(2, nrow(tmp), by = 2), ]
               ) %>%
                 data.frame() %>%
                 select(1,3,2,4) %>%
                 rename(x = "Estimate",
                        y = "Estimate.1",
                        x.se = "Std..Error",
                        y.se = "Std..Error.1")

               rdm <- rdm %>%
                 mutate(
                   id = unique(d.all$id),
                   date = d.all$date,
                   isd = d.all$isd
                 )
               if(scale) {
                 rdm <- rdm %>%
                   mutate(x = x * sd(d.all$x, na.rm = TRUE) + mean(d.all$x, na.rm = TRUE),
                          y = y * sd(d.all$y, na.rm = TRUE) + mean(d.all$y, na.rm = TRUE))
                 }
                rdm <- rdm %>%
                  select(id, date, x, y, x.se, y.se, isd)

             },
             crw = {
               tmp <- summary(rep, "random")
               loc <- tmp[rownames(tmp) == "mu",]
               vel <- tmp[rownames(tmp) == "v",]
               ss <- fxd[which(row.names(fxd) == "sv"), ] ## speeds
               
               loc <-
                 cbind(loc[seq(1, dim(loc)[1], by = 2),],
                       loc[seq(2, dim(loc)[1], by = 2),]) %>%
                 as.data.frame(., row.names = 1:nrow(.))
               names(loc) <- c("x", "x.se", "y", "y.se")
                         
               vel <-
                 cbind(vel[seq(1, dim(vel)[1], by = 2),],
                       vel[seq(2, dim(vel)[1], by = 2),]) %>%
                 as.data.frame(., row.names = 1:nrow(.))
               names(vel) <- c("u", "u.se", "v", "v.se")
               vel <- vel %>%
                 mutate(s = ss[, 1], s.se = ss[, 2])
               
               rdm <- bind_cols(loc, vel) %>%
                 mutate(
                   id = unique(d.all$id),
                   date = d.all$date,
                   isd = d.all$isd
                 ) 
               if(scale) {
                 rdm <- rdm %>%
                   mutate(x = x  * sd(d.all$x, na.rm = TRUE) + mean(d.all$x, na.rm = TRUE),
                          y = y  * sd(d.all$y, na.rm = TRUE) + mean(d.all$y, na.rm = TRUE))
               }
               rdm <- rdm %>%
                 select(id, date, x, y, x.se, y.se, u, v, u.se, v.se, s, s.se, isd)
             })
      
      ## coerce x,y back to sf object
      rdm <- rdm %>%
        st_as_sf(coords = c("x","y"), remove = FALSE) %>%
        st_set_crs(prj)

      switch(model,
             rw = {
               rdm <- rdm %>% select(id, date, x.se, y.se, isd)

             },
             crw = {
               rdm <- rdm %>%
                 select(id, date, x.se, y.se, u, v, u.se, v.se, s, s.se, isd)
             })

      ## Fitted values (estimated locations at observation times)
      fv <- subset(rdm, isd) %>%
        select(-isd)

      ## Predicted values (estimated locations at regular time intervals, defined by `ts`)
      if(all(!is.na(time.step))) {
      pv <- subset(rdm, !isd) %>%
        select(-isd)
      } else {
        pv <- NULL
      }
      
      if (control$optim == "nlminb") {
        aic <- 2 * length(opt[["par"]]) + 2 * opt[["objective"]]
      } else if (control$optim == "optim") {
        aic <- 2 * length(opt[["par"]]) + 2 * opt[["value"]]
      }
      ## drop sv's from fxd
      if(model == "crw") {
        fxd <- fxd[which(row.names(fxd) != "sv"), ]
      }
      
      out <- list(
        call = call,
        predicted = pv,
        fitted = fv,
        par = fxd,
        data = x,
        isd = d.all$isd,
        inits = parameters,
        pm = model,
        ts = time.step,
        opt = opt,
        tmb = obj,
        rep = rep,
        aic = aic,
        optimiser = control$optim,
        time = proc.time() - st
      )
      if(!rep$pdHess & any(!is.na(x$smaj), !is.na(x$smin), !is.na(x$eor))) warning("Hessian was not positive-definite so some standard errors could not be calculated. Try simplifying the model by adding the following argument:
             map = list(psi = factor(NA))", call. = FALSE)
    } else if (rep$pdHess & all(is.na(x$smaj), is.na(x$smin), is.na(x$eor))){
      warning("Hessian was not positive-definite so some standard errors could not be calculated.", 
              call. = FALSE)
    } else {
      
      ## if optimiser fails
      out <- list(
        call = call,
        data = x,
        inits = parameters,
        pm = model,
        ts = time.step,
        tmb = obj,
        errmsg = opt
      )

      if(model == "crw" & any(!is.na(x$smaj), !is.na(x$smin), !is.na(x$eor))) {
        warning("The optimiser failed. Try simplifying the model with the following argument: 
                                 map = list(psi = factor(NA))", call. = FALSE)
      } else if (all(is.na(x$smaj), is.na(x$smin), is.na(x$eor))){
        warning("The optimiser failed. Try simplifying the model with the following argument: 
                                 map = list(rho_o = factor(NA))", call. = FALSE)
      } else {
        warning("The optimiser failed. You could try using a different time.step", call. = FALSE)
      }
    }
    
    class(out) <- append("ssm", class(out))
    out
  }
