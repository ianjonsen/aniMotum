##' Filter Argos LS or KF track data
##'
##' Internal function
##'
##' @title fit a C-T SSM to individual track data
##'
##' @useDynLib foieGras
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom stats approx cov sd predict nlminb optim
##' @importFrom dplyr mutate filter select full_join arrange lag %>%
##' @importFrom tibble as_tibble
##' @importFrom rgdal project
##'
##' @export

sfilter <-
  function(d,
           model = c("rw","crw"),
           time.step = 1,
           fit.to.subset = TRUE,
           parameters = NULL,
           optim = c("nlminb","optim"),
           verbose = FALSE,
           inner.control = NULL
           ) {

    st <- proc.time()
    call <- match.call()
    optim <- match.arg(optim)
    model <- match.arg(model)

    ## drop any records flagged to be ignored, if fit.to.subset is TRUE
    ## add is.data flag (distinquish obs from reg states)
    if(fit.to.subset) {
      dnew <- d %>%
        filter(.$keep) %>%
        mutate(isd = TRUE)
    } else {
      dnew <- d %>%
        mutate(isd = TRUE)
    }

    if (length(time.step) == 1) {
      ## Interpolation times - assume on time.step-multiple of the hour
      tsp <- time.step * 3600
      tms <- (as.numeric(d$date) - as.numeric(d$date[1])) / tsp
      index <- floor(tms)
      time.step <-
        data.frame(date = seq(
          trunc(d$date[1], "hour"),
          by = tsp,
          length.out = max(index) + 2
        ))
    } else {
      time.step <- time.step %>%
        filter(id == unique(d$id)) %>%
        select(date)
    }

    ## merge data and interpolation times
    d.all <- full_join(dnew, time.step, by = "date") %>%
      arrange(date) %>%
      mutate(isd = ifelse(is.na(isd), FALSE, isd)) %>%
      mutate(id = ifelse(is.na(id), na.omit(unique(id))[1], id))

    ## calc delta times in hours for observations & interpolation points (states)
    dt <- difftime(d.all$date, lag(d.all$date), units = "hours") %>%
      as.numeric() / 24
    dt[1] <- 0.00000001 # - 0 causes numerical issues in CRW model

    ## use approx & MA filter to obtain state initial values
        x.init <- approx(x = select(dnew, date, x), xout = d.all$date, rule = 2)$y
        x.init <- stats::filter(x.init, rep(1,10)/10) %>% as.numeric()
        x.init[1:4] <- x.init[5]
        x.init[which(is.na(x.init))] <- x.init[which(is.na(x.init))[1]-1]

        y.init <- approx(x = select(dnew, date, y), xout = d.all$date, rule = 2)$y
        y.init <- stats::filter(y.init, rep(1,10)/10) %>% as.numeric()
        y.init[1:4] <- y.init[5]
        y.init[which(is.na(y.init))] <- y.init[which(is.na(y.init))[1]-1]
        xs <- cbind(x.init, y.init)

    state0 <- c(xs[1,], 0 ,0)
    if (is.null(parameters)) {
      ## Estimate stochastic innovations
      es <- xs[-1,] - xs[-nrow(xs),]

      ## Estimate components of variance
      V <- cov(es)
      sigma <- sqrt(diag(V))
      rho <- V[1, 2] / prod(sigma)

      parameters <-
        switch(model,
               rw = {
                 list(
                      l_sigma = log(pmax(1e-08, sigma)),
                      l_rho_p = log((1 + rho) / (1 - rho)),
                      X = xs,                                   # location states in RW
                      l_psi = 0,
                      l_tau = c(0,0),
                      l_rho_o = 0
                      )
               },
               crw = {
                 list(
                      logD = 10,      # 1-d Diffusion term in CRW
                      mu = t(xs),      # location states in CRW
                      v = t(xs) * 0,   # velocities in CRW
                      l_psi = 0,
                      l_tau = c(0,0),
                      l_rho_o = 0
                      )
               }
        )
    }

    ## TMB - data list
    data <- list(
      Y = cbind(d.all$x, d.all$y),
      state0 = state0,
      dt = dt,
      isd = as.integer(d.all$isd),
      proc_mod = switch(model, rw = 0, crw = 1),
      obs_mod = ifelse(d.all$obs.type == "LS", 0, 1),
      m = d.all$smin,
      M = d.all$smaj,
      c = d.all$eor,
      K = cbind(d.all$amf_x, d.all$amf_y)
    )

    browser()
## TMB - create objective function
    if(is.null(inner.control)) {
      inner.control <- list(smartsearch = TRUE, maxit = 1000)
    }
    rnd <- switch(model, rw ="X", crw = c("u","v"))
    obj <-
      MakeADFun(
        data,
        parameters,
#        map = map,
        random = rnd,
        DLL = "foieGras",
        hessian = TRUE,
        silent = !verbose,
        inner.control = inner.control
      )
#    newtonOption(obj, trace = verbose, smartsearch = TRUE)
#    obj$env$inner.control$trace <- verbose
#    obj$env$inner.control$smartsearch <- FALSE
#    obj$env$inner.control$maxit <- 1
    obj$env$tracemgc <- verbose

## add par values to trace if verbose = TRUE
    myfn <- function(x){print("pars:"); print(x); obj$fn(x)}

    browser()

    ## Minimize objective function
    opt <-
      suppressWarnings(switch(
        optim,
        nlminb = try(nlminb(obj$par, obj$fn, obj$gr)), #myfn #obj$fn
        optim = try(do.call(optim, args = list(par = obj$par, fn = obj$fn, gr = obj$gr, method = "L-BFGS-B"))) #myfn
      ))

  ## if error then exit with limited output to aid debugging
  rep <- try(sdreport(obj), silent = TRUE)
  if (class(opt) != "try-error" & class(rep) != "try-error") {
    ## Parameters, states and the fitted values

    fxd <- summary(rep, "report")

    if (data.class == "KF" & psi == 0) {
      fxd <- fxd[c(1:3), ]
    } else if (data.class == "KF" & psi == 1) {
      fxd <- fxd[c(1:3,7),]
    } else if (data.class == "LS") {
      fxd <- fxd[1:6,]
    }

    rdm <-
      matrix(summary(rep, "random"),
             nrow(d.all),
             4,
             dimnames = list(NULL, c("x", "y", "x.se", "y.se")))

    ## Fitted values (estimated locations at observation times)
    fd <- as.data.frame(rdm) %>%
      mutate(id = unique(d.all$id),
             date = d.all$date,
             isd = d.all$isd)

    ## reproject mercator x,y back to WGS84 longlat
    if(d$cntr[1] == 0) {
      prj <- "+proj=merc +lon_0=180 +datum=WGS84 +units=km +no_defs"
    } else {
      prj <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
    }

    fd[, c("lon", "lat")] <- as_tibble(project(as.matrix(fd[, c("x", "y")]), proj = prj, inv = TRUE))

    if(d$cntr[1] == 0) {
      fd <- fd %>%
        mutate(lon = wrap_lon(lon, 0))
    } else if(d$cntr[1] == 90) {
      fd <- fd %>%
        mutate(lon = lon + 180)
    }

    fd <- fd %>%
      select(id, date, lon, lat, x, y, x.se, y.se, isd) %>%
      filter(isd) %>%
      select(-isd) %>%
      as_tibble()

    ## Predicted values (estimated locations at regular time intervals, defined by `ts`)
    pd <- as.data.frame(rdm) %>%
      mutate(id = unique(d.all$id),
             date = d.all$date,
             isd = d.all$isd)
    ## reproject mercator x,y back to WGS84 longlat
    pd[, c("lon", "lat")] <- as_tibble(project(as.matrix(pd[, c("x", "y")]), proj = prj, inv = TRUE))

    if(d$cntr[1] == 0) {
      pd <- pd %>%
        mutate(lon = wrap_lon(lon, 0))
    } else if(d$cntr[1] == 90) {
      pd <- pd %>%
        mutate(lon = lon + 180)
    }

    pd <- pd %>%
      select(id, date, lon, lat, x, y, x.se, y.se, isd) %>%
      filter(!isd) %>%
      select(-isd) %>%
      as_tibble()

    if (optim == "nlminb") {
      aic <- 2 * length(opt[["par"]]) + 2 * opt[["objective"]]
    } else {
      aic <- 2 * length(opt[["par"]]) + 2 * opt[["value"]]
    }
    out <- list(
      call = call,
      predicted = pd,
      fitted = fd,
      par = fxd,
      data = select(d, -cntr),
      lon.wrapped = ifelse(d$cntr[1]==0, TRUE, FALSE),
      inits = parameters,
      mmod = data.class,
      opt = opt,
      tmb = obj,
      rep = rep,
      aic = aic,
      time = proc.time() - st
    )
  } else {
    ## if optimiser fails
    out <- list(
      call = call,
      data = d,
      inits = parameters,
      mmod = data.class,
      tmb = obj,
      errmsg = opt
    )
  }

    out
  }
