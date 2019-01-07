##' Filter Argos LS or KF track data
##'
##' Internal function
##'
##' @title fit continuous-time correlated random walk to Argos data
##'
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom stats approx cov sd predict nlminb optim
##' @importFrom dplyr mutate filter select full_join arrange lag %>% group_by bind_cols
##' @importFrom tibble as_tibble
##' @importFrom rgdal project
##'
##' @useDynLib ctcrw
##'
##' @export

sfilter <-
  function(d,
           time.step = 1,
           fit.to.subset = TRUE,
           parameters = NULL,
           optim = c("nlminb", "optim"),
           verbose = FALSE,
           inner.control = NULL
           ) {
    st <- proc.time()
    call <- match.call()
    optim <- match.arg(optim)

    if("smaj" %in% names(d)) data.class <- "KF"
    else {
      data.class <- "LS"
    }

    ## drop any records flagged to be ignored, if fit.to.subset is TRUE
    ## add is.data flag (distinquish obs from reg states)
    if(fit.to.subset) {
      dnew <- d %>%
        filter(.$keep) %>%
        mutate(isd = TRUE)
    } else {
    dnew <- d %>% mutate(isd = TRUE)
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
    dt[1] <- 0.000001 # Was 0 -- caused problems numerically...

    ## use approx & MA filter to obtain state initial values from obs times (dnew)
    x.init <- approx(x = select(dnew, date, x), xout = d.all$date, rule = 2)$y
    x.init <- stats::filter(x.init, rep(1,10)/10) %>% as.numeric()
    x.init[1:4] <- x.init[5]
    x.init[which(is.na(x.init))] <- x.init[which(is.na(x.init))[1]-1]

    y.init <- approx(x = select(dnew, date, y), xout = d.all$date, rule = 2)$y
    y.init <- stats::filter(y.init, rep(1,10)/10) %>% as.numeric()
    y.init[1:4] <- y.init[5]
    y.init[which(is.na(y.init))] <- y.init[which(is.na(y.init))[1]-1]
    xs <- cbind(x.init, y.init)

    state0 <- c(xs[1,],0,0)
    if (is.null(parameters)) {
      parameters <-
        list(
          l_psi = 1,
          logD = 10,
#          l_tau = runif(2), # dummy - gets ignored for KF
          l_tau = c(0,0),
          l_rho_o = 0,
          u = t(xs), # location states
          v = t(xs) * 0 # velocities
        )
    }

    ## TMB - data list
    fill <- rep(1, nrow(d.all))
    if(data.class == "KF") {
      data <-
        list(
          y = rbind(d.all$x, d.all$y),
          state0 = state0,
          del_t = dt,
          isd = as.integer(d.all$isd),
          obs_mod = 1,
          m = d.all$smin,
          M = d.all$smaj,
          c = d.all$eor,
          K = cbind(fill, fill)
        )
    } else if(data.class == "LS") {
      data <-
        list(
          y = rbind(d.all$x, d.all$y),
          state0 = state0,
          del_t = dt,
          isd = as.integer(d.all$isd),
          obs_mod = 0,
          m = fill,
          M = fill,
          c = fill,
          K = cbind(d.all$amf_x,d.all$amf_y)
        )
    } else stop("Data class not recognised")

    if(data.class == "KF") {
        map = list(l_tau = factor(c(NA,NA)),
                    l_rho_o = factor(NA))
    }
    else if(data.class == "LS") map = list(l_psi = factor(NA))

    ## TMB - create objective function
    obj <-
      MakeADFun(
        data,
        parameters,
        map = map,
        random = c("u","v"),
        DLL = "ctcrw",
        hessian = TRUE,
        silent = !verbose,
        inner.control = inner.control
    )
    # obj$env$inner.control$trace <- verbose
    # obj$env$tracemgc <- verbose

    ## Minimize objective function
    opt <-  suppressWarnings(switch(
        optim,
        nlminb = try(nlminb(obj$par, obj$fn, obj$gr)),
        optim = try(do.call(optim, args = list(par = obj$par, fn = obj$fn,
                                               gr = obj$gr, method = "L-BFGS-B")
                            ))
      ))

    ## Parameters, states and the fitted values
    rep <- sdreport(obj)
    fxd <- summary(rep, "report")
    if(data.class == "KF") {
      fxd <- fxd[c(1,5), ]
    } else if(data.class == "LS") {
      fxd <- fxd[1:4, ]
    }

    tmp <- summary(rep, "random")
    loc <- tmp[rownames(tmp) == "u",]
    vel <- tmp[rownames(tmp) == "v",]
    loc <- cbind(loc[seq(1,dim(loc)[1], by=2),], loc[seq(2,dim(loc)[1],by=2),]) %>%
      as_tibble() %>%
      select(1,3,2,4)
    names(loc) <- c("x","y","x.se","y.se")
    vel <- cbind(vel[seq(1,dim(vel)[1], by=2),], vel[seq(2,dim(vel)[1],by=2),]) %>%
      as_tibble() %>%
      select(1,3,2,4)
    names(vel) <- c("u","v","u.se","v.se")

    rdm <- bind_cols(loc,vel) %>%
      mutate(id = unique(d.all$id), date = d.all$date, isd = d.all$isd) %>%
      select(id, date, x, y, x.se, y.se, u, v, u.se, v.se, isd)

    ## reproject mercator x,y back to WGS84 longlat
    if(d$cntr[1] == 0) {
      prj <- "+proj=merc +lon_0=180 +datum=WGS84 +units=km +no_defs"
    } else {
      prj <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
    }

    rdm[, c("lon", "lat")] <- as_tibble(project(as.matrix(rdm[, c("x", "y")]), proj = prj, inv = TRUE))

    if(d$cntr[1] == 0) {
      rdm <- rdm %>%
        mutate(lon = wrap_lon(lon, 0))
    } else if(d$cntr[1] == 90) {
      rdm <- rdm %>%
        mutate(lon = lon + 180)
    }

    rdm <- rdm %>%
      select(id, date, lon, lat, x, y, x.se, y.se, u, v, u.se, v.se, isd)

    ## Fitted values (estimated locations at observation times)
    fd <- rdm %>%
      filter(isd) %>%
      select(-isd)

    ## Predicted values (estimated locations at regular time intervals, defined by `ts`)
    pd <- rdm %>%
      filter(!isd) %>%
      select(-isd)

    if (optim == "nlminb") {
      aic <- 2 * length(opt[["par"]]) + 2 * opt[["objective"]]
    } else if (optim == "optim") {
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
    # class(out) <- append("ctrwSSM", class(out))
    out
  }
