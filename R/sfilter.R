##' @title fit the state-space model to \code{prefilter}-ed data
##'
##' @description generates initial values for model parameters and unobserved states;
##' structures data and initial values for C++ \code{TMB} template;
##' fits state-space model; minimises the joint log-likelihood via the selected
##' optimizer (\code{nlminb} or \code{optim}); structures and passes output
##' object to \code{fit_ssm}
##'
##' @details called by \code{fit_ssm}. \code{sfilter} can only fit to an
##' individual track, use \code{fit_ssm} to fit to multiple tracks (see ?fit_ssm).
##'
##' @param x Argos data passed through prefilter()
##' @param model specify which SSM is to be fit: "rw" or "crw"
##' @param time.step the regular time interval, in hours, to predict to.
##' Alternatively, a vector of prediction times, possibly not regular, must be
##' specified as a data.frame with id and POSIXt dates.
##' @param map a named list of parameters as factors that are to be fixed during estimation, e.g., list(psi = factor(NA))
##' @param parameters a list of initial values for all model parameters and
##' unobserved states, default is to let sfilter specify these. Only play with
##' this if you know what you are doing...
##' @param fit.to.subset fit the SSM to the data subset determined by prefilter
##' (default is TRUE)
##' @param optim numerical optimizer to be used ("nlminb" or "optim")
##' @param verbose report progress during minimization
##' @param control list of control parameters for the outer optimization (type ?nlminb or ?optim for details)
##' @param inner.control list of control settings for the inner optimization
##' (see ?TMB::MakeADFUN for additional details)
##' @param lpsi lower bound for the psi parameter
##'
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom stats approx cov sd predict nlminb optim na.omit
##' @importFrom dplyr mutate filter select full_join arrange lag bind_cols "%>%"
##' @importFrom tibble as_tibble
##' @importFrom sf st_crs st_coordinates st_geometry<- st_as_sf st_set_crs
##'
##' @examples
##' data(ellie)
##' pf <- prefilter(ellie, vmax=4, ang=c(15,25), min.dt=120)
##' out <- sfilter(pf, model="rw", time.step=24)
##'
##' @export

sfilter <-
  function(x,
           model = c("rw", "crw"),
           time.step = 6,
           parameters = NULL,
           map = NULL,
           fit.to.subset = TRUE,
           optim = c("nlminb", "optim"),
           verbose = FALSE,
           control = NULL,
           inner.control = NULL,
           lpsi=-10) {

    st <- proc.time()
    call <- match.call()
    optim <- match.arg(optim)
    model <- match.arg(model)

    ## populate control list if any parameters specified...
    if (length(control)) {
      nms <- names(control)
      if (!is.list(control) || is.null(nms))
        stop("'control' argument must be a named list")
    }

    if(is.null(time.step)) {
      print("\nNo time.step specified, using 6 h as a default time step")
      time.step <- 6
    } else if(length(time.step) > 1 & !is.data.frame(time.step)) {
        stop("\ntime.step must be a data.frame with id's when specifying multiple prediction times")
    } else if(length(time.step) > 1 & is.data.frame(time.step)) {
        if(sum(!names(time.step) %in% c("id","date")) > 0) stop("\n time.step names must be `id` and `date`")
    }

    ## drop any records flagged to be ignored, if fit.to.subset is TRUE
    if(fit.to.subset) xx <- x %>% filter(keep)
    else xx <- x

    prj <- st_crs(xx)
    loc <- as.data.frame(st_coordinates(xx))
    names(loc) <- c("x","y")
    st_geometry(xx) <- NULL
    d <- cbind(xx, loc) %>%
      mutate(isd = TRUE)

    if (length(time.step) == 1) {
      ## Interpolation times - assume on time.step-multiple of the hour
      tsp <- time.step * 3600
      tms <- (as.numeric(d$date) - as.numeric(d$date[1])) / tsp
      index <- floor(tms)
      ts <-
        data.frame(date = seq(
          trunc(d$date[1], "hour"),
          by = tsp,
          length.out = max(index) + 2
        ))
    } else {
      ts <- time.step %>%
        filter(id == unique(d$id)) %>%
        select(date)
    }

    ## add 1 s to observation time(s) that exactly match prediction time(s) & throw a warning
    if(sum(d$date %in% ts$date) > 0) {
      o.times <- which(d$date %in% ts$date)
      d[o.times, "date"] <- d[o.times, "date"] + 1

    }

    ## merge data and interpolation times
    ## add is.data flag (distinquish obs from reg states)
    d.all <- full_join(d, ts, by = "date") %>%
      arrange(date) %>%
      mutate(isd = ifelse(is.na(isd), FALSE, isd)) %>%
      mutate(id = ifelse(is.na(id), na.omit(unique(id))[1], id))

    ## calc delta times in hours for observations & interpolation points (states)
    dt <- difftime(d.all$date, lag(d.all$date), units = "hours") %>%
      as.numeric() / 24
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
    
    xs <- cbind(x.init, y.init)

    state0 <- c(xs[1,1], xs[1,2], 0 , 0)
    attributes(state0) <- NULL

    if (is.null(parameters)) {
      ## Estimate stochastic innovations
      es <- xs[-1,] - xs[-nrow(xs),]

      ## Estimate components of variance
      V <- cov(es)
      sigma <- sqrt(diag(V))
      rho <- V[1, 2] / prod(sigma)

      parameters <- list(
        l_sigma = log(pmax(1e-08, sigma)),
        l_rho_p = log((1 + rho) / (1 - rho)),
        X = t(xs),
        logD = 10,
        mu = t(xs),
        v = t(xs) * 0,
        l_psi = 0,
        l_tau = c(0, 0),
        l_rho_o = 0
      )
    }

    ## calculate prop'n of obs that are LS-derived
    d <- d %>% mutate(obs.type = factor(obs.type, levels = c("LS","KF","GL"), labels = c("LS","KF","GL")))
    pls <- table(d$obs.type)["LS"] / nrow(d)
    automap <- switch(model,
                  rw = {
                    if (pls == 1) {
                      list(l_psi = factor(NA),
                           logD = factor(NA),
                           mu = factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs)))),
                           v =  factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs))))
                           )
                    } else if (pls == 0 & unique(d$obs.type) == "KF") {
                      list(l_tau = factor(c(NA, NA)),
                           l_rho_o = factor(NA),
                           logD = factor(NA),
                           mu = factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs)))),
                           v =  factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs))))
                           )
                    } else if(pls == 0 & unique(d$obs.type == "GL")) {
                      list(l_tau = factor(c(NA, NA)),
                           l_psi = factor(NA),
                           logD = factor(NA),
                           mu = factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs)))),
                           v =  factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs))))
                           )
                    } else if (pls > 0 & pls < 1) {
                      list(logD = factor(NA),
                           mu = factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs)))),
                           v =  factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs))))
                           )
                    }
                  },
                  crw = {
                    if (pls == 1) {
                      list(
                        l_sigma = factor(c(NA, NA)),
                        l_rho_p = factor(NA),
                        X = factor(cbind(rep(NA, nrow(xs)), rep(NA, nrow(xs)))),
                        l_psi = factor(NA)
                      )
                    } else if (pls == 0 & unique(d$obs.type) == "KF") {
                      list(
                        l_sigma = factor(c(NA, NA)),
                        l_rho_p = factor(NA),
                        X = factor(cbind(rep(NA, nrow(xs)), rep(NA, nrow(xs)))),
                        l_tau = factor(c(NA, NA)),
                        l_rho_o = factor(NA)
                      )
                    } else if (pls == 0 & unique(d$obs.type) == "GL") {
                      list(
                        l_sigma = factor(c(NA, NA)),
                        l_rho_p = factor(NA),
                        X = factor(cbind(rep(NA, nrow(xs)), rep(NA, nrow(xs)))),
                        l_tau = factor(c(NA, NA)),
                        l_psi = factor(NA)
                      )
                    }
                    else if (pls > 0 & pls < 1) {
                      list(l_sigma = factor(c(NA, NA)),
                           l_rho_p = factor(NA),
                           X = factor(cbind(rep(NA, nrow(xs)), rep(NA, nrow(xs))))
                      )
                    }
                  })

    if(!is.null(map)) {
      names(map) <- paste0("l_", names(map))
      map <- append(automap, map, after = 0)
    } else {
      map <- automap
    }

    ## TMB - data list
    obs_mod <- ifelse(d.all$obs.type == "LS", 0, 
                      ifelse(d.all$obs.type == "KF", 1, 2)
                      )
    
    data <- list(
      model_name = "ssm",
      Y = switch(
        model,
        rw = rbind(d.all$x, d.all$y),
        crw = rbind(d.all$x, d.all$y)
      ),
      state0 = state0,
      dt = dt,
      isd = as.integer(d.all$isd),
      proc_mod = ifelse(model == "rw", 0, 1),
      obs_mod = obs_mod,
      m = d.all$smin,
      M = d.all$smaj,
      c = d.all$eor,
      K = cbind(d.all$emf.x, d.all$emf.y),
      GLerr = cbind(d.all$lonerr, d.all$laterr)
    )

    ## TMB - create objective function
    if (is.null(inner.control)) {
      inner.control <- list(smartsearch = TRUE)
    }
    rnd <- switch(model, rw = "X", crw = c("mu", "v"))
    obj <-
      MakeADFun(
        data,
        parameters,
        map = map,
        random = rnd,
        hessian = FALSE,
        method = "L-BFGS-B",
        DLL = "foieGras",
        silent = !verbose,
        inner.control = inner.control
      )
    #    newtonOption(obj, trace = verbose, smartsearch = TRUE)
    #    obj$env$inner.control$trace <- verbose
    #    obj$env$inner.control$smartsearch <- FALSE
    #    obj$env$inner.control$maxit <- 1
    obj$env$tracemgc <- verbose

    ## add par values to trace if verbose = TRUE
    myfn <- function(x) {
      print("pars:")
      print(x)
      obj$fn(x)
    }

    ## Set parameter bounds - most are -Inf, Inf
    L = c(l_sigma=c(-Inf,-Inf),
          l_rho_p=-Inf,
          logD=-Inf,
          l_psi=lpsi,
          l_tau=c(-Inf,-Inf),
          l_rho_o=-Inf)
    U = c(l_sigma=c(Inf,Inf),
          l_rho_p=Inf,
          logD=Inf,
          l_psi=Inf,
          l_tau=c(Inf,Inf),
          l_rho_o=Inf)
    names(L)[c(1:2,6:7)] <- c("l_sigma", "l_sigma", "l_tau", "l_tau")
    names(U)[c(1:2,6:7)] <- c("l_sigma", "l_sigma", "l_tau", "l_tau")

    # Remove inactive parameters from bounds
    L <- L[!names(L) %in% names(map)]
    U <- U[!names(U) %in% names(map)]

    ## Minimize objective function
    opt <-
      suppressWarnings(switch(optim,
                              nlminb = try(nlminb(obj$par,
                                                  obj$fn,
                                                  obj$gr,
                                                  control = control,
                                                  lower = L,
                                                  upper = U
                              ))
                              , #myfn #obj$fn
                              optim = try(do.call(
                                optim,
                                args = list(
                                  par = obj$par,
                                  fn = obj$fn,
                                  gr = obj$gr,
                                  method = "L-BFGS-B",
                                  control = control,
                                  lower = L,
                                  upper = U
                                )
                              ))))

    ## if error then exit with limited output to aid debugging
    rep <- suppressWarnings(try(sdreport(obj)))
    if (!inherits(opt, "try-error") & !inherits(rep, "try-error")) {

      ## Parameters, states and the fitted values
      fxd <- summary(rep, "report")

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
                 ) %>%
                 select(id, date, x, y, x.se, y.se, isd)

             },
             crw = {
               tmp <- summary(rep, "random")
               loc <- tmp[rownames(tmp) == "mu",]
               vel <- tmp[rownames(tmp) == "v",]
               loc <-
                 cbind(loc[seq(1, dim(loc)[1], by = 2),],
                       loc[seq(2, dim(loc)[1], by = 2),]) %>%
                 data.frame() %>%
                 select(1, 3, 2, 4)
               names(loc) <- c("x", "y", "x.se", "y.se")
               vel <-
                 cbind(vel[seq(1, dim(vel)[1], by = 2),],
                       vel[seq(2, dim(vel)[1], by = 2),]) %>%
                 data.frame() %>%
                 select(1, 3, 2, 4)
               names(vel) <- c("u", "v", "u.se", "v.se")

               rdm <- bind_cols(loc, vel) %>%
                 mutate(
                   id = unique(d.all$id),
                   date = d.all$date,
                   isd = d.all$isd
                 ) %>%
                 select(id, date, x, y, x.se, y.se, u, v, u.se, v.se, isd)
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
                 select(id, date, x.se, y.se, u, v, u.se, v.se, isd)
             })

      ## Fitted values (estimated locations at observation times)
      fv <- rdm %>%
        filter(isd) %>%
        select(-isd)

      ## Predicted values (estimated locations at regular time intervals, defined by `ts`)
      pv <- rdm %>%
        filter(!isd) %>%
        select(-isd)

      if (optim == "nlminb") {
        aic <- 2 * length(opt[["par"]]) + 2 * opt[["objective"]]
      } else if (optim == "optim") {
        aic <- 2 * length(opt[["par"]]) + 2 * opt[["value"]]
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
        optimiser = optim,
        time = proc.time() - st
      )
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
    }
    class(out) <- append("ssm", class(out))
    out
  }
