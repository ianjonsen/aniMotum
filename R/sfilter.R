##' Filter Argos LS or KF track data
##'
##' Internal function
##'
##' @title fit a C-T SSM to individual track data
##'
##' @useDynLib foieGras
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom stats approx cov sd predict nlminb optim
##' @importFrom dplyr mutate filter select full_join arrange lag bind_cols %>%
##' @importFrom tibble as_tibble
##' @importFrom sf st_crs st_coordinates st_geometry<- st_as_sf st_set_crs
##' @importFrom stats na.omit
##'
##' @export

sfilter <-
  function(x,
           model = c("rw", "crw"),
           time.step,
           parameters = NULL,
           fit.to.subset = TRUE,
           optim = c("nlminb", "optim"),
           verbose = FALSE,
           inner.control = NULL) {
    st <- proc.time()
    call <- match.call()
    optim <- match.arg(optim)
    model <- match.arg(model)

    if(is.null(time.step)) {
      print("\nNo time.step specified, using 6 h as a default time step")
      time.step <- 6
    } else if(length(time.step) > 1 & !is.data.frame(time.step)) {
        stop("\ntime.step must be a data.frame with id's when specifying multiple prediction times")
    } else if(length(time.step) > 1 & is.data.frame(time.step)) {
        if(sum(!names(time.step) %in% c("id","date")) > 0) stop("\n time.step names must be `id` and `date`")
    }

    ## drop any records flagged to be ignored, if fit.to.subset is TRUE
    ## add is.data flag (distinquish obs from reg states)
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

    ## merge data and interpolation times
    d.all <- full_join(d, ts, by = "date") %>%
      arrange(date) %>%
      mutate(isd = ifelse(is.na(isd), FALSE, isd)) %>%
      mutate(id = ifelse(is.na(id), na.omit(unique(id))[1], id))

    ## calc delta times in hours for observations & interpolation points (states)
    dt <- difftime(d.all$date, lag(d.all$date), units = "hours") %>%
      as.numeric() / 24
    dt[1] <- 0.000001 # - 0 causes numerical issues in CRW model

    ## use approx & MA filter to obtain state initial values
    x.init <-
      approx(x = select(d, date, x),
             xout = d.all$date,
             rule = 2)$y
    x.init <-
      stats::filter(x.init, rep(1, 10) / 10) %>% as.numeric()
    x.init[1:4] <- x.init[5]
    x.init[which(is.na(x.init))] <-
      x.init[which(is.na(x.init))[1] - 1]

    y.init <-
      approx(x = select(d, date, y),
             xout = d.all$date,
             rule = 2)$y
    y.init <-
      stats::filter(y.init, rep(1, 10) / 10) %>% as.numeric()
    y.init[1:4] <- y.init[5]
    y.init[which(is.na(y.init))] <-
      y.init[which(is.na(y.init))[1] - 1]
    xs <- cbind(x.init, y.init)

    state0 <- c(xs[1,], 0 , 0)
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
        X = xs,
        logD = 10,
        mu = t(xs),
        v = t(xs) * 0,
        l_psi = 0,
        l_tau = c(0, 0),
        l_rho_o = 0
      )
    }

    ## calculate prop'n of obs that are LS-derived
    d <- d %>% mutate(obs.type = factor(obs.type, levels = c("LS","KF"), labels = c("LS","KF")))
    pls <- table(d$obs.type)["LS"] / nrow(d)
    map <- switch(model,
                  rw = {
                    if (pls == 1) {
                      list(logD = factor(NA),
                           l_psi = factor(NA),
                           mu = factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs)))),
                           v =  factor(rbind(rep(NA, nrow(xs)), rep(NA, nrow(xs))))
                           )
                    } else if (pls == 0) {
                      list(logD = factor(NA),
                           l_tau = factor(c(NA, NA)),
                           l_rho_o = factor(NA),
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
                    } else if (pls == 0) {
                      list(
                        l_sigma = factor(c(NA, NA)),
                        l_rho_p = factor(NA),
                        X = factor(cbind(rep(NA, nrow(xs)), rep(NA, nrow(xs)))),
                        l_tau = factor(c(NA, NA)),
                        l_rho_o = factor(NA)
                      )
                    } else if (pls > 0 & pls < 1) {
                      list(l_sigma = factor(c(NA, NA)),
                           l_rho_p = factor(NA),
                           X = factor(cbind(rep(NA, nrow(xs)), rep(NA, nrow(xs))))
                      )
                    }
                  })

    ## TMB - data list
    data <- list(
      Y = switch(
        model,
        rw = cbind(d.all$x, d.all$y),
        crw = rbind(d.all$x, d.all$y)
      ),
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
    myfn <- function(x) {
      print("pars:")
      print(x)
      obj$fn(x)
    }

    ## Minimize objective function
    opt <-
      suppressWarnings(switch(optim,
                              nlminb = try(nlminb(obj$par, obj$fn, obj$gr))
                              , #myfn #obj$fn
                              optim = try(do.call(
                                optim,
                                args = list(
                                  par = obj$par,
                                  fn = obj$fn,
                                  gr = obj$gr,
                                  method = "L-BFGS-B"
                                )
                              ))))

    ## if error then exit with limited output to aid debugging
    rep <- try(sdreport(obj))
    if (!inherits(opt, "try-error") & !inherits(rep, "try-error")) {

      ## Parameters, states and the fitted values
      fxd <- summary(rep, "report")

      switch(model,
             rw = {
               rdm <-
                 matrix(summary(rep, "random"),
                        nrow(d.all),
                        4,
                        dimnames = list(NULL, c("x", "y", "x.se", "y.se")))

               rdm <- as.data.frame(rdm) %>%
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
                 cbind(loc[seq(1, dim(loc)[1], by = 2),], loc[seq(2, dim(loc)[1], by =
                                                                    2),]) %>%
                 as_tibble() %>%
                 select(1, 3, 2, 4)
               names(loc) <- c("x", "y", "x.se", "y.se")
               vel <-
                 cbind(vel[seq(1, dim(vel)[1], by = 2),], vel[seq(2, dim(vel)[1], by =
                                                                    2),]) %>%
                 as_tibble() %>%
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
        data = x,
        inits = parameters,
        pm = model,
        ts = time.step,
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
        data = x,
        inits = parameters,
        pm = model,
        ts = time.step,
        tmb = obj,
        errmsg = opt
      )
    }

    class(out) <- append("foieGras", class(out))
    out
  }
