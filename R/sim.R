##' simulate Argos error classes for LS data 
##' 
##' @description uses data from: 1) all IMOS reprocessed KF data (2012 - 2015); 
##' 2) LESE, SESE, HBTU KF data in Jonsen et al. 2020 Move Ecol to specify
##' Argos location quality class proportions
##' @param N number of location class values to be randomly sampled
##' 
##' @keywords internal

argos_lc <- function(N) {
  factor(
    sample(
      c(3, 2, 1, 0, "A", "B"),
      N,
      replace = TRUE,
      prob = c(0.0444, 0.0320, 0.0514, 0.0746, 0.2858, 0.5119) 
    ),
    levels = c(3, 2, 1, 0, "A", "B"),
    ordered = TRUE
  )
  
}

##' simulate Argos ellipse params & sample errors from multivariate normal
##' 
##' @description uses internal extdata/error_ellipse.RDS params ordered by lc 
##' to sample smaj, smin from log-normals & eor from a von Mises, then convert
##' ellipse params to covariance matrix and sample x,y errors from multivariate
##' normal. Returns both ellipse params and x,y errors
##' 
##' @param lc Argos location classes
##' 
##' @importFrom CircStats rvm
##' @importFrom stats rlnorm
##' @importFrom mvtnorm rmvnorm
##' 
##' @keywords internal

ellp.par <- function(lc) {
  
  load(system.file("extdata", "ellps_tab.rda",
                        package = "foieGras"))
  n <- length(lc)
  
  eor.m <- ellps.tab$eor.mn[lc]
  eor.c <- ellps.tab$eor.conc[lc]
  
  ## sample error ellipse parameters
  ellps <- with(ellps.tab,
                data.frame(
                  smaj = rlnorm(n, smaj.mn[lc], smaj.sd[lc]),
                  smin = rlnorm(n, smin.mn[lc], smin.sd[lc]),
                  eor = sapply(1:n, function(i) {
                    rvm(1, eor.m[i], eor.c[i]) * 180 / pi
                  })
                ))
  
  ## convert ellipse params to covar matrix
  psi <- 1 # keep this here for possible later use
  s1 <- with(ellps, 
             (smaj / sqrt(2))^2 * sin(eor)^2 + 
               (smin * psi / sqrt(2))^2 * cos(eor)^2
  )
  s2 <- with(ellps,
             (smaj / sqrt(2))^2 * cos(eor)^2 +
               (smin * psi / sqrt(2))^2 * sin(eor)^2
  )
  s12 <- with(ellps,
              (0.5 * ((smaj / sqrt(2))^2 - (smin * psi / sqrt(2))^2)) * 
                cos(eor) * sin(eor)
  )
  
  ## return x,y errors + ellipse params
  errs <- lapply(1:n, function(i) {
    Sigma <- diag(2) * c(s1[i], s2[i])
    Sigma[!Sigma] <- s12[i]
    rmvnorm(1, c(0, 0), sigma = Sigma)
  }) %>%
    do.call(rbind, .) / 1000 # convert errors in m to km
  
  data.frame(x.err = errs[,1],
         y.err = errs[,2], 
         smaj = ellps$smaj,
         smin = ellps$smin,
         eor = ellps$eor
         ) 
}

##' @title simulate animal tracks
##'
##' @description simulate from the `rw`, `crw`, or `mpm` process models 
##' to generate a set of `x,y` (or `lon,lat`) coordinates with or without error 
##' from supplied input parameters. 
##' @param N number of time steps to simulate
##' @param start coordinates and datetime of start location for simulated track
##' @param model simulate from the `rw`, `crw` or `mpm` process models
##' @param vmax maximum travel rate (m/s) of simulated animal
##' @param sigma a vector of process error sd's for the `rw` model 
##' (ignored if `model != "rw"`)
##' @param rho_p correlation parameter for `rw` model process covariance matrix 
##' (ignored if `model != "rw"`)
##' @param D diffusion coefficient for `crw` model process covariance matrix 
##' (ignored if `model != "crw"`)
##' @param sigma_g random walk sd for time-varying move persistence parameter 
##' (ignored if `model != "mpm"`)
##' @param error indicates whether measurement error should mimic Argos 
##' Least-Squares (`ls`) or Argos Kalman Filter (`kf`)
##' @param tau vector of LS measurement error sd's (ignored if `error = "kf"`)
##' @param rho_o correlation parameter for LS covariance matrix 
##' (ignored if `error = "kf"`)
##' @param tdist distribution for simulating location times (`reg` generates locations 
##' at regular ts intervals, in h; `gamma` uses a gamma distribution to generate random 
##' time intervals)
##' @param ts time interval in h
##' @param tpar rate parameter for the gamma distributed times, shape is take to be
##' `ts * tpar` for a mean interval of approximately `ts` h 
##' (ignored if `tdist = "reg"`)
##' @param alpha transition probabilities switching model versions of 
##' `rw` or `crw` models. Probabilities are the transition matrix diagonals 
##' (ignored if sigma has length 2 or D has length 1)
##' 
##' @return a tibble is returned with columns that can include some or all of the 
##' following, depending on the arguments used
##'  * `date` time as POSIXct, tz = UTC (default)
##'  * `lc` Argos location class
##'  * `lon` longitude with error
##'  * `lat` latitude with error
##'  * `x` x in km from arbitrary origin without error
##'  * `y` y in km from arbitrary origin without error
##'  * `x.err` a random deviate drawn from Argos LS or KF error distribution
##'  * `y.err` a random deviate drawn from Argos LS or KF error distribution
##'  * `smaj` Argos error ellipse semi-major axis in m (if `error = "kf"`)
##'  * `smin` Argos error ellipse semi-minor axis in m (if `error = "kf"`)
##'  * `eor` Argos error ellipse orientation in degrees (if `error = "kf"`)
##'  * `u` velocity in x direction (if `model = "crw"`)
##'  * `v` velocity in y direction (if `model = "crw"`)
##'  * `b` behavioural state (if `model = "rw"` or `model = "crw"` and multiple process variances given, see examples)
##'  * `g` movement persistence - the autocorrelation between successive movements on the interval 0,1 (if `model = "mpm"`)
##' 
##' 
##' @examples 
##' tr <- sim(N = 200, model = "crw", D = 0.1, error = "kf", tdist = "reg", ts=12)
##' plot(tr, error = TRUE)
##' 
##' tr <- sim(N = 200, model = "rw", sigma = c(4,4,0.5,0.5), error = "ls", tdist = "reg")
##' plot(tr)
##' 
##' tr <- sim(N = 200, model = "crw", D = c(0.1, 0.05), error = "kf", tdist="reg")
##' plot(tr)
##' 
##' tr <- sim(N = 200, model = "mpm", sigma_g = 1.2, error = "ls", tau = c(2, 1.5), ts=12,
##' tdist = "gamma", tpar = 1.5)
##' plot(tr, error = TRUE, pal = "Cividis")
##' 
##' @importFrom tmvtnorm rtmvnorm
##' @importFrom mvtnorm rmvnorm
##' @importFrom tibble as_tibble
##' @importFrom sf st_coordinates st_as_sf st_transform st_geometry<-
##' @importFrom stats rgamma runif
##' @importFrom CircStats rvm
##' 
##' @export
##' @md

sim <- function(N = 100,
                start = list(c(0, 0), 
                             as.POSIXct(format(Sys.time(), tz = "UTC", usetz = TRUE))
                             ),
                model = c("rw", "crw", "mpm"),
                vmax = 4,
                sigma = c(4, 4),
                rho_p = 0,
                D = 0.05,
                sigma_g = 1.25,
                error = c("ls", "kf"),
                tau = c(1.5, 0.75),
                rho_o = 0,
                tdist = c("reg", "gamma"),
                ts = 6,
                tpar = 1.2,
                alpha = c(0.9, 0.8)) {
  
  
  ################
  ## Check args ##
  ################
  
  model <- match.arg(model)
  error <- match.arg(error)
  tdist <- match.arg(tdist)

  if(!model %in% c("rw","crw","mpm")) stop("model can only be 1 of `rw`, `crw`, or `mpm`")
  if(!error %in% c("ls","kf")) stop("error can only be 1 of `ls` or `kf`")
  if(!tdist %in% c("gamma","reg")) stop("tdist can only be 1 of `gamma` or `reg`")
  if(!all(inherits(start, "list"), length(start) == 2, inherits(start[[2]], "POSIXct")))
    stop("`start` must be a 2-element list, with coordinates as the 1st element and a POSIX datetime as the 2nd element")
  
  n.states <- 1
  if(length(sigma) > 2 & model == "rw") {
    n.states <- length(sigma)/2
    bm <- matrix(sigma, n.states, 2, byrow = TRUE)
    bm <- apply(bm,1,mean)
    beta <- bm / max(bm)

    ## ensure covar matrices are symmetric and non-NA
    if(length(rho_p) == 1) rho_p <- rep(rho_p, n.states)
  } else if(length(sigma == 2) & model == "rw") {
    beta <- 1
  }
  
  if(length(D) > 1 & model == "crw") {
    n.states <- length(D)
    beta <- D / max(D)
  } else if (length(D) == 1 & model == "crw") {
    beta <- 1
  }
  
  ###########################
  ## Simulate from scratch ##
  ###########################
    
    mu <- v <- matrix(NA, N, 2)
    mu[1, ] <- start[[1]]
    v[1, ] <- c(0,0)
    
    ## generate random time intervals (h)
    switch(tdist, 
           gamma = {
             # mean = ts * tpar 
             dt <- c(0, rgamma(N-1, shape = ts * tpar, rate = tpar))
           },
           reg = {
             dt <- c(0, rep(ts, N-1))
           })
    
    ## Define var-cov matrix for movement process
      Sigma <- lapply(1:n.states, function(i) {
        Sigma <- diag(2) * sigma[i:(i+1) + (i-1)] ^ 2
        switch(model,
               rw = {
                 Sigma[!Sigma] <- prod(sigma[i:(i+1) + (i-1)]) * rho_p[i]
               },
               crw = {
                 Sigma <- diag(2) * 2 * D[i]
               })
        Sigma
      })

    #################################
    ## Simulate behavioural states ##
    #################################
    if(n.states > 1) {
      ## set up transition matrix
      T <- diag(2) * alpha
      T[!T] <- rev(diag(1 - T))
      b <- rep(NA, N)
      ## initial state
      b[1] <- sample(1:n.states, size = 1, prob = rep(1, n.states) / n.states)
      ## sample subsequent states
      for (k in 2:N) {
        b[k] <- sample(1:n.states, size = 1, prob = T[b[k-1], ])
      }
    } else {
      b <- rep(1, N)
    }
      
    #######################
    ## Simulate movement ##
    #######################
    switch(model,
           crw = {
             for (i in 2:N) {
               v[i,] <- rtmvnorm(1, beta[b[i]] * v[i - 1,], 
                                 sigma = Sigma[[b[i]]] * dt[i], 
                                 lower = rep(-sqrt(vmax*vmax/2)*3.6,2), 
                                 upper = rep(sqrt(vmax*vmax/2)*3.6,2))
               mu[i,] <- mu[i - 1,] + v[i,] * dt[i]
             }
           },
           rw = {
             mu[2, ] <- rtmvnorm(1, beta[b[2]] * (mu[1, ] - mu[1, ]), 
                                 sigma = Sigma[[b[2]]] * dt[2]^2,
                                 lower = rep(-sqrt(vmax*vmax/2)*3.6,2),
                                 upper = rep(sqrt(vmax*vmax/2)*3.6,2)) + mu[1, ]
               
             for (i in 3:N) {
               dxy <- rtmvnorm(1, beta[b[i]] * (mu[i - 1,] - mu[i - 2, ]), 
                                 sigma = Sigma[[b[i]]] * dt[i]^2,
                               lower = rep(-sqrt(vmax*vmax/2)*3.6,2),
                               upper = rep(sqrt(vmax*vmax/2)*3.6,2))
               mu[i, ] <- mu[i - 1, ] + dxy
             }
           },
           mpm = {
             #dt.g <- dt / median(dt)
             
             lg <- rep(NA, N)
             lg[1] <- runif(1,-5, 5)
             sd <- sigma_g * dt
             for (i in 2:N) {
               ## use a truncated normal to simulate g in logit space:
               ##   suitable bounds keep RW wandering off at extremes
               lg[i] <- rtnorm(1, lg[i-1], sd[i], l = -8, u = 8)
             }
             g <- plogis(lg)
             mu[2,] <- rmvnorm(1, mu[1,], Sigma[[1]] * dt[2] ^ 2)
             for (i in 3:N) {
               dxy <- rtmvnorm(1, c(0,0), Sigma[[1]] * dt[i] ^ 2,
                               lower = rep(-sqrt(vmax*vmax/2)*3.6, 2),
                               upper = rep(sqrt(vmax*vmax/2)*3.6, 2))
               mu[i, ] <- mu[i-1, ] + g[i] * (dt[i] / dt[i-1]) * 
                 (mu[i-1,] - mu[i-2,]) + dxy
             }
           })

    ## time interval is nominally ts h, or determine by tpar
    dts <- start[[2]] + cumsum(dt) * 3600
    
    ## add Argos lc values
    d <- data.frame(
      date = dts,
      lc = argos_lc(N),
      x = mu[, 1],
      y = mu[, 2]
      )
    
    switch(error,
           ls = {
             ## Merge ARGOS error multiplication factors
             d <- merge(d, emf(), by = "lc", all.x = TRUE)
             d <- d[order(d$date), c("date", "lc", "x", "y", "emf.x", "emf.y")]
             
             Tau <- diag(2) %o% c((tau[1] * d$emf.x)^2, 
                                  (tau[2] * d$emf.y)^2)
             ## Sample errors
             err <- lapply(1:N, function(i)
                 rmvnorm(1, c(0, 0), Tau[, , i]))
             err <- do.call(rbind, err)
      
             ## add errors to data.frame
             d$x.err <- err[, 1]
             d$y.err <- err[, 2]
             d <- d[, c("date","lc","x","y","x.err","y.err")]
           },
           kf = {
             errs <- ellp.par(d$lc)
             d <- data.frame(d, errs)
           })
    
    ## add errors and transform to lon,lat - keeping true x,y
    d$x1 <- with(d, x + x.err)
    d$y1 <- with(d, y + y.err)
    d <- st_as_sf(d, coords = c("x1", "y1"),
               crs = "+proj=merc +units=km +datum=WGS84")
    d <- st_transform(d, crs = 4326)
    ll <- as.data.frame(st_coordinates(d))
    names(ll) <- c("lon","lat")
    st_geometry(d) <- NULL
    d <- data.frame(d, ll)
    
    if(error == "ls") {
      d <- d[, c("date", "lc", "lon", "lat", "x", "y", "x.err", "y.err")]
    } else if(error == "kf") {
      d <- d[, c("date", "lc", "lon", "lat", "x", "y", "x.err", "y.err","smaj","smin","eor")]
    }
    d$lc <- as.character(d$lc)
    
    switch(model,
           crw = {
             d$u <- v[, 1]
             d$v <- v[, 2]
           },
           mpm = {
             d$g <- g
           })
    
    if(n.states > 1) d$b <- b
    
    d <- as_tibble(d)
    
    switch(model,
           rw = { 
             class(d) <- append("rws", class(d))
           },
           crw = {
             class(d) <- append("crws", class(d))
           },
           mpm = {
             class(d) <- append("mpms", class(d))
           })
    
    class(d) <- append("sim", class(d))
    
    return(d)
  
}