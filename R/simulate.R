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
##' to sample smaj, smin from log-normals & eor froma von Mises, then convert
##' ellipse params to covariance matrix and sample x,y errors from multivariate
##' normal. Returns both ellipse params and x,y errors
##' 
##' @param lc Argos location classes
##' 
##' @importFrom CircStats rvm
##' @importFrom stats rlnorm
##' @importFrom mvtnorm rmvnorm
##' 
##' @export

epar <- function(lc) {
  
  ellps.tab <-
    readRDS(system.file("extdata", "error_ellipse.RDS",
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


##' @title simulate animal tracks from a \code{fG_ssm} fit or from scratch
##'
##' @description simulate from the \code{rw} or \code{crw} process models to generate either a set of x,y (or lon,lat) coordinates from a \code{fG_ssm} fit with length equal to the number of observations used in the SSM fit, or a set of x,y (or lon,lat) coordinates with or without error from supplied input parameters. 
##' @param x a compound \code{fG} tbl fit object (ignored if NULL)
##' @param N number of time steps to simulate (ignored if x is suppled)
##' @param model simulate from the \code{rw} or \code{crw} process model (ignored if x is suppled)
##' @param Sigma covariance matrix for movement process (ignored if x is suppled)
##' @param error simulate Argos errors based on Least-Squares location classes ("lc"), Argos Kalman Filter error ellipses ("kf"), or from an arbitrary distribution ("dist") (ignored if x is suppled)
##' 
##' @return a data.frame
##' 
##' @examples 
##' 
##' @importFrom dplyr "%>%" 
##' @importFrom tmvtnorm rtmvnorm
##' @importFrom mvtnorm rmvnorm
##' @importFrom tibble tibble as_tibble
##' @importFrom assertthat assert_that
##' @importFrom sf st_coordinates st_as_sf st_transform st_geometry<-
##' @importFrom CircStats rvm
##' 
##' @export

simulate <- function(x = NULL, 
                     reps = 1,
                     what = c("fitted", "predicted"),
                     sim_only = FALSE,
                     N = 100, 
                     start = list(c(0, 0), Sys.time()),
                     model = c("rw", "crw", "mpm"),
                     vmax = 4,
                     sigma = c(4, 4), 
                     rho_p = 0,
                     D = 0.05,
                     tau = c(1.5, 0.75),
                     rho_o = 0,
                     sigma_g = 1.25,
                     error = c("ls","kf"),
                     t_dist = c("reg", "gamma"),
                     tpar = c(1.5, 0.5), 
                     alpha = c(0.9, 0.8)
                     ) {
  
  ################
  ## Check args ##
  ################
  what <- match.arg(what)
  model <- match.arg(model)
  error <- match.arg(error)
  t_dist <- match.arg(t_dist)

  assert_that(what %in% c("fitted","predicted"), 
              msg = "only `fitted` or `predicted` locations can be simulated 
              from a model fit")
  assert_that(model %in% c("rw","crw","mpm"), 
              msg = "model can only be 1 of `rw`, `crw`, or `mpm`")
  assert_that(error %in% c("ls","kf"), 
              msg = "error can only be 1 of `ls` or `kf`")
  assert_that(t_dist %in% c("gamma","reg"),
              msg = "t_dist can only be 1 of `gamma` or `reg`")
  assert_that(inherits(start, "list") & length(start) == 2 & inherits(start[[2]], "POSIXct"),
              msg = "`start` must be a 2-element list, with coordinates as the 1st element and
              a POSIX datetime as the 2nd element")
  
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
  if(is.null(x)) {
    
    mu <- v <- matrix(NA, N, 2)
    mu[1, ] <- start[[1]]
    v[1, ] <- c(0,0)
    
    ## generate random time intervals (h)
    switch(t_dist, 
           gamma = {
             # 1.5, 0.5 = mean 3 h, extreme ~ 30-35 h
             dt <- c(0, rgamma(N-1, tpar[1], tpar[2]))
           },
           reg = {
             dt <- c(0, rep(3, N-1))
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

    ## time interval is nominally 1 h
    dts <- start[[2]] + cumsum(dt) * 3600
    
    ## add Argos lc values
    d <- tibble(
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
                 rmvnorm(1, c(0, 0), Tau[, , i])) %>%
               do.call(rbind, .)
      
             ## add errors to data.frame
             d <- d %>%
               mutate(x.err = err[, 1],
                      y.err = err[, 2]) %>%
               select(date, lc, x, y, x.err, y.err) 
           },
           kf = {
             errs <- epar(d$lc)
             d <- data.frame(d, errs)
           })
    
    ## add errors and transform to lon,lat - keeping true x,y
    d <- d %>% mutate(x1 = x + x.err, y1 = y + y.err) %>%
      st_as_sf(., coords = c("x1", "y1"),
               crs = "+proj=merc +units=km +datum=WGS84") %>%
      st_transform(xy, crs = 4326)
    ll <- st_coordinates(d) %>%
      as.data.frame() 
    names(ll) <- c("lon","lat")
    st_geometry(d) <- NULL
    d <- tibble(d, ll) %>%
      select(date, lc, lon, lat, x, y, x.err, y.err, everything()) %>%
      mutate(lc = as.character(lc))
    
    switch(model,
           crw = {
             d <- d %>% mutate(u = v[, 1], v = v[, 2])
           },
           mpm = {
             d <- d %>% mutate(g = g)
           })
    
    if(n.states > 1) d <- d %>% mutate(b = b)
    
    switch(model,
           rw = { 
             class(d) <- append("fG_rws", class(d))
           },
           crw = {
             class(d) <- append("fG_crws", class(d))
           },
           mpm = {
             class(d) <- append("fG_mpms", class(d))
           })
    
    class(d) <- append("fG_sim", class(d))
    
    return(d)
    
  } else {
    ########################################
    ## Simulate from a foieGras model fit ##
    ########################################
    n <- nrow(x)
    d <- lapply(1:n, function(k) {
        model <- x$ssm[[k]]$pm
        switch(what,
               fitted = {
                 loc <- grab(x[k,], "fitted")
                 },
               predicted = {
                 loc <- grab(x[k,], "predicted")
               })
        N <- nrow(loc)
        dts <- loc$date
        dt <-
          difftime(dts, lag(dts), units = "hours") %>% as.numeric()
        dt[1] <- 0
        
        switch(model,
               crw = {
                 Sigma <- diag(2) * 2 * x$ssm[[k]]$par["D", 1]
                 vmin <- with(loc,
                              c(min(u, na.rm = TRUE),
                                min(v, na.rm = TRUE))) # in km/h
                 vmax <- with(loc,
                              c(max(u, na.rm = TRUE),
                                max(v, na.rm = TRUE)))
               },
               rw = {
                 Sigma <-
                   diag(2) * c(x$ssm[[k]]$par["sigma_x", 1], 
                               x$ssm[[k]]$par["sigma_y", 1]) ^ 2
                 Sigma[!Sigma] <-
                   prod(Sigma[1, 1]^0.5, Sigma[2, 2]^0.5) * x$ssm[[k]]$par["rho_p", 1]
                 vmin <- with(loc, c(min(diff(x), na.rm = TRUE), 
                                     min(diff(y), na.rm = TRUE)))
                 vmax <- with(loc, c(max(diff(x), na.rm = TRUE), 
                                     max(diff(y), na.rm = TRUE)))
               })
        
        ###############################
        ## Simulate movement process ##
        ###############################
        tmp <- lapply(1:reps, function(j) {
          switch(model,
                 crw = {
                   mu <- v <- matrix(NA, N, 2)
                   mu[1,] <- st_coordinates(loc$geometry)[1,]
                   v[1, ] <- c(loc$u[1], loc$v[1])
                   for (i in 2:N) {
                     v[i,] <- rtmvnorm(1,
                                       v[i - 1,],
                                       sigma = Sigma * dt[i],
                                       lower = vmin,
                                       upper = vmax)
                     mu[i,] <- mu[i - 1,] + v[i,] * dt[i]
                   }
                   data.frame(
                     rep = j,
                     date = dts,
                     x = mu[, 1],
                     y = mu[, 2],
                     u = v[, 1],
                     v = v[, 2]
                   )
                 },
                 rw = {
                   mu <- matrix(NA, N, 2)
                   mu[1, ] <- st_coordinates(loc$geometry)[1,]
                   mu[2, ] <- rmvnorm(1, mu[1,], sigma = Sigma * dt[2]^2)
                   for (i in 3:N) {
                     dxy <- rtmvnorm(1, mu[i - 1, ] - mu[i - 2,], 
                                     sigma = Sigma * dt[i]^2,
                                     lower = vmin,
                                     upper = vmax)
                     mu[i, ] <- mu[i - 1, ] + dxy
                   }
                   data.frame(
                     rep = j,
                     date = dts,
                     x = mu[, 1],
                     y = mu[, 2]
                   )
                 })
        }) %>% 
          do.call(rbind, .) %>%
          as_tibble()
        
        loc <- grab(x[k,], what = what, as_sf = FALSE) %>%
          mutate(rep = 0)
        switch(model,
               crw = { 
                 loc <- loc %>% select(rep, date, x, y, u, v)
                 },
               rw = {
                 loc <- loc %>% select(rep, date, x, y)
               })
        tmp <- rbind(loc, tmp)
        ## lon,lat
        tmp <- st_as_sf(tmp, coords = c("x","y"), crs = "+proj=merc +units=km +datum=WGS84")
        xy <- st_coordinates(tmp) %>% as.data.frame()
        names(xy) <- c("x","y")
        ll <- tmp %>% 
          st_transform(., crs = 4326) %>%
          st_coordinates(.) %>%
          as.data.frame(.)
        names(ll) <- c("lon","lat")
        st_geometry(tmp) <- NULL
        cbind(tmp, xy, ll) %>% 
          as_tibble() %>%
          select(rep, date, lon, lat, x, y, everything())
      }) 
    
    d <- tibble(id = x$id, model = x$pmodel, sims = d)
    switch(model,
           rw = { 
             class(d) <- append("fG_rws", class(d))
           },
           crw = {
             class(d) <- append("fG_crws", class(d))
           })
    
    class(d) <- append("fG_simfit", class(d))
    return(d)
  }
  
}