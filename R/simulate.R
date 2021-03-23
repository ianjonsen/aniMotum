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
##' @importFrom mvtnorm rmvnorm
##' @importFrom tibble tibble
##' @importFrom assertthat assert_that
##' 
##' @export

simulate <- function(x = NULL, 
                     N = 100, 
                     start = list(c(0, 0), Sys.time()),
                     model = c("rw", "crw", "mpm"), 
                     sigma = c(7, 6.5), 
                     rho_p = -0.45,
                     D = 0.05,
                     tau = c(0.85, 0.55),
                     rho_o = 0.08,
                     sigma_g = 1,
                     error = c("ls","kf","dist"),
                     t_dist = c("gamma","lnorm","exp"),
                     tpar = c(1.5, 0.5), 
                     alpha = c(0.9, 0.8),
                     beta = c(1, 0.25)
                     ) {
  
  ################
  ## Check args ##
  ################
  model <- match.arg(model)
  error <- match.arg(error)
  t_dist <- match.arg(t_dist)

  assert_that(model %in% c("rw","crw","mpm"), 
              msg = "model can only be 1 of `rw`, `crw`, or `mpm`")
  assert_that(error %in% c("ls","kf","dist"), 
              msg = "error can only be 1 of `ls`, `kf`, or `dist`")
  assert_that(t_dist %in% c("gamma","lnorm","exp"),
              msg = "t_dist can only be 1 of `gamma`, `lnorm`, or `exp`")
  assert_that(inherits(start, "list") & length(start) == 2 & inherits(start[[2]], "POSIXct"),
              msg = "`start` must be a 2-element list, with coordinates as the 1st element and
              a POSIX datetime as the 2nd element")
  
  n.states <- 1
  if(length(sigma) > 2 & model == "rw") n.states <- length(sigma)/2
  if(length(D) > 1 & model == "crw") n.states <- length(D)
  
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
           lnorm = {
             #3, 0.5
             dt <- c(0, rlnorm(N-1, tpar[1], tpar[2]))
           },
           exp = {
             dt <- c(0, rexp(N-1, tpar[1]))
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
               v[i,] <- rmvnorm(1, beta[b[i]] * v[i - 1,], Sigma[[b[i]]] * dt[i])
               mu[i,] <- mu[i - 1,] + v[i,] * dt[i]
             }
           },
           rw = {
             for (i in 2:N) {
               mu[i,] <- rmvnorm(1, mu[i - 1,], Sigma[[b[i]]] * dt[i] ^ 2)
             }
           },
           mpm = {
             dt.g <- dt / median(dt)
             
             lg <- rep(NA, N)
             lg[1] <- runif(1,-5, 5)
             sd <- sigma_g * dt.g
             for (i in 2:N) {
               ## use a truncated normal to simulate g in logit space:
               ##   suitable bounds keep RW wandering off at extremes
               lg[i] <- rtnorm(1, lg[i - 1], sd[i], l = -8, u = 8)
             }
             g <- plogis(lg)
             mu[2,] <- rmvnorm(1, mu[1,], Sigma[[1]] * dt.g[2] ^ 2)
             for (i in 3:N) {
               mu[i, ] <- rmvnorm(1, mu[i - 1,] +
                                    g[i] * (dt.g[i] / dt.g[i - 1]) *
                                    (mu[i - 1,] - mu[i - 2,]),
                                  Sigma[[1]] * dt[i] ^ 2)
             }
           })

    ## time interval is nominally 1 h
    dts <- start[[2]] + cumsum(dt) * 3600
    
    ##############################################
    ## Simulate Argos error classes for LS data ##
    ##############################################
    lc <- factor(
      sample(
        c(3, 2, 1, 0, "A", "B"),
        N,
        replace = TRUE,
        prob = c(0.01, 0.04, 0.13, 0.2, 0.27, 0.35) # from ellies ex data
      ),
      levels = c(3, 2, 1, 0, "A", "B"),
      ordered = TRUE
    )
    
    d <- data.frame(
      date = dts,
      lc = lc,
      x = mu[, 1],
      y = mu[, 2]
      )
    
    switch(error,
           ls = {
             ## Merge ARGOS error multiplication factors
             d <- merge(d, emf(), by = "lc", all.x = TRUE)
             d <- d[order(d$date), ]
             
             Tau <- diag(2) %o% c((tau[1] * d$emf.x)^2, 
                                  (tau[2] * d$emf.y)^2)
             ## Sample errors
             err <-
               lapply(1:N, function(i)
                 rmvnorm(1, c(0, 0), Tau[, , i])) %>%
               do.call(rbind, .)
             
             ## add errors to data.frame
             d <- d %>%
               mutate(err.x = err[, 1],
                      err.y = err[, 2]) %>%
               select(date, lc, x, y, err.x, err.y)
           },
           kf = {
             
           })
    
    switch(model,
           crw = {
             d <- d %>% mutate(u = v[, 1], v = v[, 2])
           },
           mpm = {
             d <- d %>% mutate(g = g)
           })
    
    if(n.states > 1) d <- d %>% mutate(b = b)
    row.names(d) <- 1:N
    
    class(d) <- append("fG_sim", class(d))
    
    return(d)
    
  } else {
    ########################################
    ## Simulate from a foieGras model fit ##
    ########################################
    
  }
  
}