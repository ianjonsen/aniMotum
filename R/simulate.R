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
                     start = c(0,0),
                     model = c("rw", "crw"), 
                     sigma = c(7, 6.5), 
                     rho_p = -0.45,
                     D = 0.6,
                     tau = c(0.85, 0.55),
                     rho_o = 0.08,
                     error = c("ls","kf","dist"),
                     t_dist = "gamma",
                     tpar = c(1.5, 0.5)
                     ) {
  
  model <- match.arg(model)
  error <- match.arg(error)
  
  assert_that(model %in% c("rw","crw"), 
              msg = "model can only be 1 of `rw` or `crw`")
  assert_that(error %in% c("ls","kf","dist"), 
              msg = "error can only be 1 of `ls`, `kf`, or `dist`")
  
  
  if(is.null(x)) {
    start.date <- Sys.time()
    
    Y <- mu <- v <- matrix(NA, N, 2)
    v[1,] <- mu[1,] <- start 
    
    ## generate random time intervals (h)
    switch(t_dist, 
           lnorm = {
             #3, 0.5
             dt <- c(0, rlnorm(N-1, tpar[1], tpar[2]))
           },
           gamma = {
             # 1.5, 0.5 = mean 3 h, extreme ~ 30-35 h
             dt <- c(0, rgamma(N-1, tpar[1], tpar[2]))
           },
           exp = {
             dt <- c(0, rexp(N-1, tpar[1]))
           })
    
    
    ## Define var-covar matrix
    switch(model,
          rw = {
      Sigma <-
        matrix(c(sigma[1] ^ 2, 
                 sigma[1] * sigma[2] * rho_p[1], 
                 sigma[1] * sigma[2] * rho_p[1], 
                 sigma[2] ^ 2),
               2, 
               2,
               byrow = TRUE)
    },
    crw = {
      Sigma <- diag(2) * 2 * D
    })
    
    switch(model,
           crw = {
             for (i in 2:N) {
               v[i, ] <- rmvnorm(1, v[i - 1, ], Sigma * dt[i])
               mu[i, ] <- mu[i - 1, ] + v[i, ] * dt[i]
             }
           },
           rw = {
             for(i in 2:N) {
               mu[i, ] <- rmvnorm(1, mu[i-1, ], Sigma * dt[i]^2)
             }
           }
           )

    ## time interval is nominally 1 h
    dts <- start.date + cumsum(dt) * 3600
    
    ## probability vector
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
    
    switch(model,
           crw = {
             d <- data.frame(
               date = dts,
               lc = lc,
               x = mu[, 1],
               y = mu[, 2],
               u = v[, 1],
               v = v[, 2]
             )
           },
           rw = {
             d <- data.frame(
               date = dts,
               lc = lc,
               x = mu[, 1],
               y = mu[, 2]
             )
           })
    
    ## Merge ARGOS error multiplication factors
     d <- merge(d, emf(), by = "lc", all.x = TRUE)
     d <- d[order(d$date), ]
    
    ## Sample Argos measurement errors
     Tau <- diag(2) %o% c(tau[1] * d$emf.x, tau[2] * d$emf.y)
     err <- lapply(1:N, function(i) rmvnorm(1, c(0,0), Tau[,,i])) %>%
       do.call(rbind, .)
     
     d <- d %>% 
       mutate(err.x = err[, 1], 
              err.y = err[, 2]) %>%
       mutate(id = round(runif(1, 1, 10000))) %>%
       select(id, date, lc, x, y, err.x, err.y)
       
    return(d)
    
  } else {
    
  }
  
}