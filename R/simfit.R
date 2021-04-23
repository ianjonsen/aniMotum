##' @title simulate animal tracks from a \code{fG_ssm} fit
##'
##' @description simulate from the \code{rw} or \code{crw} process models to generate
##' either a set of x,y (or lon,lat) coordinates from a \code{fG_ssm} fit with length
##' equal to the number of observations used in the SSM fit. 
##' @param x a compound \code{fG_ssm} model fit object (ignored if NULL)
##' @param reps number of replicate tracks to simulate from an \code{fG_ssm} model 
##' fit object (ignored if x is NULL)
##' @param what simulate fitted (typically irregular in time) or predicted 
##' (typically regular in time) locations 
##' @param cpf logical; should simulated tracks return to their start point (ie. a central-place forager)
##' @param sim_only logical, do not include \code{fG_ssm} estimated location in output 
##' (default is FALSE)
##' 
##' @examples 
##' fit <- fit_ssm(ellie, vmax = 4, model = "crw", time.step = 48, control = ssm_control(se = FALSE))
##' trs <- simfit(fit, reps = 2, what = "predicted")
##' plot(trs)
##' 
##' @importFrom dplyr "%>%" 
##' @importFrom tmvtnorm rtmvnorm
##' @importFrom mvtnorm rmvnorm
##' @importFrom tibble tibble as_tibble
##' @importFrom assertthat assert_that
##' @importFrom sf st_coordinates st_as_sf st_transform st_geometry<-
##' @importFrom stats rgamma runif
##' @importFrom CircStats rvm
##' @export

simfit <- function(x, 
                   what = c("fitted", "predicted"),
                   reps = 1,
                   cpf = FALSE,
                   sim_only = FALSE) {

  ################
  ## Check args ##
  ################
  what <- match.arg(what)
  
  assert_that(what %in% c("fitted", "predicted"),
              msg = "only `fitted` or `predicted` locations can be simulated
              from a model fit")
  
  ########################################
  ## Simulate from a foieGras model fit ##
  ########################################
  n <- nrow(x)
  d <- lapply(1:n, function(k) {
    model <- x$ssm[[k]]$pm
    switch(what,
           fitted = {
             loc <- grab(x[k, ], "fitted")
           },
           predicted = {
             loc <- grab(x[k, ], "predicted")
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
               prod(Sigma[1, 1] ^ 0.5, Sigma[2, 2] ^ 0.5) * x$ssm[[k]]$par["rho_p", 1]
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
               mu[1, ] <- st_coordinates(loc$geometry)[1, ]
               v[1,] <- c(loc$u[1], loc$v[1])
               for (i in 2:N) {
                 v[i, ] <- rtmvnorm(1,
                                    v[i - 1, ],
                                    sigma = Sigma * dt[i],
                                    lower = vmin,
                                    upper = vmax)
                 mu[i, ] <- mu[i - 1, ] + v[i, ] * dt[i]
                 ## keep within world Mercator y bounds (km)
                 #                     if(mu[i, 2] < -15496300) mu[i, 2] <- -15496300
                 #                     if(mu[i, 2] > 18764386) mu[i, 2] <- 18764386
               }
               df <- data.frame(
                 rep = j,
                 date = dts,
                 x = mu[, 1],
                 y = mu[, 2],
                 u = v[, 1],
                 v = v[, 2]
               )
               if(cpf) {
                 time <- seq(0, 1, length = N)
                 bb.x <- with(df, ((x - x[1]) - time * (x - x[1])[N]) + x[1])
                 bb.y <- with(df, ((y - y[1]) - time * (y - y[1])[N]) + y[1])
                 df <- df %>%
                   mutate(x = bb.x,
                          y = bb.y)
               }
               df
             },
             rw = {
               mu <- matrix(NA, N, 2)
               mu[1,] <- st_coordinates(loc$geometry)[1, ]
               mu[2,] <-
                 rmvnorm(1, mu[1, ], sigma = Sigma * dt[2] ^ 2)
               for (i in 3:N) {
                 dxy <- rtmvnorm(
                   1,
                   mu[i - 1,] - mu[i - 2, ],
                   sigma = Sigma * dt[i] ^ 2,
                   lower = vmin,
                   upper = vmax
                 )
                 mu[i,] <- mu[i - 1,] + dxy
                 ## keep within world Mercator y bounds (km)
                 #                     if(mu[i, 2] < -15496300) mu[i, 2] <- -15496300
                 #                     if(mu[i, 2] > 18764386) mu[i, 2] <- 18764386
               }
               df <- data.frame(
                 rep = j,
                 date = dts,
                 x = mu[, 1],
                 y = mu[, 2]
               )
               ## convert to Brownian-Bridge to simulate a central-place forager
               if(cpf) {
                 n <- nrow(df)
                 time <- seq(0, 1, length = N)
                 df <- df %>% 
                   mutate(x = ((x - x[1]) - time * (x - x[1])[N]) + x[1],
                          y = ((y - y[1]) - time * (y - y[1])[N]) + y[1]
                          )
               }
               df
             })
    }) %>%
      do.call(rbind, .) %>%
      as_tibble()
    
    if (!sim_only) {
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
    }
    
    ## lon,lat
    tmp <-
      st_as_sf(tmp, coords = c("x", "y"), crs = "+proj=merc +units=km +datum=WGS84")
    xy <- st_coordinates(tmp) %>% as.data.frame()
    names(xy) <- c("x", "y")
    ll <- tmp %>%
      st_transform(., crs = 4326) %>%
      st_coordinates(.) %>%
      as.data.frame(.)
    names(ll) <- c("lon", "lat")
    st_geometry(tmp) <- NULL
    cbind(tmp, xy, ll) %>%
      as_tibble() %>%
      select(rep, date, lon, lat, x, y, everything())
  })

  
    
  d <- tibble(id = x$id,
              model = x$pmodel,
              sims = d)
  switch(unique(x$pmodel),
         rw = {
           class(d) <- append("fG_rws", class(d))
         },
         crw = {
           class(d) <- append("fG_crws", class(d))
         })
  
  class(d) <- append("fG_simfit", class(d))
  return(d)

}
                