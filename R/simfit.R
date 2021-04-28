##' @title simulate animal tracks from a \code{fG_ssm} fit
##'
##' @description simulate from the \code{rw} or \code{crw} process models to generate
##' either a set of x,y (or lon,lat) coordinates from a \code{fG_ssm} fit with length
##' equal to the number of observations used in the SSM fit. 
##' @param x a compound \code{fG_ssm} model fit object (ignored if NULL)
##' @param what simulate fitted (typically irregular in time) or predicted 
##' (typically regular in time) locations 
##' @param reps number of replicate tracks to simulate from an \code{fG_ssm} model 
##' fit object
##' @param grad a rasterStack of x- and y-gradients as separate layers
##' @param beta a 2-element vector of parameters defining the potential function 
##' magnitude in x- and y-directions (ignored if \code{is.null(grad)}, 
##' ie. no potential function)
##' @param cpf logical; should simulated tracks return to their start piont (ie. a central-place forager)
##' @param sim_only logical, do not include \code{fG_ssm} estimated location in output 
##' (default is FALSE)
##' 
##' @examples 
##' fit <- fit_ssm(ellies, vmax = 4, model = "crw", time.step = 72)
##' trs <- simfit(fit, what = "predicted", reps = 2)
##' plot(trs)
##' 
##' @importFrom dplyr "%>%" 
##' @importFrom tmvtnorm rtmvnorm
##' @importFrom mvtnorm rmvnorm
##' @importFrom tibble tibble as_tibble
##' @importFrom assertthat assert_that
##' @importFrom sf st_coordinates st_as_sf st_transform st_geometry<-
##' @importFrom stats rgamma runif
##' @importFrom raster extent extract nlayers
##' @importFrom CircStats rvm
##' @export

simfit <-
  function(x,
           what = c("fitted", "predicted"),
           reps = 1,
           grad = NULL,
           beta = c(-250, -250),
           cpf = FALSE,
           sim_only = FALSE) {
    
  
  ################
  ## Check args ##
  ################
  what <- match.arg(what)
  if(!is.null(grad)) {
    if(!inherits(grad, "RasterStack")) 
      stop("grad must be NULL or a RasterStack with 2 layers")
    if(inherits(grad, "RasterStack") & nlayers(grad) != 2)
      stop("grad RasterStack must have 2 layers")
  }
  if(length(beta) != 2) stop("beta must be specified as a 2-element vector")
  
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
             loc <- grab(x[k,], "fitted", as_sf = FALSE)
           },
           predicted = {
             loc <- grab(x[k,], "predicted", as_sf = FALSE)
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
    if(!is.null(grad)) {
      ex <- extent(grad[[1]])
    } else {
      ex <- c(-20077.51,20082.49,-19622.54,18437.46) ## approx extents of world mercator in km
    }
    wrap_x <- function(mu, x_rng) c((mu[1] - x_rng[1]) %% sum(abs(x_rng)) + x_rng[1], mu[2])
    reflect_y <- function(mu, y_rng) {
      if(mu[2] < y_rng[1]) {
        c(mu[1], y_rng[1] * 2 - mu[2])
      } else if(mu[2] > y_rng[2]) {
        c(mu[1], y_rng[2] * 2 - mu[2])
      } else {
        mu
      }
    }
    
    tmp <- lapply(1:reps, function(j) {
      switch(model,
             crw = {
               mu <- v <- matrix(NA, N, 2)
               time <- seq(0, 1, length = N)
               mu[1,] <- c(loc$x[1], loc$y[1])
               v[1, ] <- c(loc$u[1], loc$v[1])
               for (i in 2:N) {
                 v[i,] <- rtmvnorm(1,
                                   v[i - 1,],
                                   sigma = Sigma * dt[i],
                                   lower = vmin,
                                   upper = vmax)
                 ## wrap x values, reflect y values
                 mu1 <- wrap_x(mu[i-1,] + v[i,] * dt[i], ex[1:2])
                 mu1 <- reflect_y(mu1, ex[3:4])
                 if(!is.null(grad)) {
                   pv <- c(extract(grad[[1]], rbind(mu1))[1],
                           extract(grad[[2]], rbind(mu1))[1])
                   mu[i,] <- mu1 + pv * beta
                 } else {
                   mu[i,] <- mu1
                 }
               }
               df <- data.frame(
                 rep = j,
                 date = dts,
                 x = mu[, 1],
                 y = mu[, 2]
               )
               if(cpf) {
                 df <- df %>%
                   mutate(x = ((x - x[1]) - time *
                                 (x - x[1])[N]) + x[1]) %>%
                   mutate(y = ((y - y[1]) - time *
                                 (y - y[1])[N]) + y[1])
                 
               }
               df
             },
             rw = {
               mu <- matrix(NA, N, 2) 
               time <- seq(0, 1, length = N)
               mu[1, ] <- c(loc$x[1], loc$y[1])
               mu[2, ] <- rmvnorm(1, mu[1,], sigma = Sigma * dt[2]^2)
               for (i in 3:N) {
                 dxy <- rtmvnorm(100, mu[i - 1, ] - mu[i - 2,], 
                                 sigma = Sigma * dt[i]^2,
                                 lower = vmin,
                                 upper = vmax,
                                 algorithm = "gibbs", burn.in.samples = 100)
                 dxy <- dxy[which(!is.na(dxy))[1],]
                 ## wrap x values, reflect y values
                 mu1 <- wrap_x(mu[i-1,] + dxy, ex[1:2])
                 mu1 <- reflect_y(mu1, ex[3:4])
                 if(!is.null(grad)) {
                   pv <- c(extract(grad[[1]], rbind(mu1))[1],
                           extract(grad[[2]], rbind(mu1))[1])
                   mu[i,] <- mu1 + pv * beta
                 } else {
                   mu[i,] <- mu1
                 }
               }
               df <- data.frame(
                 rep = j,
                 date = dts,
                 x = mu[, 1],
                 y = mu[, 2]
               )
               if(cpf) {
                 df <- df %>%
                   mutate(x = ((x - x[1]) - time * 
                                 (x - x[1])[N]) + x[1]) %>%
                   mutate(y = ((y - y[1]) - time * 
                                 (y - y[1])[N]) + y[1])
               }
               df
             })
    }) %>% 
      do.call(rbind, .) %>%
      as_tibble()

    if (!sim_only) {
      loc <- grab(x[k, ], what = what, as_sf = FALSE) %>%
        mutate(rep = 0)
      loc <- loc %>% select(rep, date, x, y)
      tmp <- rbind(loc, tmp)
    }
    
    ## lon,lat
    tmp1 <- try(st_as_sf(tmp, coords = c("x","y"), crs = "+proj=merc +units=km +datum=WGS84"), silent = TRUE)
    if(inherits(tmp1, "try-error")) {
      stop("oops something went wrong, try again", call. = FALSE)
    }
    
    xy <- st_coordinates(tmp1) %>% as.data.frame()
    names(xy) <- c("x","y")
    ll <- tmp1 %>% 
      st_transform(., crs = 4326) %>%
      st_coordinates(.) %>%
      as.data.frame(.)
    names(ll) <- c("lon","lat")
    st_geometry(tmp1) <- NULL
    cbind(tmp1, xy, ll) %>% 
      as_tibble() %>%
      select(rep, date, lon, lat, x, y, everything())
  }) 
  
  d <- tibble(id = x$id, model = x$pmodel, sims = d)
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
