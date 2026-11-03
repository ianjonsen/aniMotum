##' @title simulate animal tracks from a \code{ssm} fit
##'
##' @description simulate from the \code{rw} or \code{crw} process models to generate
##' either a set of x,y or lon,lat coordinates from a \code{ssm} fit with length
##' equal to the number of observations used in the SSM fit. 
##' @param x a \code{ssm} fit object with class `ssm_df`
##' @param what simulate fitted (typically irregular in time) or predicted 
##' (typically regular in time) locations 
##' @param reps number of replicate tracks to simulate from an \code{ssm} model 
##' fit object
##' @param start a 2-element vector for the simulated track start location
##' (lon,lat)
##' @param end a 2-element vector for the simulated track end location 
##' (lon,lat)
##' @param grad a SpatRaster of x- and y-gradients as separate layers (see details)
##' @param beta a 2-element vector of parameters defining the potential function 
##' magnitude in x- and y-directions (ignored if \code{is.null(grad)}, 
##' ie. no potential function; see details).
##' @param cpf logical; should simulated tracks return to their start point 
##' (ie. a central-place forager)
##' @param sim_only logical, do not include \code{ssm} estimated location in output 
##' (default is FALSE)
##' 
##' @details A potential function can be applied to the simulated paths to help 
##' avoid locations on land (or in water), using the \code{grad} and \code{beta}
##' arguments. A coarse-resolution rasterStack of global x- and y-gradients of 
##' distance to land are provided. Stronger beta parameters result in stronger 
##' land (water) avoidance but may also introduce undesirable/unrealistic artefacts 
##' (zig-zags) in the simulated paths. See Brillinger et al. (2012) and 
##' \code{vignette("momentuHMM", package = "momentuHMM")} for more details on the
##' use of potential functions for simulating constrained animal movements. 
##' WARNING: This application of potential functions to constrain simulated 
##' paths is experimental, likely to change in future releases, and NOT guaranteed 
##' to work enitrely as intended, especially if \code{cpf = TRUE}!
##'
##' Each track is simulated in the projection its model was fitted in: the
##' projection of an \code{sf} object supplied to \code{fit_ssm}, or otherwise
##' the Mercator projection chosen by \code{fit_ssm} (centred on 180 for tracks
##' that cross the dateline). The movement parameters are estimated in that
##' projection, so simulating in it keeps them at the correct scale. Simulated
##' x-coordinates are wrapped, and y-coordinates reflected, at the world limits
##' only when that projection is Mercator. When \code{grad} is supplied, tracks
##' are simulated in the projection of \code{grad}, with a warning for any fit
##' made in a different projection. \code{start} and \code{end} are lon,lat
##' coordinates.
##'
##' @return a \code{fG_sim_fit} object containing the paths simulated from a
##' \code{ssm} fit object. The \code{x,y} coordinates are in the projection
##' returned by \code{grab(x, what, as_sf = TRUE)}, stored in the \code{prj}
##' attribute; \code{lon,lat} are WGS84.
##' 
##' @references 
##' Brillinger DR, Preisler HK, Ager AA, Kie J (2012) The use of potential functions in modelling animal movement. In: Guttorp P., Brillinger D. (eds) Selected Works of David Brillinger. Selected Works in Probability and Statistics. Springer, New York. pp. 385-409.
##' 
##' @examples 
##' fit <- fit_ssm(ellie, model = "crw", time.step = 24)
##' trs <- sim_fit(fit, what = "predicted", reps = 3)
##' plot(trs)
##' 
##' @importFrom tmvtnorm rtmvnorm
##' @importFrom mvtnorm rmvnorm
##' @importFrom tibble tibble as_tibble
##' @importFrom sf st_crs st_coordinates st_as_sf st_transform st_geometry<-
##' @importFrom stats rgamma runif
##' @importFrom terra ext extract
##' @importFrom CircStats rvm
##' @importFrom dplyr bind_rows
##' @export

sim_fit <-
  function(x,
           what = c("fitted", "predicted"),
           reps = 1,
           start = NULL,
           end = NULL,
           grad = NULL,
           beta = c(-300, -300),
           cpf = FALSE,
           sim_only = FALSE) {
    
  ################
  ## Check args ##
  ################
  what <- match.arg(what)
  
  if(!is.null(grad)) {
    if(!inherits(grad, "SpatRaster")) 
      stop("grad must be NULL or a SpatRaster with 2 layers")
    if(inherits(grad, "SpatRaster") & length(names(grad)) != 2)
      stop("grad must have 2 layers")
  }

  if(all(!is.null(start), length(start) != 2)) 
    stop("start location must be a single lon,lat coordinate")
  if(all(!is.null(end), length(end) != 2)) 
    stop("end location must be a single lon,lat coordinate")
  
  if(length(beta) != 2) 
    stop("beta must be specified as a 2-element vector")
  
  stopifnot("x must be an `ssm_df` fit object" = inherits(x, "ssm_df"))
  
  if(!what %in% c("fitted", "predicted")) 
    stop("only `fitted` or `predicted` locations can be simulated from a model fit")
  
  if(any(sapply(x$ssm, function(m) m$pm) %in% c("mp", "jmp")))
    stop("'mp' process model fit(s) detected\n  tracks can only be simulated from SSM fits using either the 'rw' or 'crw' process models")
  
  ## Projections
  ## Each track is simulated in the projection its model was fitted in. The
  ## movement parameters (D or sigma), and the crw velocities u, v, are
  ## estimated in that projection's coordinates, so simulating in any other
  ## projection would apply them at the wrong scale and orientation. That
  ## projection is the user's own when an sf object was supplied to fit_ssm(),
  ## otherwise the Mercator projection aniMotum chose, which is centred on 180
  ## for tracks that cross the dateline.
  ## The one exception is a potential function: grad's gradients are defined
  ## in grad's coordinates, so with grad supplied the tracks are simulated in
  ## grad's projection.
  ## Simulated x, y are returned in the single projection grab() uses for
  ## these fits (as_sf = TRUE), so that tracks of several individuals share
  ## one coordinate system.
  prj.out <- grab(x, what, as_sf = TRUE) |>
    st_crs()
  prj.grad <- if(!is.null(grad)) st_crs(terra::crs(grad)) else NULL

  ## world Mercator extents in km, for wrapping x and reflecting y
  ## (relative to the central meridian, so they hold for any lon_0)
  ex.merc <- c(-20077.51,20082.49,-19622.54,18437.46)

  ## lon,lat start or end location to x,y in a given projection
  ll_to_xy <- function(p, prj) {
    xy <- st_as_sf(data.frame(lon = p[1], lat = p[2]),
                   coords = c("lon","lat"), crs = 4326) |>
      st_transform(crs = prj) |>
      st_coordinates()
    as.numeric(xy)
  }

  ######################
  ## Helper functions ##    
  ######################
  ## both return mu unchanged when no extents apply (x_rng, y_rng NULL)
  wrap_x <- function(mu, x_rng) {
    if(is.null(x_rng)) return(mu)
    c((mu[1] - x_rng[1]) %% sum(abs(x_rng)) + x_rng[1], mu[2])
  }

  reflect_y <- function(mu, y_rng) {
    if(is.null(y_rng)) return(mu)
    if(mu[2] < y_rng[1]) {
      c(mu[1], y_rng[1] * 2 - mu[2])
    } else if(mu[2] > y_rng[2]) {
      c(mu[1], y_rng[2] * 2 - mu[2])
    } else {
      mu
    }
  }

  ######################
  ## Simulate from a aniMotum model fit ##
  ########################################
  n <- nrow(x)
  d <- lapply(1:n, function(k) {
    model <- x$ssm[[k]]$pm

    ## projection to simulate this track in
    prj.fit <- st_crs(x$ssm[[k]][[what]])
    if(!is.null(grad)) {
      prj.sim <- prj.grad
      if(prj.fit != prj.sim) {
        warning(sprintf(paste0("%s: the model was fitted in a different projection ",
                               "from `grad`; its movement parameters are applied in ",
                               "`grad`'s projection, so the simulated movements ",
                               "will be distorted"), x$id[k]),
                call. = FALSE, immediate. = TRUE)
        x$ssm[[k]][[what]] <- x$ssm[[k]][[what]] |>
          st_transform(crs = prj.sim)
      }
    } else {
      prj.sim <- prj.fit
    }

    ## simulation extents: wrap x and reflect y only where the coordinates
    ## are world Mercator or the potential function's raster; in any other
    ## projection the world-Mercator limits do not apply
    if(!is.null(grad)) {
      ex <- as.vector(ext(grad))
    } else if (isTRUE(grepl("+proj=merc", prj.sim$proj4string, fixed = TRUE))) {
      ex <- ex.merc
    } else {
      ex <- NULL
    }

    ## start and end locations in the simulation projection
    if(!is.null(start)) st1 <- ll_to_xy(start, prj.sim)
    if(!is.null(end)) ed1 <- ll_to_xy(end, prj.sim)

    ## extract locations
    switch(what,
           fitted = {
             loc <- grab(x[k,], "fitted", as_sf = FALSE)
           },
           predicted = {
             loc <- grab(x[k,], "predicted", as_sf = FALSE)
           })
    
    N <- nrow(loc)
    dts <- loc$date
    dt <- as.numeric(difftime(dts, c(as.POSIXct(NA), dts[-length(dts)]), units = "hours"))
    dt[1] <- 0
       
    ## get parameters from model fit object
    switch(model,
           crw = {
             Sigma <- diag(2) * 2 * x$ssm[[k]]$par[c("D_x","D_y"), 1]
             Sigma[1,2] <- Sigma[2,1] <- x$ssm[[k]]$par["rho_p",1] * sqrt(Sigma[1,1]) * sqrt(Sigma[2,2])
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
             if(any(abs(vmin / mean(dt[-1]) / 3.6) > 5) | any(vmax / mean(dt[-1]) / 3.6 > 5))
               stop("Implausible travel rates detected, check SSM fit for implausible locations")
             ## implausible for pinnipeds but not for eg. birds** need to re-think this...***
           })

    ###############################
    ## Simulate movement process ##
    ###############################
    
    tmp <- lapply(1:reps, function(j) {
      switch(model,
             crw = {
               mu <- v <- matrix(NA, N, 2)
               time <- seq(0, 1, length = N)
               if(is.null(start)) {
                 mu[1,] <- c(loc$x[1], loc$y[1])
               } else {
                 mu[1,] <- st1
               }
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
                   pv <- as.numeric(extract(grad, rbind(mu1)))
                   
                   if(all(is.na(pv))) pv <- c(0,0) # unsure why NA's can creep in here
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
               if(cpf & is.null(end)) {
                 df <- df |>
                   mutate(x = ((x - x[1]) - time *
                                 (x - x[1])[N]) + x[1]) |>
                   mutate(y = ((y - y[1]) - time *
                                 (y - y[1])[N]) + y[1])
                 
               } else if (!is.null(end)) { 
                 df <- df |>
                   mutate(x = ((x - x[1]) - time *
                                 (x - ed1[1])[N]) + x[1]) |>
                   mutate(y = ((y - y[1]) - time *
                                 (y - ed1[2])[N]) + y[1])
                 
               }
               df
             },
             rw = {
               mu <- matrix(NA, N, 2) 
               time <- seq(0, 1, length = N)
               if(is.null(start)) {
                 mu[1, ] <- c(loc$x[1], loc$y[1])
               } else {
                 mu[1, ] <- st1
               }
               
               mu[2, ] <- rmvnorm(1, mu[1,], sigma = Sigma * dt[2]^2)
               for (i in 3:N) {
                 dxy <- rtmvnorm(100, mu[i - 1, ] - mu[i - 2,], 
                                 sigma = Sigma * dt[i]^2,
                                 lower = vmin,
                                 upper = vmax,
                                 algorithm = "gibbs", 
                                 burn.in.samples = 100)
                 
                 dxy <- dxy[which(!is.na(dxy))[1],]
                 if(all(is.na(dxy))) dxy <- c(1,1)

                 ## wrap x values, reflect y values
                 mu1 <- wrap_x(mu[i-1,] + dxy, ex[1:2])
                 mu1 <- reflect_y(mu1, ex[3:4])
                 if(!is.null(grad)) {
                   pv <- as.numeric(extract(grad, rbind(mu1)))
                   if(all(is.na(pv))) pv <- c(0,0) # unsure why NA's can creep in here
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
               if(cpf & is.null(end)) {
                 df <- df |>
                   mutate(x = ((x - x[1]) - time *
                                 (x - x[1])[N]) + x[1]) |>
                   mutate(y = ((y - y[1]) - time *
                                 (y - y[1])[N]) + y[1])
               }
               df
             })
    }) 
    
    tmp <- bind_rows(tmp)
    tmp <- as_tibble(tmp)

    if (!sim_only) {
      loc <- grab(x[k, ], what = what, as_sf = FALSE)
      loc$rep <- 0
      loc <- loc[, c("rep", "date", "x", "y")]
      tmp <- rbind(loc, tmp)
    }
    
    ## simulated x,y are in the simulation projection; return x,y in the
    ## common output projection and lon,lat from the simulation projection
    tmp1 <- try(st_as_sf(tmp, coords = c("x","y"), crs = prj.sim), silent = TRUE)
    if(inherits(tmp1, "try-error")) {
      stop("oops something went wrong, try again", call. = FALSE)
    }

    xy <- as.data.frame(st_coordinates(st_transform(tmp1, crs = prj.out)))
    names(xy) <- c("x","y")
    ll <- st_transform(tmp1, crs = 4326)
    ll <- st_coordinates(ll)
    ll <- as.data.frame(ll)
    names(ll) <- c("lon","lat")
    st_geometry(tmp1) <- NULL
    cbind(tmp1, xy, ll)[, c("rep","date","lon","lat","x","y")]
  }) 
  
  d <- tibble(id = x$id, model = x$pmodel, sims = d)
  if(cpf | !is.null(end)) class(d) <- append("cpf", class(d))
  
  switch(unique(x$pmodel),
         rw = { 
           class(d) <- append("rws", class(d))
         },
         crw = {
           class(d) <- append("crws", class(d))
         })

  class(d) <- append("sim_fit", class(d))
  ## projection of the returned x,y (used by route_path())
  attr(d, "prj") <- if(!is.na(prj.out$proj4string) && nzchar(prj.out$proj4string)) {
    prj.out$proj4string
  } else {
    prj.out$wkt
  }
  
  return(d)
}
