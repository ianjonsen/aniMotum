##' @title simulate from the posterior of a \code{ssm} fit, conditional on 
##' parameters and data.
##'
##' @description ... 
##' @param x a \code{ssm} fit object with class `ssm_df`
##' @param what simulate fitted or predicted locations
##' @param reps number of replicate tracks to simulate from the \code{ssm} model 
##' fit object
##' @param sim_only logical, do not include \code{ssm} estimated locations in 
##' output (default is FALSE)
##' 
##' @return a \code{fG_sim_post} object containing the paths simulated from a 
##' \code{ssm} fit object
##' 
##' @examples 
##' fit <- fit_ssm(sese2, model = "crw", time.step = 12)
##' psims <- sim_post(fit[2,], "p", reps = 500)
##' plot(y ~ x, grab(fit[2,], "p"), pch = NA)
##' sapply(1:length(psims), function(i) lines(psims[[i]], lwd=0.25, col = rgb(0.7,0.7,0.7,0.4)))
##' points(y ~ x, grab(fit[2,], "p"), pch = 19, cex=0.3, col="red")
##' points(y ~ x, grab(fit[2,], "d"), pch = 19, cex=0.3, col="orange")
##' 
##' @importFrom TMB sdreport
##' @importFrom dplyr mutate select bind_rows
##' @export

sim_post <- function(x, 
                     what = "predicted",
                     reps = 1,
                     sim_only = FALSE
                     ) {
  
  what <- match.arg(what, choices = c("predicted", "fitted"))
  model <- x$pmodel[1]
  X <- switch(model,
              rw = {
                "X"
              },
              crw = {
                "mu"
              }, 
              mp = {
                "X"
              })

  n <- nrow(x)

  ps <- lapply(1:n, function(k) {
  
    ## re-gen sdreport w jnt prec matrix
    sdp <- sdreport(x$ssm[[k]]$tmb, getJointPrecision = TRUE)
  
    ## get random parameters & subset to just locations
    reMu <- sdp$par.random
    reMu <- reMu[names(reMu) %in% X]

    ## use full joint prec matrix
    jp <- as.matrix(sdp$jointPrecision)
    muCov <- solve(jp) ## matrix inverse, 1/prec = varcov
    ## subset to just the location covars after inversion
    muCov <- muCov[rownames(muCov) %in% X, colnames(muCov) %in% X]
    
    # simulate
    rtracks <- mvtnorm::rmvnorm(reps,
                                mean = reMu,
                                sigma = muCov,
                                checkSymmetry = FALSE)
    
    ## what are we simulating? fitted or predicted locations?
    ## use obs index to subset simulated locs - do after sim so
    ## full covar structure is preserved
    obs <- switch(what,
                  fitted = {
                    rep(x$ssm[[k]]$isd, each = 2)
                  },
                  predicted = {
                    rep(!x$ssm[[k]]$isd, each = 2)
                  })
    
    ## return as a list (prob less efficient than array...)
    tmp <- lapply(1:reps, function(j) {
      ## obs - subset to just fitted or predicted locs
      as.data.frame(matrix(rtracks[j, obs],
             nrow = sum(obs) / 2,
             ncol = 2,
             byrow = TRUE)) %>%
        mutate(rep = j) %>%
        select(rep, x = V1, y = V2)
    }) %>%
      bind_rows()
    
    
   
    if (!sim_only) {
      loc <- grab(x[k, ], what = what, as_sf = FALSE)
      loc$rep <- 0
      loc <- loc[, c("rep", "date", "x", "y")]
      tmp$date <- rep(loc$date, reps)
      tmp <- tmp[, c("rep", "date", "x", "y")]
      tmp <- rbind(loc, tmp)
    } else {
      date <- grab(x[k, ], what = what)$date
      tmp$date <- rep(date, reps)
    }
    
    tmp1 <- try(st_as_sf(tmp, coords = c("x","y"), 
                         crs = "+proj=merc +units=km +datum=WGS84"), silent = TRUE)
    if(inherits(tmp1, "try-error")) {
      stop("oops something went wrong, try again", call. = FALSE)
    }
    
    xy <- as.data.frame(st_coordinates(tmp1))
    names(xy) <- c("x","y")
    ll <- st_transform(tmp1, crs = 4326)
    ll <- st_coordinates(ll)
    ll <- as.data.frame(ll)
    names(ll) <- c("lon","lat")
    st_geometry(tmp1) <- NULL
    cbind(tmp1, xy, ll)[, c("rep","date","lon","lat","x","y")]
  })
  
  ps <- tibble(id = x$id, model = x$pmodel, psims = ps)

  switch(unique(x$pmodel),
         rw = { 
           class(ps) <- append("rwps", class(ps))
         },
         crw = {
           class(ps) <- append("crwps", class(ps))
         },
         mp = {
           class(ps) <- append("mpps", class(ps))
         })
  
  class(ps) <- append("sim_post", class(ps))
  
  return(ps)
}