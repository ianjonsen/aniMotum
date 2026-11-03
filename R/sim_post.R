## Taken from RTMB:::rgmrf0
# Sampling from mean zero multivariate Gaussian using sparse precision matrix Q.
# This is much more efficient if precision in sparse but Covariance is dense.
rgmrf0 <- function (n, Q) 
{
  L <- Matrix::Cholesky(Q, super = TRUE, LDL = FALSE)
  u <- matrix(stats::rnorm(ncol(L) * n), ncol(L), n)
  u <- Matrix::solve(L, u, system = "Lt")
  u <- Matrix::solve(L, u, system = "Pt")
  as.matrix(u)
}

##' @title simulate from the posterior of a \code{ssm} fit.
##'
##' @description simulates track locations from the joint precision matrix of a 
##' \code{ssm} model fit. Currently, the joint precision of the SSM movement 
##' parameters is not included (ie. a full posterior simulation).
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
##' fit <- fit_ssm(ellie, model = "crw", time.step = 24)
##' psim <- sim_post(fit, "p", reps = 10)
##' plot(psim, type = "lines")
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
              rw = "X",
              crw = "mu",
              mp = "X",
              jmp = "X",
              jcrw = "mu")
  if (is.null(X))
    stop("sim_post() does not know which random effects hold the locations ",
         "of a `", model, "` fit", call. = FALSE)

  ## A joint fit has ONE TMB object, ONE joint precision matrix and ONE
  ## concatenated vector of random effects covering every individual, and the
  ## same object is stored on every row of the fit. Sampling it once and then
  ## slicing out each animal is both cheaper - by a factor of nrow(x) - and
  ## more correct, because the individuals are drawn from a single joint
  ## posterior and so retain the covariance the joint fit estimated between
  ## them. Sampling per row would draw each animal independently.
  jnt <- inherits(x, "jssm_df")

  n <- nrow(x)

  if (jnt) {
    sdp.j <- sdreport(x$ssm[[1]]$tmb, getJointPrecision = TRUE)
    jp.j <- sdp.j$jointPrecision            # keep sparse
    samples.j <- rgmrf0(reps, jp.j)         # no mean
    sel.j <- which(rownames(jp.j) %in% X)   # every individual's locations
    reMu.j <- sdp.j$par.random
    reMu.j <- reMu.j[names(reMu.j) %in% X]
  }

  ps <- lapply(1:n, function(k) {

    if (jnt) {
      ## this individual's slice of the joint random effects vector
      rr <- x$ssm[[k]]$ridx
      if (is.null(rr))
        stop("this fit was made before sim_post() supported joint models; ",
             "re-fit with the current fit_ssm()", call. = FALSE)
      reMu <- reMu.j[rr]
      dev <- samples.j[sel.j[rr], , drop = FALSE]

    } else {
      ## re-gen sdreport w jnt prec matrix
      sdp <- sdreport(x$ssm[[k]]$tmb, getJointPrecision = TRUE)

      ## get random parameters & subset to just locations
      reMu <- sdp$par.random
      reMu <- reMu[names(reMu) %in% X]

      # Not inverting full precision and sampling using rmvnorm on margin
      # but sampling full parameter vector (including fixed effects) using
      # RTMB:::rgmrf0 (which is efficient when precision is sparse) and then only keeping the desired margin

      jp <- sdp$jointPrecision # keeping this sparse

      # directly sample using sparse joint precision
      samples <- rgmrf0(reps, jp) # no mean
      sel <- rownames(jp) %in% X # selector variable
      dev <- samples[sel, , drop = FALSE]
    }

    # initialise with posterior mode
    rtracks <- matrix(rep(reMu, reps), nrow = reps, ncol = length(reMu), byrow = TRUE)

    # add random sampled deviations (only desired margin)
    rtracks <- rtracks + t(dev)

    ## use full joint prec matrix
    # jp <- as.matrix(sdp$jointPrecision)
    # muCov <- solve(jp) ## matrix inverse, 1/prec = varcov
    ## subset to just the location covars after inversion
    # muCov <- muCov[rownames(muCov) %in% X, colnames(muCov) %in% X]
    
    # simulate
    # rtracks <- mvtnorm::rmvnorm(reps,
    #                             mean = reMu,
    #                             sigma = muCov,
    #                             checkSymmetry = FALSE)
    
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
    
    ## Use the projection the model was actually fitted in. Hard-coding
    ## Mercator was harmless while every fit was in Mercator, but fit_ssm() now
    ## chooses a projection from the extent of the data, and respects any sf
    ## object handed to it, so the simulated x,y can be in a Lambert conformal
    ## conic or polar stereographic frame. Labelling those as Mercator puts the
    ## back-transformed lon,lat in the wrong place entirely.
    prj <- st_crs(x$ssm[[k]]$fitted)
    if (is.na(prj))
      prj <- st_crs("+proj=merc +units=km +datum=WGS84 +no_defs")

    tmp1 <- try(st_as_sf(tmp, coords = c("x","y"), crs = prj), silent = TRUE)
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

  ## the joint models take the class of the single-animal model they generalise
  cls <- switch(model,
                rw = "rwps",
                crw = "crwps",
                jcrw = "crwps",
                mp = "mpps",
                jmp = "mpps")
  if (!is.null(cls)) class(ps) <- append(cls, class(ps))
  
  class(ps) <- append("sim_post", class(ps))
  
  return(ps)
}