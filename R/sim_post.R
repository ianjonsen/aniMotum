##' @title simulate from the posterior of a \code{ssm} fit, conditional on parameters and data
##'
##' @description ... 
##' @param x a \code{ssm} fit object with class `ssm_df`
##' @param reps number of replicate tracks to simulate from an \code{ssm} model 
##' fit object
##' @param what simulate fitted or predicted locations
##' 
##' @return a \code{fG_sim_post} object containing the paths simulated from a 
##' \code{ssm} fit object
##' 
##' 
##' @examples 
##' fit <- fit_ssm(sese2, model = "crw", time.step = 12)
##' psims <- sim_post(fit[2,], reps = 500, what = "pred")
##' plot(y ~ x, grab(fit[2,], "pred"), pch = NA)
##' sapply(1:length(psims), function(i) lines(psims[[i]], lwd=0.25, col = rgb(0.7,0.7,0.7,0.4)))
##' points(y ~ x, grab(fit[2,], "p"), pch = 19, cex=0.3, col="red")
##' points(y ~ x, grab(fit[2,], "d"), pch = 19, cex=0.3, col="orange")
##' 
##' @importFrom TMB sdreport
##' @export

sim_post <- function(x, 
                     reps = 1,
                     what = "predicted") {
  
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
  
  
  ## MV Normal random variate fn
  ## FIXME:: can't get this fn to work properly as it extremely under-represents
  ##          track uncertainty. Not sure why, but keeping here for now & using
  ##          mvtnorm::rmvnorm() instead
  rmvnorm_prec <- function(nsims, mu, chol_prec) {
    z <- matrix(rnorm(length(mu) * nsims), ncol=nsims)
    L <- chol_prec 
    z <- Matrix::solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
    z <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
    z <- as.matrix(z)
    mu + z
  }

  ## re-gen sdreport w jnt prec matrix
  sdp <- TMB::sdreport(x$ssm[[1]]$tmb, getJointPrecision = TRUE)
  
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
                  rep(x$ssm[[1]]$isd, each = 2)
                },
                predicted = {
                  rep(!x$ssm[[1]]$isd, each = 2)
                })
  
  ## return as a list (prob less efficient that array...)
  rtracks <- lapply(1:reps, function(i) {
    ## obs - subset to just fitted or predicted locs
    matrix(rtracks[i, obs],
           nrow = sum(obs) / 2, 
           ncol = 2,
           byrow = TRUE)
  })
  
  return(rtracks)
}