##' @title print \code{aniMotum} fit object summary information
##'
##' @method print ssm
##'
##' @aliases print
##' @param x a aniMotum ssm fit object
##' @param ... unused. For compatibility with the generic method.
##'
##' @examples
##' ## see summary fit output
##' ## generate a ssm fit object (call is for speed only)
##' xs <- fit_ssm(ellie, spdf=FALSE, model = "rw", time.step=24, 
##' control = ssm_control(se = FALSE, verbose = 0))
##' 
##' xs$ssm[[1]]
##'
##' @export

print.ssm <- function(x, ...)
{
  pm <- x$pm
  timeStep <- ifelse(length(x$ts) == 1, x$ts, "multiple time.steps")
  nobs.tot <- nrow(x$data)
  nobs.pf <- sum(x$data$keep)
  n.fit <- nrow(x$fitted)
  n.pred <- nrow(x$predicted)
  parm <- x$par
  if("objective" %in% names(x$opt)) {
    nll <- x$opt$objective
  } else{
    nll <- x$opt$value
  }
  cat("Process model:", pm, "\n")
  cat("Time interval:", timeStep, if(is.numeric(timeStep)) {"hours"}, "\n")
  cat("number of original observations:", nobs.tot, "\n")
  cat("number of observations fitted by ssm:", nobs.pf, "\n")
  cat("number of fitted states:", n.fit, "\n")
  cat("number of predicted states:", n.pred, "\n\n")
  cat("parameter estimates\n")
  cat("-------------------\n")
  print(round(parm, 5), justify = "right")
  cat("-------------------\n")
  cat("negative log-likelihood:", nll, "\n")
  cat("convergence:", ifelse(x$opt$convergence==0, "yes", "no"), "\n\n")
      

}
