##' @title print \code{foieGras} object summary information
##'
##' @method print foieGras
##'
##' @param x a \code{ctrw} fit object
##' @param ... unused. For compatibility with the generic method.
##'
##' @examples
##' ## see summary fit output
##' ## load example foieGras fit object (to save time)
##' data(fit)
##' fit
##'
##' @export

print.foieGras <- function(x, ...)
{
  pm <- x$pm
  timeStep <- x$ts
  nbrStates <- nrow(x$predicted)
  nbStates <- nrow(x$fitted)
  parm <- x$par

  cat("Process model:", pm, "\n")
  cat("Time interval:", timeStep, "hours \n")
  cat("number of observations:", nbStates, "\n")
  cat("number of regularised state estimates:", nbrStates, "\n\n")
  cat("parameter estimates\n")
  cat("-------------------\n")
  print(round(parm, 3), justify = "right")
  cat("-------------------\n")
  cat("negative log-likelihood:", x$opt$objective, "\n")
  cat("convergence:", x$opt$message, "\n\n")

}
