##' Print \code{ctrw}
##'
##' @method print foieGras
##'
##' @param x a \code{ctrw} fit object
##' @param digits number of digits to use in display
##' @param ... unused. For compatibility with the generic method.
##'
##' @export

print.foieGras <- function(x, digits = 3, ...)
{
  pm <- x$pm
  nbrStates <- nrow(x$predicted)
  nbStates <- nrow(x$fitted)
  parm <- x$par

  cat("negative log-likelihood:", x$opt$objective, "\n")
  cat("convergence:", x$opt$message, "\n\n")
  cat("Process model:", pm, "\n")
  cat("number of observations:", nbStates, "\n")
  cat("number of regularised state estimates:", nbrStates, "\n\n")
  cat("parameter estimates\n")
  cat("-------------------\n")
  print(parm, digits = digits, justify = "right")

}
