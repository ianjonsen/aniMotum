##' @importFrom stats pnorm
##' @importFrom dplyr %>%
##' @method summary ctrwSSM
##' @export
summary.ctrwSSM <- function(x, digits = 3, ...) {
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }

  mmod <- class(x$data)[2]
  nbrStates <- nrow(x$predicted)
  nbStates <- nrow(x$fitted)
  parm <- x$par
  resid <- list(x = x$fitted$x - subset(x$data, keep)$x,
                y = x$fitted$y - subset(x$data, keep)$y)

  cat("convergence:", x$opt$message, "\n")
  cat("negative log-likelihood:", x$opt$objective, "\n")
  cat("AIC:", x$aic, "\n\n")

  cat("number of observations:", nbStates, "\n")
  cat("number of regular state estimates:", nbrStates, "\n")
  cat("time interval:", x$ts, "hours\n\n")
  cat("measurement error model: Argos", mmod, "\n")
  cat("parameter estimates\n")
  cat("-------------------------\n")
  print(parm, digits = digits, justify = "right");cat("\n")

 # cat("quantiles of residuals: x\n")
 # cat("----------------------------\n")
 # print(quantile(resid$x), digits = digits); cat("\n")

 # cat("quantiles of residuals: y\n")
 # cat("----------------------------\n")
 # print(quantile(resid$y), digits = digits); cat("\n")


}
