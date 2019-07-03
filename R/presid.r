##' @title obtain one-step-ahead (prediction) residuals from a \code{foieGras} fit
##'
##' @param x a compound \code{fG} tbl or a \code{foieGras} individual fit object
##' @param method method to calculate prediction residuals (default is "oneStepGaussianOffMode"; see `?TMB::oneStepPrediction` for details)
##' @param parallel run TMB::oneStepPredict in parallel (default is TRUE; requires \code{parallel} package)
##'
##'
##' @examples
##' ## see summary fit output
##' ## load example foieGras fit object (to save time)
##' data(fit)
##' presid(fit$ssm[[1]])
##'
##' @importFrom dplyr "%>%" select filter
##' @importFrom TMB oneStepPredict
##' @export

presid <- function(x, method = "oneStepGaussianOffMode", parallel = TRUE)
{

  if(inherits(x, "tbl_fG")) {

  } else if(inherits(x, "foieGras")) {
    sub <- which(rep(x$ssm[[1]]$isd, each = 2))
    r <- oneStepPredict(x$ssm[[1]]$tmb, "Y", "keep", method = method,
                        subset = sub, discrete = FALSE, parallel = parallel, ...)
  }
browser()


}
