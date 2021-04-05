##' @title rtnorm
##'
##' @details simulate values from a truncated normal distribution
##' @param n number of random values to generate
##' @param mean vector of means
##' @param sd vector of standard deviations
##' @param l lower limit of distribution
##' @param u upper limit of distribution
##'
##' @importFrom stats pnorm qnorm runif
##' @examples
##' x <- rtnorm(10, l = -5, u = 5)
##' range(x)
##' @export
##' @keywords internal

rtnorm <- function(n, mean = 0, sd = 1, l = -Inf, u = Inf) {
  x <- runif(n, pnorm(l, mean, sd), pnorm(u, mean, sd))
  qnorm(x, mean, sd) 
}