##' @title rtnorm
##'
##' @details simulate values from a truncated normal distribution
##' @param n number of random values to generate
##' @param mean vector of means
##' @param sd vector of standard deviations
##' @param a lower limit of distribution
##' @param b upper limit of distribution
##'
##' @examples
##' x <- rtnorm(10, a = -5, b = 5)
##' range(x)
##' @export
##' @keywords internal

rtnorm <- function(n, mean = 0, sd = 1, a = -Inf, b = Inf) {
  U <- runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd))
  qnorm(U, mean, sd) 
}