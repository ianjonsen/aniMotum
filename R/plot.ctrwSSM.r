##' Visualise ctrw SSM fit to track data
##'
##' @title plot
##' @param m a fitted object of class ctrwSSM
##' @importFrom ggplot2 ggplot geom_point geom_qq aes ggtitle theme_bw
##' @importFrom gridExtra grid.arrange
##' @method plot ctrwSSM
##' @export

plot.ctrwSSM <- function(m)
{
  dat <- subset(m$data, keep)
  ndat <- subset(m$data, !keep)

  p1 <- ggplot() +
    geom_point(data = dat, aes(x, y), shape = 19, col = grey(0.85)) +
    geom_point(data = fit$fitted, aes(x, y), size = 0.4, shape = 20, col = "dodgerblue") +
    theme_bw() +
    ggtitle("Fitted values")

  p2 <- ggplot() +
    geom_point(data = dat, aes(x, y), shape = 19, col = grey(0.85)) +
    geom_point(data = fit$predicted, aes(x, y), size = 0.4, shape = 20, col = "dodgerblue") +
    theme_bw() +
    ggtitle("Predicted values")

  p3 <- ggplot() +
    geom_point(data = dat, aes(date, x), shape = 19, col = grey(0.85)) +
    geom_point(data = fit$fitted, aes(date, x), size = 0.2, shape = 20, col = "dodgerblue") +
    theme_bw()

  p4 <- ggplot() +
    geom_point(data = dat, aes(date, y), shape = 19, col = grey(0.85)) +
    geom_point(data = fit$fitted, aes(date, y), size = 0.2, shape = 20, col = "dodgerblue") +
    theme_bw()


  grid.arrange(p1, p2, p3, p4, layout_matrix = matrix(
    c(1, 2, 1, 2, 3, 3, 4, 4),
    nrow = 4,
    ncol = 2,
    byrow = T
    ))
}
