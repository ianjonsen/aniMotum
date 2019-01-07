##' @title fit diagnostic plot with lc values
##' @export
diag.plt <- function(fit) {
  par(mfrow=c(2,1), mar=c(2,4,1,1))
  plot(x ~ date, fit$data,subset=keep, col=grey(0.6), type='n',main=paste0("id: ",fit$data$id[1]))
  with(subset(fit$data, keep), text(date, x, labels=lc, cex=0.6))
  points(x ~ date, fit$predicted, pch=19,cex=0.4,col='red',type='b')
  lines(I(x-2*x.se) ~ date, fit$predicted, col='red', lwd=0.5)
  lines(I(x+2*x.se) ~ date, fit$predicted, col='red', lwd=0.5)

  plot(y ~ date, fit$data,subset=keep, col=grey(0.6), type='n')
  with(subset(fit$data, keep), text(date, y, labels=lc, cex=0.6))
  points(y ~ date, fit$predicted, pch=19,cex=0.4,col='red',type='b')
  lines(I(y-2*y.se) ~ date, fit$predicted, col='red', lwd=0.5)
  lines(I(y+2*y.se) ~ date, fit$predicted, col='red', lwd=0.5)
}
