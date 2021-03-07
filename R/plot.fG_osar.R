##' @title plot
##'
##' @description plot One-Step-Ahead (prediction) residuals from a \code{foieGras osar} object
##'
##' @param x a \code{foieGras osar} object with class `fG_osar`
##' @param type type of residual plot to generate; either qqnorm (default) or histogram
##' @param bw binwidth for histogram plots (see ggplot2::geom_histogram for details), ignored if type = "qqnorm"
##' @param ... additional arguments to be ignored
##' @importFrom ggplot2 ggplot geom_qq geom_qq_line geom_histogram geom_boxplot geom_vline geom_hline
##' @importFrom ggplot2 aes facet_grid facet_wrap coord_flip
##' @importFrom wesanderson wes_palette
##' @method plot fG_osar
##'
##' @examples
##' ## load example osar output (to save time)
##' data(xs)
##' dres <- osar(xs[1, ]) # only use first seal to save time
##' plot(dres, type = "qq")
##'
##' @export

plot.fG_osar <- function(x, type = c("qqnorm", "histogram"), bw = 0.5, ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }

  type <- match.arg(type)
  wpal <- wes_palette("Zissou1", n = 5, "discrete")
  
  if(inherits(x, "fG_osar")) {
  
  switch(type,
         qqnorm = {
           x <- x[!is.na(x$residual), ]
           p <- ggplot(x, aes(sample = residual)) +
             geom_qq(colour = wpal[1]) +
             geom_qq_line(colour = wpal[4]) +
             facet_grid(id ~ coord)
         },
         histogram = {
           x <- x[!is.na(x$residual), ]
           p <- ggplot(x, aes(x = residual)) +
             geom_histogram(binwidth = bw, col = grey(0.9), lwd = 0.5, fill = wpal[1]) +
             geom_vline(xintercept = 0, colour = wpal[4]) +
             facet_grid(id ~ coord)
         })

  print(p)
  } else {
    stop("an fG_osar class object is required")
  }
}
