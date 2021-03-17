##' @title plot
##'
##' @description plot One-Step-Ahead (prediction) residuals from a \code{foieGras osar} object
##'
##' @param x a \code{foieGras osar} object with class `fG_osar`
##' @param type type of residual plot to generate; time-series (ts), qqnorm (default, qq) or acf
##' @param bw binwidth for histogram plots (see ggplot2::geom_histogram for details), ignored if type = "qqnorm"
##' @param ... additional arguments to be ignored
##' @importFrom ggplot2 ggplot geom_qq geom_qq_line geom_segment geom_boxplot geom_hline
##' @importFrom ggplot2 aes facet_grid facet_wrap coord_flip
##' @importFrom stats acf qnorm
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

plot.fG_osar <- function(x, type = c("ts", "qqnorm", "acf"), bw = 0.5, ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }

  type <- match.arg(type)
  wpal <- wes_palette("Zissou1", n = 5, "discrete")
  
  if(inherits(x, "fG_osar")) {
  
  switch(type,
         ts = {
           p <- ggplot(x) +
             geom_point(aes(x = 1:length(residual), y = residual), shape = 19, colour = wpal[1]) +
             geom_hline(aes(yintercept = 0), lty = 2, colour = wpal[4]) +
             facet_grid(id ~ coord, scales = "free_x")
         }, 
         qqnorm = {
           x <- x[!is.na(x$residual), ]
           p <- ggplot(x, aes(sample = residual)) +
             geom_qq(colour = wpal[1]) +
             geom_qq_line(colour = wpal[4]) +
             facet_grid(id ~ coord)
         },
         acf = {
           browser()
           x <- x[!is.na(x$residual), ]
           x.acf <- acf(x[x$coord == "x", "residual"], plot = FALSE)
           y.acf <- acf(x[x$coord == "y", "residual"], plot = FALSE)
           x1 <- rbind(with(x.acf, data.frame(lag, acf, coord = rep("x", length(x.acf$lag)))), 
                       with(y.acf, data.frame(lag, acf, coord = rep("y", length(y.acf$lag))))
                       )
           cil.x <- qnorm((1 - 0.95)/2) / sqrt(nrow(x[x$coord == "x", ]))
           cil.y <- qnorm((1 - 0.95)/2) / sqrt(nrow(x[x$coord == "y", ]))
           cil <- data.frame(ci = c(cil.x,cil.y), coord = c("x","y"))
           
           p <- ggplot(x1, aes(x = lag, y = acf)) +
             geom_hline(aes(yintercept = 0), colour = wpal[4]) +
             geom_segment(aes(xend = lag, yend = 0), colour = wpal[1]) +
             geom_hline(data = cil, aes(yintercept = ci), linetype = 2, color = wpal[2]) +
             geom_hline(data = cil, aes(yintercept = -ci), linetype = 2, color = wpal[2]) +
             facet_grid( ~ coord)
         })

  print(p)
  } else {
    stop("an fG_osar class object is required")
  }
}
