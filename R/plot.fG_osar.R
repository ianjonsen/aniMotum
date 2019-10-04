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
##' @importFrom dplyr "%>%" filter
##' @method plot fG_osar
##'
##' @examples
##' ## load example osar output (to save time)
##' data(fssm)
##' fres <- osar(fssm)
##' plot(fres, "qq")
##' plot(fres, "hist")
##'
##' @export

plot.fG_osar <- function(x, type = c("qqnorm", "histogram"), bw = 0.5, ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }

  type <- match.arg(type)

  if(inherits(x, "fG_osar")) {
  
  switch(type,
         qqnorm = {
           p <- ggplot(x %>% filter(!is.na(residual)), aes(sample = residual)) +
             geom_qq() +
             geom_qq_line(col = "firebrick") +
             facet_grid(id ~ coord)
         },
         histogram = {
           p <- ggplot(x %>% filter(!is.na(residual)), aes(x = residual)) +
             geom_histogram(binwidth = bw, col = grey(0.9), lwd = 0.5) +
             geom_vline(xintercept = 0, lty = 2, col = "firebrick") +
             facet_grid(id ~ coord)
         })

  print(p)
  } else {
    stop("an fG_osar class object is required")
  }
}
