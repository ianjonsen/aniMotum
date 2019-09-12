##' @title plot
##'
##' @description visualise multiple fits from an fG compound tibble
##'
##' @param x a fG compound tibble
##' @param what specify which location estimates to display on time-series plots: fitted or predicted
##' @param outlier include all extreme outliers flagged by prefilter in plots (logical)
##' @param ... additional arguments to be ignored
##' @importFrom ggplot2 ggplot geom_point geom_path aes_string ggtitle theme_bw theme element_blank
##' @importFrom ggplot2 element_text xlab
##' @importFrom gridExtra grid.arrange
##' @importFrom magrittr "%>%"
##' @method plot fG
##'
##' @examples
##' ## load example foieGras fit object (to save time)
##' data(fit)
##' plot(fit$ssm[[1]])
##'
##' @export

plot.fG <- function(x, what = c("fitted","predicted"), outlier = FALSE, ...)
{

  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }

  what <- match.arg(what)

  if(inherits(x, "fG")) {
    f <- grab(x, "fitted", as_sf = FALSE)
    p <- grab(x, "predicted", as_sf = FALSE)
    d <- grab(x, "data", as_sf = FALSE)

    browser()

  } else {
    stop("x must be an `fG` compound tibble")
  }
}
