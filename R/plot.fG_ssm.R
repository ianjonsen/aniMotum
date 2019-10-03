##' @title plot
##'
##' @description visualise multiple fits from an fG compound tibble
##'
##' @param x a \code{foieGras} ssm fit object with class `fG_ssm`
##' @param what specify which location estimates to display on time-series plots: fitted or predicted
##' @param type of plot to generate: 1-d time series for lon and lat separately (type = 1, default) or 2-d track plot (type = 2)
##' @param ncol number of columns to use for facetting. Default is ncol = 1 but this may be increased for large compound fit objects
##' @param outlier include all extreme outliers flagged by prefilter in plots (logical)
##' @param ... additional arguments to be ignored
##' @importFrom ggplot2 ggplot geom_point geom_path aes_string ggtitle theme_bw theme element_blank geom_rug geom_path
##' @importFrom ggplot2 element_text xlab scale_colour_brewer theme_dark labeller label_both label_value
##' @importFrom tidyr gather
##' @importFrom dplyr "%>%" select bind_cols rename filter
##' @importFrom tibble enframe
##' @method plot fG_ssm
##'
##' @examples
##' ## load example foieGras fit object (to save time)
##' data(ssm_fits)
##' plot(ssm_fits, what = "f", type = 1)
##' plot(ssm_fits, what = "p", type = 2)
##'
##' @export

plot.fG_ssm <- function(x, what = c("fitted","predicted"), type = 1, ncol = 1, outlier = FALSE, ...)
{
  
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  what <- match.arg(what)
  
  if(inherits(x, "fG_ssm")) {
    switch(what,
           fitted = {
             ssm <- grab(x, "fitted", as_sf = FALSE)
           },
           predicted = {
             ssm <- grab(x, "predicted", as_sf = FALSE)
           })
    
    d <- grab(x, "data", as_sf = FALSE) %>%
      mutate(lc = factor(lc, levels=c("3","2","1","0","A","B","Z"), ordered=TRUE))
    if(!outlier) d <- d %>% filter(keep)
    
    if(type == 1) {
      foo <- ssm %>% select(id, x, y) %>% gather(., key = "coord", value = "value", x, y)
      bar <- rep(ssm$date, 2) %>% enframe(name = NULL) %>% rename(date = "value")
      
      foo.d <- d %>% select(id, x, y) %>% gather(., key = "coord", value = "value", x, y)
      bar.d <- d %>% select(date, lc) %>% bind_rows(., .)
      
      pd <- bind_cols(foo, bar) %>%
        select(id, date, coord, value)
      dd <- bind_cols(foo.d, bar.d) %>%
        select(id, date, lc, coord, value)
      
      p <- ggplot(pd, aes(date, value)) + 
        geom_point(data = dd, aes(date, value), colour = "dodgerblue",
                            alpha = 0.7, size = 1.25) + 
        geom_point(col="firebrick", size = 0.6) + 
        geom_rug(data = dd, aes(date), col = "dodgerblue", alpha=0.75, sides = "b") + 
        facet_wrap(id ~ coord, scales = "free", ncol = ncol,
                   labeller = labeller(id = label_both, coord = label_value))

      
    } else if (type == 2) {
      
      p <- ggplot() + geom_point(data = d, aes(x, y), colour = "dodgerblue",
                              size = 1.25, alpha = 0.7)
      
      p <- p + geom_path(data = ssm, aes(x, y), col = "firebrick", alpha = 0.5, lwd = 0.25) +
        geom_point(data = ssm, aes(x, y), col = "firebrick", alpha = 0.5, size = 0.6) + 
        facet_wrap( ~ id, scales = "free", ncol = ncol, labeller = labeller(id = label_both))
      
    }
    return(p)
    
  } else {
    stop("x must be a fG_ssm compound tibble")
  }
}
