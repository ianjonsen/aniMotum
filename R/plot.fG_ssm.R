##' @title plot
##'
##' @description visualize multiple fits from an fG compound tibble
##'
##' @param x a \code{foieGras} ssm fit object with class `fG_ssm`
##' @param what specify which location estimates to display on time-series plots: fitted or predicted
##' @param type of plot to generate: 1-d time series for lon and lat separately (type = 1, default) or 2-d track plot (type = 2)
##' @param ncol number of columns to use for faceting. Default is ncol = 1 but this may be increased for multi-individual fit objects
##' @param ... additional arguments to be ignored
##' 
##' @return a ggplot object with either: (type = 1) 1-d time series of fits to data, 
##' separated into x and y components (units = km) with prediction uncertainty ribbons (2 x SE); 
##' or (type = 2) 2-d fits to data (units = km)
##' 
##' @importFrom ggplot2 ggplot geom_point geom_path aes_string ggtitle geom_rug
##' @importFrom ggplot2 element_text xlab scale_colour_brewer labeller label_both label_value geom_ribbon
##' @importFrom tidyr gather
##' @importFrom dplyr "%>%" select bind_cols rename
##' @importFrom tibble enframe
##' @method plot fG_ssm
##'
##' @examples
##' ## load example foieGras fit object (to save time)
##' data(fssm)
##' plot(fssm, what = "f", type = 1)
##' plot(fssm, what = "p", type = 2)
##'
##' @export

plot.fG_ssm <- function(x, what = c("fitted","predicted"), type = 1, ncol = 1, ...)
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
             if(any(sapply(x$ssm, function(.) is.na(.$ts)))) {
               stop("\n there are no predicted locations because you used time.step = NA when calling `fit_ssm`")
             } else {
             ssm <- grab(x, "predicted", as_sf = FALSE)
             }
           })
    
    d <- grab(x, "data", as_sf = FALSE) %>%
      mutate(lc = factor(lc, levels=c("3","2","1","0","A","B","Z"), ordered=TRUE))
    
    if(type == 1) {
      
      foo <- ssm %>% select(id, x, y) %>% gather(., key = "coord", value = "value", x, y)
      foo.se <- ssm %>% select(x.se, y.se) %>% gather(., key = "coord.se", value = "se", x.se, y.se)
      bar <- rep(ssm$date, 2) %>% enframe(name = NULL) %>% rename(date = "value")
      
      foo.d <- d %>% select(id, x, y) %>% gather(., key = "coord", value = "value", x, y)
      bar.d <- d %>% select(date, lc) %>% bind_rows(., .)
      
      pd <- bind_cols(foo, foo.se, bar) %>% select(id, date, coord, value, se)
      dd <- bind_cols(foo.d, bar.d) %>% select(id, date, lc, coord, value)
         
      p <- ggplot() + 
        geom_point(data = dd, aes(date, value), colour = "dodgerblue",
                            alpha = 0.7, size = 1.25) + 
        geom_ribbon(data = pd, aes(date, ymin = value - 2 * se, ymax = value + 2 * se), fill="firebrick", alpha = 0.25) + 
        geom_point(data = pd, aes(date, value), col="firebrick", size = 0.6) + 
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
