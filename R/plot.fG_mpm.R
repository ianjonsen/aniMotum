##' @title plot
##'
##' @description visualize fits from an fG_mpm object
##'
##' @param x a \code{foieGras} \code{mpm} fit object with class \code{fG_mpm}
##' @param y optional \code{ssm} fit object with class \code{fG_ssm} corresponding to x. If absent, 1-d plots of \code{gamma_t} time series are rendered 
##' otherwise, 2-d track plots with locations coloured by \code{gamma_t} are rendered.
##' @param pages plots of all individuals on a single page (pages = 1; default) or each individual on a separate page (pages = 0) 
##' @param asp used a fixed 1:1 aspect ratio for 2-d track plots (asp = 1), or allow aspect ratio to vary between plots (asp = 0; default). 
##' Ignored if \code{y} is NULL and/or pages = 0
##' @param ncol number of columns to use for faceting. Default is ncol = 1 but this may be increased for multi-individual objects. Ignored if pages = 0
##' @param ... additional arguments to be ignored
##' 
##' @return a ggplot object with either: 1-d time series of \code{gamma_t} estimates (if y not provided), with estimation uncertainty ribbons (95 % CI's); 
##' or 2-d track plots (if y provided) coloured by \code{gamma_t}, with smaller points having greater uncertainty (size is proportional to \code{SE^-2}). 
##' Plots can be rendered all on a single page (pages = 1) or on separate pages.
##' 
##' @importFrom ggplot2 ggplot geom_point geom_path theme_minimal labs coord_fixed scale_size
##' @importFrom ggplot2 element_blank xlab ylab geom_ribbon facet_wrap
##' @importFrom stats qlogis
##' @importFrom dplyr "%>%"
##' @importFrom patchwork wrap_plots
##' @importFrom wesanderson wes_palette
##' @method plot fG_mpm
##'
##' @examples
##' # plot mpm fit object
##' # 1-d time-series plots
##' plot(xm) 
##' # 2-d track plots by adding ssm fit object
##' plot(xm, xs) 
##'
##' @export

plot.fG_mpm <- function(x, y = NULL, pages = 1, asp = 0, ncol = 1, ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  wpal <- wes_palette("Zissou1", n = 5, "discrete")
  
  
  if(inherits(x, "fG_mpm") & (inherits(y, "fG_ssm") | is.null(y))) {
    d <- grab(x, as_sf = FALSE)
    
    if(is.null(y)) {
   p <- ggplot(d) +
        geom_ribbon(aes(date, ymin = plogis(qlogis(g) - 1.96 * g.se), ymax = plogis(qlogis(g) + 1.96 * g.se)), fill = grey(0.5), alpha = 0.25) +
        geom_point(aes(date, g, colour = g)) + 
        facet_wrap(~ id, scales = "free_x", ncol = ncol) +
        scale_colour_gradientn(colours = rev(wes_palette(name = "Zissou1", 
                                                     type = "continuous")),
                               limits = c(0,1),
                               name = expression(gamma[t])
                               ) +
     ylab(expression(gamma[t])) +
     xlab(element_blank()) +
     ylim(0,1) +
     theme_minimal()
        
   return(p)
    } else if(!is.null(y)) {
      if(nrow(grab(y, "predicted")) != nrow(grab(x, "fitted"))) {
        if(nrow(grab(y, "fitted")) != nrow(grab(x, "fitted"))) {
          stop("x and y hav unequal numbers of estimated values")
        } else {
          xy <- join(y, x, what.ssm = "fitted", as_sf = FALSE) %>%
            split(., .$id)
        }
      } else {
        xy <- join(y, x, as_sf = FALSE) %>%
          split(., .$id)
      }
      
      p <- lapply(xy, function(x) {
        m <- ggplot(x) +
          geom_path(aes(lon, lat), size = 0.1, col = wpal[1], alpha = 0.5) +
          geom_point(aes(lon, lat, colour = g, size = g.se^-2), show.legend = c("colour" = TRUE, "size" = FALSE)) +
          scale_colour_gradientn(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                                 colours = rev(wes_palette(name = "Zissou1", 
                                                           type = "continuous")),
                                 limits = c(0,1), name = expression(gamma[t])) +
          labs(title = paste("id:", unique(x$id))) +
          xlab(element_blank()) +
          ylab(element_blank()) +
          theme_minimal() +
          scale_size(range = c(0.01, 2))
        m
      })
     
      if(pages == 1 & asp == 1) {
        wrap_plots(p, ncol = ncol, heights = rep(2, ceiling(length(p)/ncol)), guides = "collect") &
        coord_fixed()
      } else if(pages == 1 & asp == 0) {
        wrap_plots(p, ncol = ncol, guides = "collect")
      }
      else if(pages == 0) return(p)
    }
    
  } else {
    stop("x must be a fG_mpm tibble and y must be a fG_ssm tibble or NULL")
  }
}