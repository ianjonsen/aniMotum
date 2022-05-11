##' @title plot
##'
##' @description visualize simulated tracks from a `sim` data.frame
##'
##' @param x a `foieGras` simulation data.frame with class `sim`
##' @param type either 1, a 1-D time-series of speed (if model is `rw` or `crw` 
##' specified behavioural states) or move persistence (g; if model is `mp`); or 
##' 2 (default), a 2-D track with location coloured by move persistence (g; if 
##' model = "mp")
##' @param error logical, plot locations with error (TRUE) or without. Ignored 
##' in 1-D time-series plots
##' @param pal [grDevices::hcl.colors] palette to use (default: "Cividis"); 
##' see [grDevices::hcl.pals()] for options
##' @param rev reverse direction of colour palette; logical (default = FALSE)
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of simulated tracks. Can be rendered all on a single page 
##' (pages = 1) or on separate pages (pages = 0).
##' 
##' @importFrom ggplot2 ggplot aes geom_point geom_path geom_line theme_minimal 
##' @importFrom ggplot2 element_blank xlab ylab unit scale_colour_gradientn guides guide_legend
##' @importFrom ggplot2 theme ylim coord_fixed scale_colour_manual scale_size
##' @importFrom grDevices hcl.colors extendrange
##' @method plot sim
##'
##' @examples
##' tr <- sim(N=200, model = "mp")
##' plot(tr)
##' 
##' @export
##' @md

plot.sim <- function(x, 
                     type = 2,
                     error = FALSE,
                     pal = "Cividis",
                     rev = FALSE,
                     ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  stopifnot("x must be a sim object with class `sim`" = inherits(x, "sim"))
  
  bts <- names(x)[names(x) %in% c("g", "b")]
  if(length(bts) == 0 & !is.null(bts)) bts <- NULL

  model <- class(x)[2]
  
  if (type == 2) {
    if (!error) {
      p <- ggplot() +
        geom_path(
          data = x,
          aes(x, y),
          size = 0.2,
          colour = grey(0.5),
          alpha = 0.5
        )
    } else {
      p <- ggplot()
    }
    
    if (!is.null(bts)) {
      if (!error) {
        switch(bts,
               b = {
                 p <- p +
                   geom_point(data = x, aes(x, y, size = b, colour = factor(b)))
               },
               g = {
                 p <- p +
                   geom_point(data = x, aes(x, y, colour = g))
               })
        
      } else {
        switch(bts,
               b = {
                 p <- p +
                   geom_point(
                     data = x,
                     aes(x, y),
                     colour = grey(0.5),
                     alpha = 0.5,
                     size = 0.3
                   ) +
                   geom_point(data = x, aes(
                     x + x.err,
                     y + y.err,
                     size = b,
                     colour = factor(b)
                   ))
                 
               },
               g = {
                 p <- p +
                   geom_point(
                     data = x,
                     aes(x, y),
                     colour = grey(0.5),
                     alpha = 0.5,
                     size = 0.3
                   ) +
                   geom_point(data = x, aes(x + x.err, y + y.err, colour = g))
                 
               })
        
      }
      switch(bts,
             b = {
               p <-
                 p + scale_colour_manual(values = 
                                           hcl.colors(n = 2, pal), 
                                         name = "behavioural state") +
                 scale_size(range = c(0.75, 1.5), guide = "none") +
                 guides(color = 
                          guide_legend(override.aes = 
                                         list(size = c(0.75, 1.5))))
             },
             g = {
               p <-
                 p + scale_colour_gradientn(colours = 
                                              hcl.colors(n = 100, pal, rev = rev),
                                            name = expression(gamma[t]))
             })
      
    } else if (is.null(bts)) {
      if (!error) {
        p <- p +
          geom_point(data = x,
                     aes(x, y),
                     colour = hcl.colors(n = 2, pal, rev = rev)[1])
        
      } else {
        p <- p +
          geom_point(
            data = x,
            aes(x, y),
            colour = grey(0.5),
            alpha = 0.5,
            size = 0.3
          ) +
          geom_point(data = x,
                     aes(x + x.err, y + y.err),
                     colour = hcl.colors(n = 2, pal, rev = rev)[1])
        
      }
    }
    p <- p + xlab(element_blank()) + 
      ylab(element_blank()) + 
      theme_minimal() + 
      theme(legend.position = "bottom", 
            legend.key.height = unit(0.015, units = "npc")) +
      coord_fixed()
    
  } else if (type == 1) {
    p <- ggplot()
    
    if (!is.null(bts)) {
      switch(bts,
             b = {
               x$s <- with(x, sqrt(u^2 + v^2))
               x$b <- factor(x$b)
               p <- p +
                 geom_line(data = x, aes(date, s), size = 0.3, colour = grey(0.6)) +
                 geom_point(data = x, aes(date, s, colour = b)) +
                 scale_colour_manual(values = 
                                       hcl.colors(n = 2, pal), 
                                     name = "behavioural state") +
                 scale_size(range = c(0.75, 1.5), guide = "none") +
                 guides(color = 
                          guide_legend(override.aes = 
                                         list(size = c(0.75, 1.5)))) +
                 ylab("Speed (km/h)")
                 
             },
             g = {
               p <- p +
                 geom_line(data = x, aes(date, g), size = 0.3, colour = grey(0.6)) +
                 geom_point(data = x, aes(date, g, colour = g)) +
                 scale_colour_gradientn(colours = hcl.colors(n = 100, pal, rev = rev),
                                          name = expression(gamma[t])) +
                 ylim(0, 1) +
                 ylab(expression(gamma[t]))
             })
    }
    p <- p +
      xlab(element_blank()) + 
      theme_minimal() + 
      theme(legend.position = "bottom", 
            legend.key.height = unit(0.015, units = "npc"))
    
  }
  
  


p

}