##' @title plot
##'
##' @description visualize simulated tracks from a fG_sim data.frame
##'
##' @param x a \code{foieGras} simulation data.frame with class \code{fG_sim}
##' @param error logical, plot locations with error (TRUE) or without. Ignored in 1-D time-series plots
##' @param pal hcl.colors palette to use (default: "Zissou1"); type \code{hcl.pals()} for options
##' @param rev reverse direction of colour palette; logical (default = FALSE)
##' @param col colour data points by speed; logical (default = TRUE)
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of simulated tracks. Can be rendered all on a single page (pages = 1) or on separate pages (pages = 0).
##' 
##' @importFrom ggplot2 ggplot aes geom_point geom_path geom_line theme_minimal 
##' @importFrom ggplot2 element_blank xlab ylab unit scale_colour_gradientn guides guide_legend
##' @importFrom ggplot2 theme ylim coord_fixed scale_colour_manual scale_size
##' @importFrom dplyr "%>%"
##' @importFrom stringr str_split
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices hcl.colors extendrange
##' @importFrom assertthat assert_that
##' @method plot fG_sim
##'
##' @examples
##' tr <- sim(N=100, model = "crw")
##' plot(tr, error = TRUE)
##' 
##' @export

plot.fG_sim <- function(x, 
                        error = FALSE,
                        pal = "Zissou1",
                        rev = FALSE,
                        col = TRUE,
                        ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  bts <- names(x)[names(x) %in% c("g", "b")]
  if(length(bts) == 0 & !is.null(bts)) bts <- NULL
  
  model <- str_split(class(x)[2], "_", simplify = TRUE)[,2]
  
  switch(model, 
         crws = {
           x <- x %>% mutate(s = sqrt(u^2 + v^2))
         },
         rws = {
           x <- x %>% mutate(s = c(0, sqrt(diff(x)^2 + diff(y)^2)))
         },
         mpms = {
           x <- x %>% mutate(s = c(0, sqrt(diff(x)^2 + diff(y)^2))) %>%
             mutate(s = s / c(0, diff(date)))
         })
  
  if(!error) {
    p <- ggplot() +
      geom_path(data = x, aes(x, y), size = 0.2, 
                colour = grey(0.5), alpha = 0.5)
  } else {
    p <- ggplot()
  }

  if(!is.null(bts)) {
    if(!error) {
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
                 geom_point(data = x, aes(x, y), colour = grey(0.5), alpha = 0.5, size = 0.3) +
                 geom_point(data = x, aes(x+x.err, y+y.err, size = b, colour = factor(b)))
                 
             },
             g = {
               p <- p +
                 geom_point(data = x, aes(x, y), colour = grey(0.5), alpha = 0.5, size = 0.3) +
                 geom_point(data = x, aes(x+x.err, y+y.err, colour = g))
                 
             })
      
    }
    switch(bts, 
           b = {
             p <- p + scale_colour_manual(values = hcl.colors(n=2, pal), name = "b") +
               scale_size(range = c(0.75, 1.5), guide = "none") +
               guides(color = guide_legend(override.aes = list(size = c(0.75, 1.5))))
           },
           g = {
             p <- p + scale_colour_gradientn(colours = hcl.colors(n = 100, pal, rev = rev),
                                             name = "g")
           })
  
  } else if(is.null(bts)) {
    if(!error) {
      if(col) {
               p <- p +
                 geom_point(data = x, aes(x, y, colour = s)) + 
                 scale_colour_gradientn(colours = hcl.colors(n = 100, pal, rev = rev),
                                        name = "s")
             } else {
               p <- p +
                 geom_point(data = x, aes(x, y), colour = hcl.colors(n=2, pal, rev = rev)[1])
             }
      
    } else{
      if(col) {
               p <- p +
                 geom_point(data = x, aes(x, y), colour = grey(0.5), alpha = 0.5, size = 0.3) +
                 geom_point(data = x, aes(x+x.err, y+y.err, colour = s)) +
                 scale_colour_gradientn(colours = hcl.colors(n = 100, pal, rev = rev),
                                                 name = "s")
            
             } else {
               p <- p +
                 geom_point(data = x, aes(x, y), colour = grey(0.5), alpha = 0.5, size = 0.3) +
                 geom_point(data = x, aes(x+x.err, y+y.err), colour = hcl.colors(n = 2, pal, rev = rev)[1])
                 
             }
    }
    
  }
  p <- p + xlab(element_blank()) + 
    ylab(element_blank()) + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          legend.key.height = unit(0.015, units = "npc")) +
    coord_fixed()
  
  if (!is.null(bts)) {
    switch(bts, 
           b = {
             p1 <- ggplot(x) +
               geom_line(aes(x = date, y = s), colour = hcl.colors(n=2, pal, rev = rev)[1], 
                         size = 0.1) +
               geom_point(aes(x = date, y = s, colour = factor(b)), size = 0.75) +
               scale_colour_manual(values = hcl.colors(n = 2, pal),
                                   guide = "none")
               
           },
           g = {
             p1 <- ggplot(x) +
               geom_line(aes(x = date, y = s), colour = hcl.colors(n=2, pal, rev = rev)[1],
                         size = 0.1) +
               geom_point(aes(x = date, y = s, colour = g), size = 0.75) +
               scale_colour_gradientn(colours = hcl.colors(n = 100, pal, rev = rev),
                                      name = "g", guide = "none") 
      
           })
    
  } else {
    if(col) {
      p1 <- ggplot(x, aes(x = date, y = s, colour = s)) +
        geom_point(size = 0.75) +
        scale_colour_gradientn(colours = hcl.colors(n = 100, pal, rev = rev),
                               guide = "none") +
        geom_line(size = 0.1)
    } else {
      p1 <- ggplot(x, aes(x = date, y = s)) +
        geom_point(size = 0.75, colour = hcl.colors(n = 2, pal, rev = rev)[1]) +
        geom_line(size = 0.1)
    }
    
  }
  p1 <- p1 +
    xlab(element_blank()) +
    ylab("s km/h") +
    theme_minimal() #+
#    ylim(extendrange(x$s, f = 0.025))

  wrap_plots(p, p1, heights = c(3,1))

}