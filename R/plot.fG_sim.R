##' @title plot
##'
##' @description visualize simulated tracks from a fG_sim data.frame
##'
##' @param x a \code{foieGras} simulation data.frame with class \code{fG_sim}
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of simulated tracks. Can be rendered all on a single page (pages = 1) or on separate pages (pages = 0).
##' 
##' @importFrom ggplot2 ggplot aes geom_point geom_path geom_line theme_minimal 
##' @importFrom ggplot2 element_blank xlab ylab unit scale_colour_gradientn 
##' @importFrom ggplot2 theme ylim
##' @importFrom dplyr "%>%"
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices hcl.colors extendrange
##' @importFrom assertthat assert_that
##' @method plot fG_sim
##'
##' @examples
##'
##' @export

plot.fG_sim <- function(x, 
                        pal = rev(hcl.colors(n=100, "Zissou1")), 
                        ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  bts <- if (any(names(x) %in% c("g", "b", "u", "v")))
    names(x)[names(x) %in% c("g", "b", "u", "v")]
  browser()
  if(!is.null(bts) & any(bts %in% c("g","b"))) {
    p <- ggplot(x, aes(x, y, colour = eval(parse(text = bts)))) +
      geom_path(size = 0.25) +
      geom_point() +
      scale_colour_gradientn(colours = pal,
                             name = bts) +
      xlab(element_blank()) +
      ylab(element_blank()) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.key.height = unit(0.015, units = "npc"))
    
  } else if (!is.null(bts) & all(bts %in% c("u","v"))) {
    x <- x %>% mutate(s = sqrt(u^2 + v^2))
    p <- ggplot(x, aes(x, y, colour = s)) +
      geom_path(size = 0.25) +
      geom_point() +
      scale_colour_gradientn(colours = pal,
                             name = "s km/h") +
      xlab(element_blank()) +
      ylab(element_blank()) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.key.height = unit(0.015, units = "npc"))
    
  }
  
  ## 1-D ts plot
  if (!is.null(bts) & any(bts %in% c("g", "b"))) {
    p1 <- ggplot(x, aes(
      x = date,
      y = eval(parse(text = bts)),
      colour = eval(parse(text = bts))
    )) +
      geom_line(size = 0.25) +
      geom_point(size = 0.75) +
      scale_colour_gradientn(colours = pal,
                             guide = "none") +
      xlab(element_blank()) +
      ylab(eval(bts)) +
      theme_minimal()
    
    if (bts[1] == "g")
      p1 <- p1 + ylim(0, 1)
    
    wrap_plots(p, p1, heights = c(4,1))
    
  } else if (!is.null(bts) & all(bts %in% c("u", "v"))) {
    p1 <- ggplot(x, aes(x = date, y = s, colour = s)) +
      geom_line(size = 0.25) +
      geom_point(size = 0.75) +
      scale_colour_gradientn(colours = pal,
                             guide = "none") +
      xlab(element_blank()) +
      ylab("s km/h") +
      theme_minimal()
    er <- extendrange(x$s, f = 0.025)
    p1 <- p1 + ylim(er[1], er[2])
    
    wrap_plots(p, p1, heights = c(4,1))
    
  } else {
    return(p)
  }
}