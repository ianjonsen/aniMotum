##' @title plot
##'
##' @description visualize tracks simulated from a foieGras model fit
##'
##' @param x a \code{foieGras} simulation data.frame with class \code{fG_simfit}
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of simulated tracks. 
##' 
##' @importFrom ggplot2 ggplot aes geom_point geom_path theme_minimal
##' @importFrom ggplot2 element_blank xlab ylab coord_fixed scale_colour_manual
##' @importFrom dplyr "%>%"
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices hcl.colors
##' @method plot fG_simfit
##'
##' @examples
##'
##' @export

plot.fG_simfit <- function(x, 
                           pal = "Zissou1",
                           ncol = 2,
                        ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  N <- nrow(x)
  
  p <- lapply(x$sims, function(x) {
    x$lon <- ifelse(x$lon < 0, x$lon + 360, x$lon)
    ggplot() + 
      geom_path(
          data = x %>% filter(rep != 0),
          aes(lon, lat, col = factor(rep)),
          size = 0.2,
          alpha = 0.75
        ) + 
      geom_point(
        data = x %>% filter(rep != 0),
        aes(lon, lat, col = factor(rep)),
        size = 0.4,
        alpha = 0.75
      ) +
      geom_point(
        data = x %>% filter(rep == 0),
        aes(lon, lat),
        size = 0.8,
        alpha = 0.85
      ) +
      scale_colour_manual(values = hcl.colors(n = max(x$rep), palette = pal),
                          name = "replicate") +
      xlab(element_blank()) +
      ylab(element_blank())
  })

  wrap_plots(p, guides = "collect", ncol = ncol) &
    coord_fixed() & 
    theme_minimal()
  
}
  