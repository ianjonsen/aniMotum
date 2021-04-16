##' @title plot
##'
##' @description visualize tracks simulated from a foieGras model fit
##'
##' @param x a \code{foieGras} simulation data.frame with class \code{fG_simfit}
##' @param ncol number of columns to arrange multiple plots
##' @param pal \code{hcl.colors} palette to use (default: "Zissou1"; type \code{hcl.pals()} for options)
##' @param ... additional arguments to be ignored
##' 
##' @return Plots of simulated tracks. 
##' 
##' @importFrom ggplot2 ggplot aes geom_point geom_path theme_minimal
##' @importFrom ggplot2 element_blank xlab ylab coord_fixed
##' @importFrom dplyr "%>%"
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices hcl.colors
##' @importFrom sf st_as_sf st_transform st_cast st_wrap_dateline
##' @method plot fG_simfit
##'
##' @examples
##' trs <- simfit(xs, what = "p", reps = 1)
##' plot(trs, ncol = 2)
##'
##' @export

plot.fG_simfit <- function(x, 
                           ncol = 1,
                           pal = "Zissou1",
                        ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  N <- nrow(x)
  x$sims <- lapply(x$sims, function(x) {
      st_as_sf(x, coords = c("x","y"), crs = "+proj=merc +units=km +datum=WGS84 +no_defs") %>%
        st_transform(crs = "+proj=longlat +datum=WGS84")
  })

  p <- lapply(x$sims, function(x) {
    
    sf_lines <- x %>%
      group_by(rep) %>%
      summarise(do_union = FALSE) %>%
      st_cast("MULTILINESTRING") %>%
      st_wrap_dateline()
    
    ggplot() + 
      geom_sf(
          data = sf_lines %>% filter(rep != 0),
          colour = hcl.colors(n=5, palette = pal)[1],
          size = 0.1
        ) + 
      geom_sf(
        data = x %>% filter(rep != 0),
        colour = hcl.colors(n=5, palette = pal)[1],
        size = 0.5,
        alpha = 0.4
      ) +
      geom_sf(
        data = x %>% filter(rep == 0),
        colour = hcl.colors(n=5, palette = pal)[3],
        size = 0.8
      ) +
      xlab(element_blank()) +
      ylab(element_blank()) + 
      theme_minimal()
  })

  wrap_plots(p, ncol = ncol)
}
  