##' @title plot
##'
##' @description visualize fits from an fG_mpm object
##'
##' @param x a \code{foieGras} \code{mpm} fit object with class \code{fG_mpm}
##' @param y optional \code{ssm} fit object with class \code{fG_ssm} corresponding to x. If absent, 1-d plots of \code{gamma_t} time series are rendered 
##' otherwise, 2-d track plots with locations coloured by \code{gamma_t} are rendered.
##' @param se logical (default = FALSE); should points be scaled by \code{gamma_t} uncertainty (ignored if y is not supplied)
##' @param pages plots of all individuals on a single page (pages = 1; default) or each individual on a separate page (pages = 0) 
##' @param ncol number of columns to use for faceting. Default is ncol = 1 but this may be increased for multi-individual objects. Ignored if pages = 0
##' @param ask logical; if TRUE (default) user is asked for input before each plot is rendered. set to FALSE to return ggplot objects
##' @param pal \code{hcl.colors} palette to use (default: "Zissou1"; type \code{hcl.pals()} for options)
##' @param rev reverse colour palette (logical)
##' @param ... additional arguments to be ignored
##' 
##' @return a ggplot object with either: 1-d time series of \code{gamma_t} estimates (if y not provided), with estimation uncertainty ribbons (95 % CI's); 
##' or 2-d track plots (if y provided) coloured by \code{gamma_t}, with smaller points having greater uncertainty (size is proportional to \code{SE^-2}, if se = TRUE). 
##' Plots can be rendered all on a single page (pages = 1) or on separate pages.
##' 
##' @importFrom ggplot2 ggplot geom_point geom_path theme_minimal labs coord_fixed scale_size
##' @importFrom ggplot2 element_blank xlab ylab geom_ribbon facet_wrap
##' @importFrom stats qlogis
##' @importFrom dplyr "%>%"
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices hcl.colors devAskNewPage
##' @method plot fG_mpm
##'
##' @examples
##' # plot mpm fit object
##' # 1-d time-series plots
##' plot(xm) 
##' 
##' ## generate a fG_ssm fit object (call is for speed only)
##' xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
##' control = ssm_control(se = FALSE, verbose = 0))
##' 
##' # 2-d track plots by adding ssm fit object
##' plot(xm, xs) 
##'
##' @export

plot.fG_mpm <-
  function(x,
           y = NULL,
           se = FALSE,
           pages = 0,
           ncol = 1,
           ask = TRUE,
           pal = "Zissou1",
           rev = FALSE,
           ...
  )
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  wpal <- hcl.colors(n = 5, "Zissou1")
  
  
  if(inherits(x, "fG_mpm") & (inherits(y, "fG_ssm") | is.null(y))) {
    d <- grab(x, as_sf = FALSE) %>%
      split(. ,.$id)
    
    if(is.null(y)) {
      p <- lapply(1:length(d), function(i) {
        ggplot(d[[i]]) +
          geom_ribbon(aes(
            date,
            ymin = plogis(qlogis(g) - 1.96 * g.se),
            ymax = plogis(qlogis(g) + 1.96 * g.se)
          ),
          fill = grey(0.5),
          alpha = 0.25) +
          geom_point(aes(date, g, colour = g)) + 
          scale_colour_gradientn(colours = hcl.colors(n=100, palette = pal, rev = rev),
                                 limits = c(0,1),
                                 name = expression(gamma[t])
          ) +
          ylab(expression(gamma[t])) +
          xlab(element_blank()) +
          ylim(0,1) +
          labs(title = paste("id:", d[[i]]$id[1])) +
          theme_minimal()
      })
      names(p) <- sapply(d, function(x) x$id[1])
      if (!pages) {
        if(ask) {
          devAskNewPage(ask = TRUE)
          print(p)
          devAskNewPage(ask = FALSE)
        } else {
          return(p)
        }
      } else if (pages) {
        wrap_plots(p, ncol = ncol, byrow = TRUE, guides = "collect")
      }  
    } else if(!is.null(y)) {
      if(nrow(grab(y, "predicted")) != nrow(grab(x, "fitted"))) {
        if(nrow(grab(y, "fitted")) != nrow(grab(x, "fitted"))) {
          stop("x and y have unequal numbers of estimated values")
        } else {
          xy <- join(y, x, what.ssm = "fitted", as_sf = FALSE) %>%
            split(., .$id)
        }
      } else {
        xy <- join(y, x, as_sf = FALSE) %>%
          split(., .$id)
      }
      
      p <- lapply(xy, function(x) {
        px <- ggplot(x) +
          geom_path(aes(lon, lat),
                    size = 0.25,
                    col = wpal[1],
                    alpha = 0.75)
        if(se) {
          px <- px + 
            geom_point(aes(lon, lat, colour = g, size = g.se ^ -2),
                     show.legend = c("colour" = TRUE, "size" = FALSE)) +
            scale_size(range = c(0.1, 2.5))
        } else if(!se) {
          px <- px +
            geom_point(aes(lon, lat, colour = g), size = 1)
        }
        px <- px + scale_colour_gradientn(
            breaks = c(0, 0.25, 0.5, 0.75, 1),
            colours = hcl.colors(n = 100, palette = pal, rev = rev),
            limits = c(0, 1),
            name = expression(gamma[t])
          ) +
          labs(title = paste("id:", unique(x$id))) +
          xlab(element_blank()) +
          ylab(element_blank()) +
          coord_fixed() +
          theme_minimal()
        px
      })
      names(p) <- y$id
      
      if (!pages) {
        if(ask) {
          devAskNewPage(ask = TRUE)
          print(p)
          devAskNewPage(ask = FALSE)
        } else {
          return(p)
        }
      } else if (pages) {
        wrap_plots(p, ncol = ncol, heights = rep(2, ceiling(length(p)/ncol)), 
                   guides = "collect")
      }
    }
    
  } else {
    stop("x must be a fG_mpm tibble and y must be a fG_ssm tibble or NULL")
  }
}