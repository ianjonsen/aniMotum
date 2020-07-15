##' generate error ellipses from x,y coordinates, semi-major, semi-minor axes and ellipse orientation
##' 
##' @param x x-coordinate, usually in projected units (km)
##' @param y y-coordinate, usually in projected units (km)
##' @param a ellipse semi-major axis (km)
##' @param b ellipse semi-minor axis (km)
##' @param theta ellipse orientation from north (degrees)
##'
##' @keywords internal

elps <- function(x, y, a, b, theta = 90, conf = TRUE) {
  m <- ifelse(!conf, 1, 1.96)
  ln <- seq(0, 2*pi, l = 50)
  theta <- (-1 * theta + 90) / 180 * pi   ## req'd rotation to get 0 deg pointing N
  x1 <- m * a * cos(theta) * cos(ln) - m * b * sin(theta) * sin(ln)
  y1 <- m * a * sin(theta) * cos(ln) + m * b * cos(theta) * sin(ln)
  
  cbind(c(x+x1, x+x1[1]), c(y+y1, y+y1[1]))
}


##' @title plot
##'
##' @description visualize fits from an fG_ssm object
##'
##' @param x a \code{foieGras} ssm fit object with class `fG_ssm`
##' @param what specify which location estimates to display on time-series plots: fitted or predicted
##' @param type of plot to generate: 1-d time series for lon and lat separately (type = 1, default) or 2-d track plot (type = 2)
##' @param outlier include outlier locations dropped by prefilter (outlier = TRUE, default)
##' @param pages plots of all individuals on a single page (pages = 1; default) or each individual on a separate page (pages = 0) 
##' @param ncol number of columns to use for faceting. Default is ncol = 2 but this may be increased for multi-individual fit objects
##' @param ... additional arguments to be ignored
##' 
##' @return a ggplot object with either: (type = 1) 1-d time series of fits to data, 
##' separated into x and y components (units = km) with prediction uncertainty ribbons (2 x SE); 
##' or (type = 2) 2-d fits to data (units = km)
##' 
##' @importFrom ggplot2 ggplot geom_point geom_path aes_string ggtitle geom_rug theme_minimal vars labs
##' @importFrom ggplot2 element_text element_blank xlab ylab labeller label_both label_value geom_ribbon facet_wrap
##' @importFrom tidyr gather
##' @importFrom dplyr "%>%" select bind_cols rename filter bind_rows mutate
##' @importFrom tibble enframe
##' @importFrom sf st_multipolygon st_polygon st_as_sfc st_as_sf
##' @importFrom patchwork wrap_plots
##' @importFrom wesanderson wes_palette
##' @method plot fG_ssm
##'
##' @examples
##' ## load example foieGras fit object (to save time)
##' data(xs)
##' plot(xs, what = "f", type = 1)
##' plot(xs, what = "p", type = 2)
##'
##' @export

plot.fG_ssm <- function(x, what = c("fitted","predicted"), type = 1, outlier = TRUE, pages = 1, ncol = 2, ...)
{
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  what <- match.arg(what)
  
  wpal <- wes_palette("Zissou1", n = 5, "discrete")
  
  if(inherits(x, "fG_ssm")) {
    switch(what,
           fitted = {
             ssm <- grab(x, "fitted", as_sf = FALSE)
           },
           predicted = {
             if(any(sapply(x$ssm, function(.) is.na(.$ts)))) {
               ssm <- grab(x, "fitted", as_sf = FALSE)
               warning("there are no predicted locations because you used time.step = NA when calling `fit_ssm`, 
                       plotting fitted locations instead", call. = FALSE)
             } else {
             ssm <- grab(x, "predicted", as_sf = FALSE)
             }
           })
    
    if(outlier) {
      d <- grab(x, "data", as_sf = FALSE) %>%
        mutate(lc = factor(lc, levels=c("3","2","1","0","A","B","Z"), ordered=TRUE))
    } else {
      d <- grab(x, "data", as_sf = FALSE) %>%
        mutate(lc = factor(lc, levels=c("3","2","1","0","A","B","Z"), ordered=TRUE)) %>%
        filter(keep)
    }
    
    if(type == 1) {
      
      foo <- ssm %>% select(id, x, y) %>% gather(., key = "coord", value = "value", x, y)
      foo.se <- ssm %>% select(x.se, y.se) %>% gather(., key = "coord.se", value = "se", x.se, y.se)
      bar <- rep(ssm$date, 2) %>% enframe(name = NULL) %>% rename(date = "value")
      
      foo.d <- d %>% select(id, x, y) %>% gather(., key = "coord", value = "value", x, y)
      bar.d <- d %>% select(date, lc, keep) %>% bind_rows(., .)
      
      pd <- bind_cols(foo, foo.se, bar) %>% select(id, date, coord, value, se)
      dd <- bind_cols(foo.d, bar.d) %>% select(id, date, lc, coord, value, keep)
    
      if(pages == 1) {
        ## plot SE ribbon first
        p <- ggplot() + 
          geom_ribbon(data = pd, aes(date, ymin = value - 2 * se, ymax = value + 2 * se), fill=wpal[5], alpha = 0.4)
      
        if(outlier) {
          p <- p + 
            geom_point(data = dd %>% filter(!keep), aes(date, value), 
                       colour = wpal[4], shape = 4) +
            geom_point(data = dd %>% filter(keep), aes(date, value), 
                       colour = wpal[1], shape = 19, size = 2) +
            geom_rug(data = dd %>% filter(!keep), aes(date), colour = wpal[4], sides = "b") + 
            geom_rug(data = dd %>% filter(keep), aes(date), colour = wpal[1], sides = "b")
        } else {
          p <- p + 
            geom_point(data = dd %>% filter(keep), aes(date, value), 
                       colour = wpal[1], shape = 19, size = 2) +
            geom_rug(data = dd %>% filter(keep), aes(date), colour = wpal[1], sides = "b")
        }  
          p <- p + 
           geom_point(data = pd, aes(date, value), col=wpal[5], shape = 20, size = 0.75) + 
            facet_wrap(facets = vars(id, coord), scales = "free",
                     labeller = labeller(id = label_both, coord = label_value),
                     ncol = ncol)
          
      } else if(pages == 0){
        ## coerce to lists
        pd.lst <- split(pd, pd$id)
        dd.lst <- split(dd, dd$id)
        
       p <- lapply(1:nrow(x), function(i) {
          px <- ggplot() + 
            geom_ribbon(data = pd.lst[[i]], aes(date, ymin = value - 2 * se, ymax = value + 2 * se), fill=wpal[5], alpha = 0.4)
          
          if(outlier) {
            px <- px + 
              geom_point(data = dd.lst[[i]] %>% filter(!keep), aes(date, value), 
                         colour = wpal[4], shape = 4) +
              geom_point(data = dd.lst[[i]] %>% filter(keep), aes(date, value), 
                         colour = wpal[1], shape = 19, size = 2) +
              geom_rug(data = dd.lst[[i]] %>% filter(!keep), aes(date), colour = wpal[4], sides = "b") + 
              geom_rug(data = dd.lst[[i]] %>% filter(keep), aes(date), colour = wpal[1], sides = "b")
          } else {
            px <- px + 
              geom_point(data = dd.lst[[i]] %>% filter(keep), aes(date, value), 
                         colour = wpal[1], shape = 19, size = 2) +
              geom_rug(data = dd.lst[[i]] %>% filter(keep), aes(date), colour = wpal[1], sides = "b")
          }  
          px <- px + 
            geom_point(data = pd.lst[[i]], aes(date, value), col=wpal[5], shape = 20, size = 0.75) + 
            facet_wrap(facets = vars(coord), scales = "free",
                       labeller = labeller(coord = label_value),
                       ncol = 2) +
            labs(title = paste("id:", x$id[i]))
          px
        })
        
      } 
      
      return(p)
      
    } else if (type == 2) {
      ssm.lst <- split(ssm, ssm$id)
      conf_poly <- lapply(ssm.lst, function(x) {
        conf <- lapply(1:nrow(x), function(i)
          with(x, elps(x[i], y[i], x.se[i], y.se[i], 90))
        )
        lapply(conf, function(x) st_polygon(list(x))) %>%
          st_multipolygon()
      })
      conf_sf <- st_as_sfc(conf_poly) %>%
        st_as_sf() %>%
        mutate(id = unique(ssm$id))
      
      if(nrow(x) == 1) {
      ## for a single track plot
      p <- ggplot() + 
        geom_sf(data = conf_sf, col = NA, fill = wpal[5], alpha = 0.25)
      
      if(outlier) {
      p <- p + 
        geom_point(data = d %>% filter(!keep), aes(x, y),
                   size = 1, colour = wpal[4], shape = 4) +
        geom_point(data = d %>% filter(keep), aes(x, y),
                              size = 2, colour = wpal[1], shape = 19, alpha = 0.6)
      } else {
        p <- p + 
          geom_point(data = d %>% filter(keep), aes(x, y),
                     size = 2, colour = wpal[1], shape = 19, alpha = 0.6)
      }
        p <- p + 
          geom_path(data = ssm, aes(x, y), col = wpal[5], lwd = 0.2) +
          geom_point(data = ssm, aes(x, y), col = wpal[5], shape = 20, size = 0.75) + 
          labs(title = paste("id:", x$id))
      
    p <- p + 
      xlab(element_blank()) + 
      ylab(element_blank()) +
      theme_minimal()
    
    return(p)
    
    } else {
      ## for multiple tracks on 1 page, use patchwork::wrap_plots instead of facet_wrap so we can have the equivalent of scales = "free"
    d.lst <- split(d, d$id)
    ssm.lst <- split(ssm, ssm$id)

    p <- lapply(1:nrow(x), function(i) {
      m <- ggplot() + 
        geom_sf(data = conf_sf %>% filter(id == unique(id)[i]), col = NA, fill = wpal[5], alpha = 0.25)
      
      if(outlier) {
        m <- m + 
          geom_point(data = d %>% filter(!keep & id == unique(id)[i]), aes(x, y),
                     size = 1, colour = wpal[4], shape = 4) +
          geom_point(data = d %>% filter(keep & id == unique(id)[i]), aes(x, y),
                     size = 2, colour = wpal[1], shape = 19, alpha = 0.6)
      } else {
        m <- m + 
          geom_point(data = d %>% filter(keep & id == unique(id)[i]), aes(x, y),
                     size = 2, colour = wpal[1], shape = 19, alpha = 0.6)
      }
      m <- m + 
        geom_path(data = ssm %>% filter(id == unique(id)[i]), aes(x, y), col = wpal[5], lwd = 0.2) +
        geom_point(data = ssm %>% filter(id == unique(id)[i]), aes(x, y), col = wpal[5], shape = 20, size = 0.75) + 
        labs(title = paste("id:", x[i, "id"]))
      
    m <- m + 
      xlab(element_blank()) + 
      ylab(element_blank()) +
      theme_minimal()
    m  
    })
    if(pages == 1) wrap_plots(p, ncol = ncol, heights = rep(1, ceiling(length(p)/ncol)))
    else return(p)
    }
  } else {
    stop("x must be a fG_ssm tibble")
  }
  }
}
