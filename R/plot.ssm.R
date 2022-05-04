##' generate error ellipses from x,y coordinates, semi-major, semi-minor axes 
##' and ellipse orientation
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
##' @description visualize fits from an ssm object
##'
##' @param x a `foieGras` ssm fit object with class `ssm_df`
##' @param what specify which location estimates to display on time-series plots: 
##' fitted, predicted, or rerouted
##' @param type of plot to generate: 1-d time series for lon and lat separately 
##' (type = 1, default); 2-d track plot (type = 2); 1-d time series of move
##' persistence estimates (type = 3; if fitted model was `mp`); 2-d track plot 
##' with locations coloured by move persistence (type = 4; if fitted model was `mp`)
##' @param outlier include outlier locations dropped by prefilter 
##' (outlier = TRUE, default)
##' @param alpha opacity of standard errors. Lower opacity can ease visualization
##' when multiple ellipses overlap one another 
##' @param pages each individual is plotted on a separate page by default 
##' (pages = 0), multiple individuals can be combined on a single page; pages = 1
##' @param ncol number of columns to arrange plots when combining individuals on 
##' a single page (ignored if pages = 0)
##' @param ask logical; if TRUE (default) user is asked for input before each plot 
##' is rendered. set to FALSE to return ggplot objects
##' @param pal [grDevices::hcl.colors] palette to use (default: "Zissou1"; 
##' see [grDevices::hcl.pals()] for options)
##' @param normalise logical; if plotting move persistence estimates from an `mp`
##' model fit, should estimates be normalised to 0,1 (default = TRUE).
##' @param group logical; should `g` be normalised among individuals as a group, 
##' a 'relative g', or to individuals separately to highlight regions of lowest 
##' and highest move persistence along single tracks (default = FALSE).
##' @param ... additional arguments to be ignored
##' 
##' @return a ggplot object with either: (type = 1) 1-d time series of fits to 
##' data, separated into x and y components (units = km) with prediction 
##' uncertainty ribbons (2 x SE); or (type = 2) 2-d fits to data (units = km)
##' 
##' @importFrom ggplot2 ggplot geom_point geom_path aes_string ggtitle geom_rug 
##' @importFrom ggplot2 theme_minimal vars labs coord_fixed label_value geom_ribbon 
##' @importFrom ggplot2 element_text element_blank xlab ylab labeller label_both 
##' @importFrom ggplot2 facet_wrap scale_colour_viridis_c
##' @importFrom tidyr gather
##' @importFrom sf st_multipolygon st_polygon st_as_sfc st_as_sf
##' @importFrom patchwork wrap_plots
##' @importFrom grDevices hcl.colors devAskNewPage
##' @method plot ssm_df
##'
##' @examples
##' ## generate a ssm fit object (call is for speed only)
##' xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
##' control = ssm_control(verbose = 0))
##' 
##' # plot fitted locations as 1-D timeseries
##' plot(xs, what = "f")
##' 
##' # all on 1 page
##' plot(xs, what = "f", pages = 1)
##' 
##' # plot predicted locations as 2-D tracks
##' plot(xs, what = "p", type = 2, pages = 1, ncol = 2)
##'
##' @export
##' @md

plot.ssm_df <-
  function(x,
           what = c("fitted", "predicted", "rerouted"),
           type = 1,
           outlier = TRUE,
           alpha = 0.05,
           pages = 0,
           ncol = 1,
           ask = TRUE,
           pal = "default",
           normalise = TRUE,
           group = FALSE,
           ...)
  {
    if (length(list(...)) > 0) {
      warning("additional arguments ignored")
    }
    
    what <- match.arg(what)
    
    stopifnot("palette is not an hcl.palette, type 'hcl.pals()' to see the list of palettes" = 
                pal %in% c(hcl.pals(), "default"))
    stopifnot("plot 'type' can only be one of 1, 2, 3, or 4" = type %in% 1:4)
    
    if(pal != "default") {
      cpal <- hcl.colors(n = 5, palette = pal)
    } else {
      cpal <- c("#1e90ff", # dodgerblue
                "#b22222", # firebrick
                "#ffa500", # orange
                "#000000") # black
      
      pal <- "Cividis"
    }
    
    if (inherits(x, "ssm_df")) {
      switch(what,
             fitted = {
               ssm <- grab(x, "fitted", as_sf = FALSE, normalise = normalise, group = group)
             },
             predicted = {
               if (any(sapply(x$ssm, function(.)
                 is.na(.$ts)))) {
                 ssm <- grab(x, "fitted", as_sf = FALSE, normalise = normalise, group = group)
                 warning(
                   "there are no predicted locations because you used time.step = NA when calling `fit_ssm`, plotting fitted locations instead",
                   call. = FALSE
                 )
               } else {
                 ssm <- grab(x, "predicted", as_sf = FALSE, normalise = normalise, group = group)
               }
             },
             rerouted = {
               if (any(sapply(x$ssm, function(.) "rerouted" %in% names(.)))) {
                 ssm <- grab(x, "rerouted", as_sf = FALSE, normalise = normalise, group = group)
               } else {
                 stop(
                   "there are no rerouted locations present",
                   call. = FALSE
                 )
               }
             })
             
      
      if (outlier) {
        d <- grab(x, "data", as_sf = FALSE)
        d$lc <- with(d, factor(
          lc,
          levels = c("3", "2", "1", "0", "A", "B", "Z"),
          ordered = TRUE
        ))
      } else {
        d <- grab(x, "data", as_sf = FALSE)
        d$lc <- with(d, factor(
            lc,
            levels = c("3", "2", "1", "0", "A", "B", "Z"),
            ordered = TRUE
          ))
        d <- subset(d, keep)
      }
      
      if (type == 1) {
        foo <- ssm[, c("id","x","y")]
        foo <- gather(foo, key = "coord", value = "value", x, y)
        foo.se <- ssm[, c("x.se", "y.se")] 
        foo.se <- gather(foo.se, key = "coord.se", value = "se", x.se, y.se)
        bar <- data.frame(date = rep(ssm$date, 2))

        foo.d <- d[, c("id","x","y")]
        foo.d <- gather(foo.d, key = "coord", value = "value", x, y)
        bar.d <- rbind(d[, c("date", "lc", "keep")], d[, c("date", "lc", "keep")])
        
        pd <- cbind(foo, foo.se, bar)[, c("id", "date", "coord", "value", "se")]
        dd <- cbind(foo.d, bar.d)[, c("id", "date", "lc", "coord", "value", "keep")]
        
        ## coerce to lists
        pd.lst <- split(pd, pd$id)
        dd.lst <- split(dd, dd$id)
        
        p <- lapply(1:nrow(x), function(i) {
          px <- ggplot(subset(dd.lst[[i]], keep),
                       aes(date, value))
          ## add ribbon
          px <- px + geom_ribbon(
              data = pd.lst[[i]],
              aes(date, ymin = value - 2 * se,
                  ymax = value + 2 * se),
              fill = cpal[2],
              alpha = alpha + 0.15
            )
          
          if (outlier) {
            px <- px +
              geom_point(
                data = subset(dd.lst[[i]], !keep),
                aes(date, value),
                colour = cpal[3],
                shape = 4
              ) +
              geom_point(
                data = subset(dd.lst[[i]], keep),
                aes(date, value),
                colour = cpal[1],
                shape = 19,
                size = 2
              ) +
              geom_rug(
                data = subset(dd.lst[[i]], !keep),
                aes(date),
                colour = cpal[3],
                sides = "b"
              ) +
              geom_rug(
                data = subset(dd.lst[[i]], keep),
                aes(date),
                colour = cpal[1],
                sides = "b"
              )
          } else {
            px <- px +
              geom_point(
                data = subset(dd.lst[[i]], keep),
                aes(date, value),
                colour = cpal[1],
                shape = 19,
                size = 2
              ) +
              geom_rug(
                data = subset(dd.lst[[i]], keep),
                aes(date),
                colour = cpal[1],
                sides = "b"
              )
          }
          px <- px +
            geom_point(
              data = pd.lst[[i]],
              aes(date, value),
              col = cpal[2],
              shape = 20,
              size = 0.75
            ) +
            facet_wrap(
              facets = vars(coord),
              scales = "free",
              labeller = labeller(coord = label_value),
              ncol = 2
            ) +
            labs(title = paste("id:", x$id[i])) +
            xlab(element_blank()) +
            ylab(element_blank()) +
            theme_minimal()
          px
        })
        names(p) <- x$id
        
        if (!pages) {
          if(ask & nrow(x) > 1) {
            devAskNewPage(ask = TRUE)
            print(p)
            devAskNewPage(ask = FALSE)
          } else {
            return(p)
          }
        } else if (pages) {
          wrap_plots(p, ncol = ncol, byrow = TRUE)
        }
        
      } else if (type == 2) {
        ssm.lst <- split(ssm, ssm$id)
        conf_poly <- lapply(ssm.lst, function(x) {
          conf <- lapply(1:nrow(x), function(i)
            with(x, elps(x[i], y[i], x.se[i], y.se[i], 90)))
          tmp <- lapply(conf, function(x)
            st_polygon(list(x)))
          st_multipolygon(tmp)
        })

        conf_sf <- st_as_sf(st_as_sfc(conf_poly)) 
        conf_sf$id <- unique(ssm$id)
        
        d.lst <- split(d, d$id)
        
        p <- lapply(1:nrow(x), function(i) {
          m <- ggplot() +
            geom_sf(
              data = subset(conf_sf, id == unique(id)[i]),
              col = NA,
              fill = cpal[2],
              alpha = alpha
            )
          
          if (outlier) {
            m <- m +
              geom_point(
                data = subset(d, !keep & id == unique(id)[i]),
                aes(x, y),
                size = 1,
                colour = cpal[3],
                shape = 4
              ) +
              geom_point(
                data = subset(d, keep & id == unique(id)[i]),
                aes(x, y),
                size = 2,
                colour = cpal[1],
                shape = 19,
                alpha = 0.6
              )
          } else {
            m <- m +
              geom_point(
                data = subset(d, keep & id == unique(id)[i]),
                aes(x, y),
                size = 2,
                colour = cpal[1],
                shape = 19,
                alpha = 0.6
              )
          }
          m <- m +
            geom_path(
              data = subset(ssm, id == unique(id)[i]),
              aes(x, y),
              col = cpal[4],
              alpha = 0.5,
              lwd = 0.2
            ) +
            geom_point(
              data = subset(ssm, id == unique(id)[i]),
              aes(x, y),
              col = cpal[2],
              shape = 20,
              size = 0.75
            ) +
            labs(title = paste("id:", x[i, "id"]))
          
          m <- m +
            xlab(element_blank()) +
            ylab(element_blank()) +
            theme_minimal()
          m
        })
        names(p) <- x$id
        if (!pages) {
          if(ask & nrow(x) > 1) {
            devAskNewPage(ask = TRUE)
            print(p)
            devAskNewPage(ask = FALSE)
          } else {
            return(p)
          }
        } else if (pages) {
          wrap_plots(p, ncol = ncol, heights = rep(1, ceiling(length(p) / ncol)))
        }
      } else if (type == 3) {
        stopifnot("This plot type not applicable for `rw` and `crw` model fits" = 
                    "g" %in% names(ssm))
        ssm.lst <- split(ssm, ssm$id)
        p <- lapply(1:nrow(x), function(i) {
          if(normalise) {
            ## rescale CI's naively
            ymin <- plogis(ifelse(
              qlogis(ssm.lst[[i]]$g) == Inf,
              6,
              ifelse(qlogis(ssm.lst[[i]]$g) == -Inf,-6, qlogis(ssm.lst[[i]]$g))
            )
            - 1.96 * ssm.lst[[i]]$logit_g.se)
            ymax <- plogis(ifelse(
              qlogis(ssm.lst[[i]]$g) == Inf,
              6,
              ifelse(qlogis(ssm.lst[[i]]$g) == -Inf,-6, qlogis(ssm.lst[[i]]$g))
            )
            + 1.96 * ssm.lst[[i]]$logit_g.se)
          } else {
            ymin <- plogis(ssm.lst[[i]]$logit_g - 1.96 * ssm.lst[[i]]$logit_g.se)
            ymax <- plogis(ssm.lst[[i]]$logit_g + 1.96 * ssm.lst[[i]]$logit_g.se)
          }
          m <- ggplot() +
            geom_ribbon(
              data = ssm.lst[[i]],
              aes(date,
              ymin = ymin,
              ymax = ymax),
              fill = grey(0.5),
              alpha = 0.25) +
            geom_line(
              data = ssm.lst[[i]],
              aes(date, g, col = g),
              size = 0.3
            ) +
            geom_point(
              data = ssm.lst[[i]],
              aes(date, g, col = g),
              shape = 20,
              size = 3
            ) +
            scale_colour_gradientn(
              colours = hcl.colors(n = 100, pal = pal),
              limits = c(0,1),
              name = expression(gamma[t])
            ) +
            labs(title = paste("id:", x[i, "id"]))
          
          m <- m +
            xlab(element_blank()) +
            ylab(element_blank()) +
            theme_minimal()
          m
        })
        names(p) <- x$id
        if (!pages) {
          if(ask & nrow(x) > 1) {
            devAskNewPage(ask = TRUE)
            print(p)
            devAskNewPage(ask = FALSE)
          } else {
            return(p)
          }
        } else if (pages) {
          wrap_plots(p, ncol = ncol, byrow = TRUE, guides = "collect") 
            
        }
      } else if (type == 4) {
        stopifnot("This plot type not applicable for `rw` and `crw` model fits" = 
                    "g" %in% names(ssm))
        ssm.lst <- split(ssm, ssm$id)
        p <- lapply(1:nrow(x), function(i) {
          g.33 <- subset(ssm.lst[[i]], g <= quantile(ssm.lst[[i]]$g, 0.33))
          m <- ggplot() +
            geom_path(
              data = ssm.lst[[i]],
              aes(x, y), 
              col = grey(0.7),
              lwd = 0.2
            ) +
            geom_point(
              data = ssm.lst[[i]],
              aes(x, y, col = g),
              shape = 20,
              size = 2
            ) +
            geom_point(data = g.33,
                       aes(x, y, col = g),
                       shape = 20,
                       size = 2
            ) +
            scale_colour_gradientn(
              colours = hcl.colors(n=100, palette = pal),
              limits = c(0,1),
              name = expression(gamma[t])
              ) +
            labs(title = paste("id:", x[i, "id"])) +
            coord_fixed()
          
          m <- m +
            xlab(element_blank()) +
            ylab(element_blank()) +
            theme_minimal()
          m
        })
        names(p) <- x$id
        if (!pages) {
          if(ask & nrow(x) > 1) {
            devAskNewPage(ask = TRUE)
            print(p)
            devAskNewPage(ask = FALSE)
          } else {
            return(p)
          }
        } else if (pages) {
          wrap_plots(p, 
                     ncol = ncol, 
                     heights = rep(1, ceiling(length(p) / ncol)),
                     guides = "collect")
        }
      }
    } else {
      stop("x must be an `ssm_df` tibble")
    }
}
