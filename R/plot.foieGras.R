##' Visualise foieGras SSM fits to track data
##'
##' @title plot
##' @param m a foieGras fitted object
##' @param est specify which location estimates to display on time-series plots: fitted or predicted
##' @param proj specify whether to plot mercator xy or ll (longlat)
##' @param se include 2 * SE on time-series plots (SE's currently not displayed when proj = "ll")
##' @param resid display time-series plots as trends (FALSE - default) or as residuals (TRUE)
##' @param outlier include outliers identified during prefilter-ing
##' @importFrom ggplot2 ggplot geom_point geom_line geom_path aes ggtitle theme_bw theme element_blank
##' @importFrom gridExtra grid.arrange
##' @method plot foieGras
##' @export

plot.foieGras <- function(m, est = c("fitted","predicted"), proj = c("ll","xy"), se = TRUE, resid = FALSE, outlier = FALSE)
{
  proj <- match.arg(proj)
  est <- match.arg(est)
  d <- if(!outlier) {
    subset(m$data, keep)
  } else {
    d <- m$data
  }
  nd <- subset(m$data, !keep)
  f <- m$fitted
  p <- m$predicted


  p1 <-
    switch(proj,
        xy = {
          ggplot() +
            geom_point(data = d, aes(x, y), shape = 19, col = grey(0.85)) +
            geom_point(data = f, aes(x, y), size = 0.4, shape = 20, col = "red") +
            geom_path(data = f, aes(x, y), lwd = 0.25, col = "red") +
            theme_bw() +
            ggtitle(paste0(f$id[1], "\nfitted values"))
             },
        ll = {
          ggplot() +
            geom_point(data = d, aes(lon, lat), shape = 19, col = grey(0.85)) +
            geom_point(data = f, aes(lon, lat), size = 0.4, shape = 20, col = "red") +
            geom_path(data = f, aes(lon, lat), lwd = 0.25, col = "red") +
            theme_bw() +
            ggtitle(paste0(f$id[1], "\nfitted values"))
           })


  p2 <- switch(proj,
               xy = {
                 ggplot() +
                   geom_point(data = d, aes(x, y), shape = 19, col = grey(0.85)) +
                   geom_point(data = p, aes(x, y), size = 0.4, shape = 20, col = "dodgerblue") +
                   geom_path(data = p, aes(x, y), lwd = 0.25, col = "dodgerblue") +
                   theme_bw() +
                   ggtitle("\npredicted values")
               },
               ll = {
                 ggplot() +
                   geom_point(data = d, aes(lon, lat), shape = 19, col = grey(0.85)) +
                   geom_point(data = p, aes(lon, lat), size = 0.4, shape = 20, col = "dodgerblue") +
                   geom_path(data = p, aes(lon, lat), lwd = 0.25, col = "dodgerblue") +
                   theme_bw() +
                   ggtitle("\npredicted values")
               })


  p3 <-
    switch(est,
      fitted = {
      switch(proj,
           xy = {
            p <- ggplot() +
              geom_point(data = d, aes(date, x), shape = 19, col = grey(0.85)) +
              geom_point(data = f, aes(date, x), size = 0.2, shape = 20, col = "red") +
               theme_bw() +
               theme(axis.title.x=element_blank())
              if(se) {
                p <- p +
              geom_line(data = f, aes(date, x + 2 * x.se), lwd = 0.25, col = "red") +
              geom_line(data = f, aes(date, x - 2 * x.se), lwd = 0.25, col = "red")
              }
              p
           },
           ll = {
            ggplot() +
              geom_point(data = d, aes(date, lon), shape = 19, col = grey(0.85)) +
              geom_point(data = f, aes(date, lon), size = 0.2, shape = 20, col = "red") +
              theme_bw() +
               theme(axis.title.x=element_blank())
           })
        },
      predicted = {
        switch(proj,
               xy = {
                 p <- ggplot() +
                    geom_point(data = d, aes(date, x), shape = 19, col = grey(0.85)) +
                    geom_point(data = p, aes(date, x), size = 0.2, shape = 20, col = "dodgerblue") +
                   theme_bw() +
                   theme(axis.title.x=element_blank())
                    if(se) {
                    p <-  p +
                      geom_line(data = p, aes(date, x + 2 * x.se), lwd = 0.25, col = "dodgerblue") +
                    geom_line(data = p, aes(date, x - 2 * x.se), lwd = 0.25, col = "dodgerblue")
                    }
                 p
               },
               ll = {
                 ggplot() +
                   geom_point(data = d, aes(date, lon), shape = 19, col = grey(0.85)) +
                   geom_point(data = p, aes(date, lon), size = 0.2, shape = 20, col = "dodgerblue") +
                   theme_bw() +
                   theme(axis.title.x=element_blank())
               })
        })

  p4 <- switch(est,
               fitted = {
                 switch(proj,
                        xy = {
                          p <- ggplot() +
                            geom_point(data = d, aes(date, y), shape = 19, col = grey(0.85)) +
                            geom_point(data = f, aes(date, y), size = 0.2, shape = 20, col = "red") +
                            theme_bw() +
                            theme(axis.title.x=element_blank())
                            if(se) {
                            p <- p +
                              geom_line(data = f, aes(date, y + 2 * y.se), lwd = 0.25, col = "red") +
                            geom_line(data = f, aes(date, y - 2 * y.se), lwd = 0.25, col = "red")
                            }
                          p
                        },
                        ll = {
                          ggplot() +
                            geom_point(data = d, aes(date, lat), shape = 19, col = grey(0.85)) +
                            geom_point(data = f, aes(date, lat), size = 0.2, shape = 20, col = "red") +
                            theme_bw() +
                            theme(axis.title.x=element_blank())
                        })
               },
               predicted = {
                 switch(proj,
                        xy = {
                          p <- ggplot() +
                            geom_point(data = d, aes(date, y), shape = 19, col = grey(0.85)) +
                            geom_point(data = p, aes(date, y), size = 0.2, shape = 20, col = "dodgerblue") +
                            theme_bw() +
                            theme(axis.title.x=element_blank())
                            if(se) {
                            p <- p +
                            geom_line(data = p, aes(date, y + 2 * y.se), lwd = 0.25, col = "dodgerblue") +
                            geom_line(data = p, aes(date, y - 2 * y.se), lwd = 0.25, col = "dodgerblue")
                            }
                          p
                        },
                        ll = {
                          ggplot() +
                            geom_point(data = d, aes(date, lat), shape = 19, col = grey(0.85)) +
                            geom_point(data = p, aes(date, lat), size = 0.2, shape = 20, col = "dodgerblue") +
                            theme_bw() +
                            theme(axis.title.x=element_blank())
                        })
               })

  grid.arrange(p1, p2, p3, p4, layout_matrix = matrix(
    c(1, 2, 1, 2, 3, 3, 4, 4),
    nrow = 4,
    ncol = 2,
    byrow = T
    ))
}
