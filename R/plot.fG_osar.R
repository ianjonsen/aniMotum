##' @title plot
##'
##' @description plot One-Step-Ahead (prediction) residuals from a \code{foieGras osar} object
##'
##' @param x a \code{foieGras osar} object with class `fG_osar`
##' @param type type of residual plot to generate; time-series (ts), qqnorm (qq; default) or acf (note: hist is deprecated)
##' @param pages plots of all individuals on a single page (pages = 1; default) or each individual on a separate page (pages = 0) 
##' @param ncol number of columns to use for faceting. Default is ncol = 2 but this may be increased for multi-individual fit objects
##' @param ask logical; if TRUE (default) user is asked for input before each plot is rendered. set to FALSE to return ggplot objects
##' @param pal \code{hcl.colors} colour palette to use (default = "Zissou1"; type \code{hcl.pals()} for options)
##' @param ... additional arguments to be ignored
##' 
##' @importFrom ggplot2 ggplot geom_qq geom_qq_line geom_segment geom_boxplot geom_hline
##' @importFrom ggplot2 aes facet_grid theme_minimal
##' @importFrom stats acf qnorm
##' @importFrom grDevices hcl.colors
##' @method plot fG_osar
##'
##' @examples
##' ## generate a fG_ssm fit object (call is for speed only)
##' xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
##' control = ssm_control(se = FALSE, verbose = 0))
##' 
##' dres <- osar(xs[2, ]) # only use one seal to save time
##' plot(dres, type = "qq")
##'
##' @export

plot.fG_osar <-
  function(x,
           type = c("ts", "qqnorm", "acf"),
           pages = 1,
           ncol = 1,
           ask = TRUE,
           pal = "Zissou1",
           ...)
  {
    if (length(list(...)) > 0) {
      warning("additional arguments ignored")
    }
    
    if (type[1] == "hist") {
      warning("type = 'hist' is deprecated, using type = 'qqnorm' instead",
              immediate. = TRUE)
      type <- "qqnorm"
    }
  type <- match.arg(type)
  
  wpal <- hcl.colors(n = 5, palette = pal)
  
  if(inherits(x, "fG_osar")) {
  
  switch(type,
         ts = {
           x.lst <- split(x, x$id)
           p <- lapply(x.lst, function(x) {
             ggplot(x) +
             geom_point(aes(x = date, y = residual), shape = 19, colour = wpal[1]) +
             geom_hline(aes(yintercept = 0), lty = 2, colour = wpal[4]) +
             facet_grid(id ~ coord) +
             theme_minimal()
           })
         }, 
         qqnorm = {
           x <- x[!is.na(x$residual), ]
           x.lst <- split(x, x$id)
           p <- lapply(x.lst, function(x){
             ggplot(x, aes(sample = residual)) +
               geom_qq(colour = wpal[1]) +
               geom_qq_line(colour = wpal[4]) +
               facet_grid(id ~ coord) +
               theme_minimal()
           })
         },
         acf = {
           x <- x[!is.na(x$residual), ]
           x.lst <- split(x, x$id)
           p <- lapply(x.lst, function(x) {
            x.acf <- acf(x[x$coord == "x", "residual"], plot = FALSE)
            y.acf <- acf(x[x$coord == "y", "residual"], plot = FALSE)
            x1 <- rbind(with(x.acf, data.frame(lag, acf, coord = rep("x", length(x.acf$lag)))), 
                       with(y.acf, data.frame(lag, acf, coord = rep("y", length(y.acf$lag))))
                       ) %>%
              mutate(id = unique(x$id))
            
            cil.x <- qnorm((1 - 0.95)/2) / sqrt(nrow(x[x$coord == "x", ]))
            cil.y <- qnorm((1 - 0.95)/2) / sqrt(nrow(x[x$coord == "y", ]))
            cil <- data.frame(ci = c(cil.x,cil.y), coord = c("x","y"))
            
            ggplot(x1, aes(x = lag, y = acf)) +
              geom_hline(aes(yintercept = 0), colour = wpal[4]) +
              geom_segment(aes(xend = lag, yend = 0), colour = wpal[1]) +
              geom_hline(data = cil, aes(yintercept = ci), linetype = 2, color = wpal[2]) +
              geom_hline(data = cil, aes(yintercept = -ci), linetype = 2, color = wpal[2]) +
              facet_grid(id ~ coord) +
              theme_minimal()
           })
         })

    if (pages == 1)
      wrap_plots(p, ncol = ncol, heights = rep(1, ceiling(length(p) / ncol)))
    else {
      if (ask) {
        devAskNewPage(ask = TRUE)
        print(p)
        devAskNewPage(ask = FALSE)
      } else {
        return(p)
      }
    }
    
  } else {
    stop("an fG_osar class object is required")
  }
}
