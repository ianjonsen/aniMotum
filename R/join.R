##' @title join an mpm-estimated behavioural index to ssm-predicted locations
##'
##' @description `join()` joins ssm-predicted locations and mpm-estimated behavioural index into a single tibble. If the ssm-predicted tibble is a projected sf object then the output of join will also be an sf object (default). This can be avoided by using `as_sf = FALSE`.
##'
##' @param ssm a `foieGras` ssm fitted model object
##' @param mpm a `foieGras` mpm fitted model object
##' @param what.ssm specifies whether ssm `predicted` or `fitted` values are to be extracted
##' @param as_sf logical; if FALSE then return a tibble with un-projected lonlat
##' coordinates, otherwise return an sf tibble
##' @param normalise logical; if output includes a move persistence estimate, 
##' should g (the move persistence index) be normalised to have minimum = 0 and 
##' maximum = 1 (default = FALSE).
##' @param group logical; should g be normalised among individuals as a group, 
##' a 'relative g', or separately to highlight regions of lowest and highest move
##' persistence along a track (default = FALSE).
##'
##' @return a single tbl with all individuals
##'
##' @importFrom tibble as_tibble
##' @examples
##' ## load example foieGras fit objects (to save time)
##' ## generate a ssm fit object
##' xs <- fit_ssm(ellie, spdf=FALSE, model = "rw", time.step=24, control = ssm_control(verbose = 0))
##' xm <- fit_mpm(xs, what = "p", model = "mpm")
##' 
##' ## join predicted values as an un-projected tibble
##' xsm <- join(xs, xm)
##' xsm
##' @export
##' @md

join <- function(ssm, 
                 mpm, 
                 what.ssm = "predicted", 
                 as_sf = FALSE,
                 normalise = FALSE,
                 group = FALSE) {
  
  if(!inherits(ssm, "ssm_df")) stop("ssm must be a foieGras ssm fit object with class `ssm_df`")
  if(!inherits(mpm, "mpm_df")) stop("mpm must be a foieGras mpm fit object with class `mpm_df`")
  
  x <- grab(ssm, what = what.ssm, as_sf = as_sf) 
  y <- grab(mpm, what = "fitted", normalise = normalise, group = group)[, c("logit_g","logit_g.se","g")] 
  
  if(nrow(x) != nrow(y)) stop("number of rows in ssm is NOT equal to number of rows in mpm")
  
  xy <- cbind(x, y) 
  
  if(!as_sf) {
    xy <- as_tibble(xy)
  } else {
    ## ensures geometry is last column (for cases using older sf)
    nc <- ncol(xy)
    ng <- which(names(xy) == "geometry")
    if(nc != ng) {
      xy <- xy[, c(1:(ng-1), (ng+1):nc, ng)]
    }
  }
  
  class(xy) <- append(class(xy), "ssmmpm", after = 0)
  return(xy)
  
}