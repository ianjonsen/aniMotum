##' @title join an mpm-estimated behavioural index to ssm-predicted locations
##'
##' @description `join()` joins ssm-predicted locations and mpm-estimated behavioural index into a single tibble. If the ssm-predicted tibble is a projected sf object then the output of join will also be an sf object (default). This can be avoided by using `as_sf = FALSE`.
##'
##' @param ssm a \code{foieGras} ssm fitted model object
##' @param mpm a \code{foieGras} mpm fitted model object
##' @param what.ssm specifies whether ssm `predicted` or `fitted` values are to be extracted
##' @param as_sf logical; if FALSE then return a tibble with un-projected lonlat
##' coordinates, otherwise return an sf tibble
##'
##' @return a single tbl with all individuals
##'
##' @importFrom tibble as_tibble
##' @examples
##' ## load example foieGras fit objects (to save time)
##' ## generate a fG_ssm fit object
##' xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
##' control = ssm_control(se = FALSE, verbose = 0))
##' data(xm)
##' 
##' ## join predicted values as an un-projected tibble
##' xsm <- join(xs, xm, as_sf = FALSE)
##' xsm
##' @export

join <- function(ssm, mpm, what.ssm = "predicted", as_sf = TRUE) {
  
  if(!inherits(ssm, "fG_ssm")) stop("ssm must be a foieGras ssm fit object with class `fG_ssm`")
  if(!inherits(mpm, "fG_mpm")) stop("mpm must be a foieGras mpm fit object with class `fG_mpm`")
  
  x <- grab(ssm, what = what.ssm, as_sf = as_sf) 
  y <- grab(mpm, what = "fitted")[, c("g","g.se")] 
  
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
  
#  class(xy) <- append(class(xy), "fG_ssmp", after = 0)
  return(xy)
  
}