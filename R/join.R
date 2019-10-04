##' @title join an mpm-estimated behavioural index to ssm-predicted locations
##'
##' @description `join()` joins ssm-predicted locations and mpm-estimated behavioural index into a single tibble. If the ssm-predicted tibble is a projected sf object then the output of join will also be an sf object (default). This can be avoided by using `as_sf = FALSE`.
##'
##' @param ssm a \code{foieGras} ssm fitted model object
##' @param mpm a \code{foieGras} mpm fitted model object
##' @param as_sf logical; if FALSE then return a tibble with unprojected lonlat
##' coordinates, otherwise return an sf tibble
##'
##' @return a single tbl with all individuals
##'
##' @importFrom dplyr bind_cols select "%>%"
##' @importFrom tibble as_tibble
##' @examples
##' ## load example foieGras fit objects (to save time)
##' data(fssm)
##' data(fmpm)
##' ## join predicted values as an unprojected tibble
##' fsmp <- join(fssm, fmpm, as_sf = FALSE)
##' fsmp
##' @export

join <- function(ssm, mpm, as_sf = TRUE) {
  
  if(!inherits(ssm, "fG_ssm")) stop("ssm must be a foieGras ssm fit object with class `fG_ssm`")
  if(!inherits(mpm, "fG_mpm")) stop("mpm must be a foieGras mpm fit object with class `fG_mpm`")
  
  x <- grab(ssm, what = "predicted", as_sf = as_sf) 
  y <- grab(mpm, what = "fitted") %>% select(id, g, g.se)
  
  if(nrow(x) != nrow(y)) stop("number of rows in ssm is NOT equal to number of rows in mpm")
  
  if(as_sf) {
    xy <- bind_cols(x, y) %>% 
      select(-id1)
  } else {
    xy <- bind_cols(x, y) %>%
      select(-id1) %>%
      as_tibble()
  }
  
  class(xy) <- append(class(xy), "fG_ssmp", after = 0)
  return(xy)
  
}