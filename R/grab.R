##' @title grab tibble's by name from a foieGras model object
##'
##' @description `grab()` lets you obtain `fitted`, `predicted`, or `data` tibble's
##' from a compound tibble created when fitting to multiple individual data sets.
##' The specified tibble's are appended to a single output tibble.
##'
##' @param x a \code{foieGras} ssm or mpm model object
##' @param what the tibble to be grabbed; either `fitted`, `predicted` (ssm only), or
##' `data` (single letters can be used)
##' @param as_sf logical; if FALSE then return a tibble with un-projected lonlat
##' coordinates, otherwise return an sf tibble. Ignored if x is an mpm model object.
##'
##' @return a tibble with all individual tibble's appended
##'
##' @importFrom dplyr select bind_rows "%>%" everything
##' @importFrom sf st_crs st_coordinates st_transform st_geometry st_as_sf st_set_crs
##' @importFrom tibble as_tibble
##'
##' @examples
##' ## generate a fG_ssm fit object
##' xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
##' control = ssm_control(se = FALSE, verbose = 0))
##' 
##' ## grab predicted values as an un-projected tibble
##' preds <- grab(xs, what = "predicted", as_sf = FALSE)
##' 
##' @export
##'
grab <- function(x, what = "fitted", as_sf = TRUE) {

  what <- match.arg(what, choices = c("fitted","predicted","data"))

  if(!inherits(x, "fG_ssm") & !inherits(x, "fG_mpm")) 
    stop("a foieGras ssm or mpm model object with class `fG_ssm` of `fG_mpm`, respectively, must be supplied")
  if(!what %in% c("fitted","predicted","data"))
    stop("only `fitted`, `predicted` or `data` objects can be grabbed from an fG_ssm fit object")
  if(inherits(x, "fG_mpm") & what == "predicted")
    stop("predicted values do not exist for `fG_mpm` objects; use what = `fitted` instead")
  if(inherits(x, "fG_ssm")) {
    if(any(sapply(x$ssm, function(.) is.na(.$ts))) && what == "predicted")
      stop("\n there are no predicted locations because you used time.step = NA when calling `fit_ssm`. \n Either grab `fitted` values or re-fit with a positive-valued `time.step`")
  }
  
  switch(class(x)[1],
         fG_ssm = {
           ## remove optimizer crash results from extraction
           nf <- which(sapply(x$ssm, length) < 15)
           if (length(nf) > 0) {
             sprintf("%d optimizer crashes removed from output", length(nf))
             sprintf("ids: %s", x[nf, "id"])
             x <- x[-nf,]
           }
           
           out_lst <- lapply(x$ssm, function(.) {
             x <-
               switch(
                 what,
                 fitted = .$fitted,
                 predicted = .$predicted,
                 data = .$data
               )
             prj <- st_crs(x)
             xy <- st_coordinates(x) %>%
               as.data.frame(.)
             names(xy) <- c("x", "y")
             ll <- x %>%
               st_transform("+proj=longlat +datum=WGS84 +no_defs") %>%
               st_coordinates(.) %>%
               as.data.frame(.)
             names(ll) <- c("lon", "lat")
             st_geometry(x) <- NULL
             cbind(x, xy, ll)
           })
           
           if (as_sf) {
             ## get crs from fit object x, allow for different crs' among individuals to handle -180,180; 0,360 wrapping
             prj <- lapply(x$ssm, function(.)
               switch(
                 what,
                 fitted = st_crs(.$fitted),
                 predicted = st_crs(.$predicted),
                 data = st_crs(.$data)
               ))
             out <- lapply(1:length(out_lst), function(i) {
               st_as_sf(out_lst[[i]], coords = c("lon", "lat")) %>%
                 st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
                 st_transform(., prj[[i]])
             }) %>%
               bind_rows(.)
             
             if (what != "data") {
               out <- switch(
                 x$ssm[[1]]$pm,
                 rw = {
                   out %>% select(id, date, x.se, y.se, geometry)
                   },
                 crw = {
                   ## use everything() to deal w fit objects from <= 0.6-9, which don't contain s, s.se
                   out %>% select(id, date, u, v, u.se, v.se, x.se, 
                                      y.se, everything())
                 }
               )
               
             } else {
               out <-
                 out %>% select(id,
                                   date,
                                   lc,
                                   smaj,
                                   smin,
                                   eor,
                                   keep,
                                   obs.type,
                                   emf.x,
                                   emf.y,
                                   geometry)
             }
             
           } else {
             out <- do.call(bind_rows, out_lst)
             if (what != "data") {
               out <- switch(
                 x$ssm[[1]]$pm,
                 rw = out %>% select(id, date, lon, lat, x, y, x.se, y.se),
                 crw = out  %>% select(id, date, lon, lat, x, y, x.se, y.se, 
                                       u, v, u.se, v.se, everything())
               ) %>% as_tibble()
             } else {
               out <- out %>%
                 select(id,
                        date,
                        lc,
                        lon,
                        lat,
                        smaj,
                        smin,
                        eor,
                        obs.type,
                        keep,
                        x,
                        y,
                        emf.x,
                        emf.y) %>%
                 as_tibble()
             }
           }
         },
         fG_mpm = {
           ## remove optimiser crash results from extraction
           nf <- which(sapply(x$mpm, length) < 8)
           if (length(nf) > 0) {
             sprintf("%d optimiser crashes removed from output", length(nf))
             sprintf("ids: %s", x[nf, "id"])
             x <- x[-nf,]
           }

           out <- lapply(x$mpm, function(.) {
             x <-
               switch(
                 what,
                 fitted = .$fitted,
                 data = .$data
               )
             }) %>% do.call(rbind, .)
         }
         )
      
  return(out)       

}
