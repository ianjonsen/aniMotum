##' @title grab tibble's by name from a foieGras model object
##'
##' @description `grab()` lets you obtain `fitted`, `predicted`, `rerouted` or 
##' `data` tibble's from a compound tibble created when fitting to multiple 
##' individual data sets. The specified tibble's are appended to a single output
##' tibble.
##'
##' @param x a \code{foieGras} ssm or mpm model object
##' @param what the tibble to be grabbed; either `fitted`, `predicted`, 
##' `rerouted` (ssm only), or `data` (single letters can be used).
##' @param as_sf logical; if FALSE (default) then return a tibble with 
##' un-projected lonlat coordinates, otherwise return an sf tibble. Ignored if x
##' is an mpm model object.
##' @param normalise logical, should move persistence estimates (g) be normalised 
##' to 0, 1 (default = FALSE). Ignored if what is `data` or `rerouted`.
##' @param group logical; should move persistence be normalised among individuals 
##' as a group (ie. relative) or separately (default = FALSE)
##'
##' @return a tibble with all individual tibble's appended
##'
##' @importFrom sf st_crs st_coordinates st_transform st_geometry st_as_sf st_set_crs
##' @importFrom dplyr group_by mutate ungroup "%>%"
##' @importFrom tibble as_tibble
##'
##' @examples
##' ## generate an ssm fit object
##' xs <- fit_ssm(sese2, spdf=FALSE, model = "rw", time.step=72, 
##' control = ssm_control(verbose = 0))
##' 
##' ## grab predicted values as an un-projected tibble
##' preds <- grab(xs, what = "predicted")
##' 
##' @export
##'
grab <- function(x, what = "fitted", as_sf = FALSE, normalise = FALSE, group = FALSE) {

  what <- match.arg(what, choices = c("fitted","predicted","rerouted","data"))

  if(!any(inherits(x, "ssm_df"), inherits(x, "mpm_df"), inherits(x, "fG_ssm"), inherits(x, "fG_mpm"))) 
    stop("a foieGras ssm or mpm model object must be supplied")
  if(!what %in% c("fitted","predicted","rerouted","data"))
    stop("only `fitted`, `predicted`, `rerouted`, or `data` objects can be grabbed from an ssm fit object")
  if(any(inherits(x, "mpm_df"), inherits(x, "fG_mpm")) & what == "predicted")
    stop("predicted values do not exist for `mpm` objects; use what = `fitted` instead")
  if(any(inherits(x, "ssm_df"), inherits(x, "fG_ssm"))) {
    if(any(sapply(x$ssm, function(.) is.na(.$ts))) && what == "predicted")
      stop("\n there are no predicted locations because you used time.step = NA when calling `fit_ssm`. 
           \n Either grab `fitted` values or re-fit with a positive integer value for `time.step`")
  }

  ## coerce old foieGras classes "fG_ssm" and "fG_mpm" to new classes
  if(inherits(x, "fG_ssm")) class(x)[1] <- "ssm_df"
  if(inherits(x, "fG_mpm")) class(x)[1] <- "mpm_df"
  
  switch(class(x)[1],
         ssm_df = {
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
                 rerouted = .$rerouted,
                 data = .$data
               )
             prj <- st_crs(x)
             xy <- as.data.frame(st_coordinates(x))
             names(xy) <- c("x", "y")
             ll <- st_transform(x, "+proj=longlat +datum=WGS84 +no_defs")
             ll <- as.data.frame(st_coordinates(ll))
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
                 rerouted = st_crs(.$rerouted),
                 data = st_crs(.$data)
               ))
             out <- lapply(1:length(out_lst), function(i) {
                st_as_sf(out_lst[[i]], coords = c("lon", "lat")) %>%
                  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
                  st_transform(prj[[i]])
             })
             out <- do.call(rbind, out)
             
             if (what %in% c("fitted","predicted")) {
               out <- switch(
                 x$ssm[[1]]$pm,
                 rw = {
                   out[, c("id", "date", "x.se", "y.se", "geometry")]
                   },
                 crw = {
                   ## deal w fit objects from <= 0.6-9, which don't contain s, s.se
                   if(all(c("s","s.se") %in% names(out))) {
                     out[, c("id", "date", "u", "v", "u.se", "v.se", "x.se", 
                             "y.se", "s", "s.se", "geometry")]
                     } else {
                       out[, c("id", "date", "u", "v", "u.se", "v.se", "x.se", 
                             "y.se", "geometry")]
                       }
                   },
                 mp = {
                   out[, c("id", "date", "x", "y", "x.se", "y.se", 
                           "logit_g", "logit_g.se", "g", "geometry")]
                   if(normalise & !group) {
                     out <- out %>% 
                       group_by(id) %>%
                       mutate(g = (g - min(g))/(max(g) - min(g))) %>%
                       ungroup()
                     } else if(normalise & group) {
                     out <- out %>% 
                       mutate(g = (g - min(g))/(max(g) - min(g)))
                   }
                   out
                 })
               
             } else if (what == "rerouted") {
               out <- out[, c("id", "date", "x.se", "y.se", "geometry")]
             } else if (what == "data") {
               out <- out[, c("id", "date", "lc", "smaj", "smin", "eor", "keep", 
                         "obs.type", "emf.x", "emf.y", "geometry")]
             }
             
           } else {
             out <- do.call(rbind, out_lst)
             if (what %in% c("fitted","predicted")) {
               out <- switch(
                 x$ssm[[1]]$pm,
                 rw = {
                   out[, c("id", "date", "lon", "lat", "x", "y", "x.se", "y.se")]
                   },
                 crw = {
                   if(all(c("s","s.se") %in% names(out))) {
                     out[, c("id", "date", "lon", "lat", "x", "y", "x.se", 
                             "y.se", "u", "v", "u.se", "v.se", "s", "s.se")]
                     } else {
                       out[, c("id", "date", "lon", "lat", "x", "y", 
                             "x.se", "y.se", "u", "v", "u.se", "v.se")]
                       }
                   },
                 mp = {
                    out <- out[, c("id", "date", "lon", "lat", "x", "y", 
                            "x.se", "y.se", "logit_g", "logit_g.se", "g")]
                    if(normalise & !group) {
                      out <- out %>% 
                        group_by(id) %>%
                        mutate(g = (g - min(g))/(max(g) - min(g))) %>%
                        ungroup()
                    } else if(normalise & group) {
                      out <- out %>% 
                        mutate(g = (g - min(g))/(max(g) - min(g)))
                    }
                    out
                 })
               
               out <- as_tibble(out)
             } else if (what == "rerouted") {
               out <- out[, c("id", "date", "lon", "lat", "x", "y", "x.se", "y.se")]
               out <- as_tibble(out)
             } else if (what == "data") {
               out <- out[, c("id", "date", "lc", "lon", "lat", 
                              "smaj", "smin", "eor", "obs.type", "keep", 
                              "x", "y", "emf.x", "emf.y")]
               out <- as_tibble(out)
             }
           }
         },
         mpm_df = {
           ## remove optimiser crash results from extraction
           nf <- which(sapply(x$mpm, length) < 8)
           if (length(nf) > 0) {
             sprintf("%d optimiser crashes removed from output", length(nf))
             sprintf("ids: %s", x[nf, "id"])
             x <- x[-nf,]
           }

           out <- lapply(x$mpm, function(.) {
             x <- switch(what, fitted = .$fitted, data = .$data)
             }) 
           out <- do.call(rbind, out)
           out <- as_tibble(out)
           if(normalise & !group) {
             out <- out %>% 
               group_by(id) %>%
               mutate(g = (g - min(g))/(max(g) - min(g))) %>%
               ungroup()
           } else if(normalise & group) {
             out <- out %>% 
               mutate(g = (g - min(g))/(max(g) - min(g)))
           }
           out
         }
         )
      
  return(out)

}
