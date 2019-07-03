##' @title grab tbl's by name from a foieGras fitted model object
##'
##' @description `grab()` lets you obtain `fitted`, `predicted`, or `data` tbl's
##' from a compound tbl created when fitting to multiple individual data sets.
##' The specified tbl's are appended to a single output tbl.
##'
##' @param x a foieGras fitted model object
##' @param what the tibble to be grabbed; either `fitted`, `predicted`, or
##' `data` (single letters can be used)
##' @param as_sf logical; if FALSE then return a tibble with unprojected lonlat
##' coordinates, otherwise return an sf tibble
##'
##' @return a tbl with all individual tbl's appended
##'
##' @importFrom dplyr tbl_df arrange mutate select bind_rows
##' @importFrom magrittr "%>%"
##' @importFrom sf st_crs st_coordinates st_transform st_geometry st_as_sf st_set_crs
##' @importFrom tibble as_tibble
##'
##' @examples
##' ## load example foieGras fit object (to save time)
##' data(fit)
##' ## grab predicted values as an unprojected tibble
##' preds <- grab(fit, what = "p", as_sf = FALSE)
##' @export
##'
grab <- function(x, what = "fitted", as_sf = TRUE) {

  what <- match.arg(what, choices = c("fitted","predicted","data"))

  if(!what %in% c("fitted","predicted","data"))
    stop("Only `fitted`, `predicted` or `data` objects can be grabbed")


  ## remove convergence failures from extraction
  nf <- which(sapply(x$ssm, length) < 13)
  if(length(nf) > 0) {
    sprintf("%d convergence failures removed from output", length(nf))
    sprintf("ids: %s", x[nf, "id"])
    x <- x[-nf, ]
  }
    out_lst <- lapply(x$ssm, function(.) {
      x <- switch(what, fitted = .$fitted, predicted = .$predicted, data = .$data)
      prj <- st_crs(x)
      xy <- st_coordinates(x) %>%
        as.data.frame(.)
      names(xy) <- c("x", "y")
      ll <- x %>%
        st_transform(4326) %>%
        st_coordinates(.) %>%
        as.data.frame(.)
      names(ll) <- c("lon", "lat")
      st_geometry(x) <- NULL
      cbind(x, xy, ll)
    })

    if (as_sf) {
      ## get crs from fit object x, allow for different crs' among individuals to handle -180,180; 0,360 wrapping
      prj <- lapply(x$ssm, function(.)
        switch(what, fitted = st_crs(.$fitted), predicted = st_crs(.$predicted), data = st_crs(.$data))
        )
      out_sf <- lapply(1:length(out_lst), function(i) {
        st_as_sf(out_lst[[i]], coords = c("lon", "lat")) %>%
          st_set_crs(4326) %>%
          st_transform(., prj[[i]])
      }) %>%
        do.call(rbind, .)

      if(what != "data") {
      out_sf <- switch(x$ssm[[1]]$pm,
             rw = out_sf %>% select(id, date, x.se, y.se, geometry),
             crw = out_sf %>% select(id, date, u, v, u.se, v.se, x.se, y.se, geometry)
             )
      attr(out_sf, "class") <- append(class(out_sf), what, after = 1)
      return(out_sf)
      } else {
        out_sf <- out_sf %>% select(id, date, lc, smaj, smin, eor, obs.type, amf_x, amf_y, geometry)
        return(out_sf)
      }

    } else {
      out_df <- do.call(bind_rows, out_lst)
      if(what != "data") {
        out_df <- switch(x$ssm[[1]]$pm,
             rw = out_df %>% select(id, date, lon, lat, x, y, x.se, y.se),
             crw = out_df  %>% select(id, date, lon, lat, x, y, x.se, y.se, u, v, u.se, v.se)
          ) %>% as_tibble()
      } else {
        out_df <- out_df %>%
          select(id, date, lon, lat, lc, smaj, smin, eor, obs.type, x, y, amf_x, amf_y) %>%
          as_tibble()
      }
      attr(out_df, "class") <- append(class(out_df), what, after = 1)
      return(out_df)
    }


}
