##' Deprecated functions.
##'
##' \code{extract} and \code{pluck} have been deprecated to avoid conflict with raster::extract, tidyr::extract, purrr::pluck. Instead use
##' \code{grab}.
##' @rdname foieGras-deprecated
##' @param ... ignored
##' @export
##' @rdname foieGras-deprecated
pluck <- function(...) {
  .Deprecated("grab")
}

##' @export
##' @rdname foieGras-deprecated
extract <- function(...) {
  .Deprecated("grab")
}
