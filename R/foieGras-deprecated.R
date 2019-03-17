#' Deprecated functions.
#'
#' \code{pluck} has been deprecated to avoid conflict with purrr::pluck. Instead use
#' \code{grab}.
#' @rdname foieGras-deprecated
#' @param ... ignored
#' @export
#' @rdname foieGras-deprecated
pluck <- function(...) {
  .Deprecated("grab")
}
