##' @importFrom dplyr %>% tbl_df arrange mutate select
##' @importFrom tibble as_tibble
##' @export
extract <- function(x, what = "fitted", ...) {
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }

  if(!what %in% c("fitted","predicted","data"))
    stop("Only `fitted`, `predicted` or `data` objects can be extracted")

  switch(what,
         fitted = {
           lapply(x$ssm, function(.) .$fitted) %>%
             do.call(rbind, .) %>%
             tibble::as_tibble() %>%
             dplyr::arrange(id) %>%
             dplyr::select(id, date, lat, lon, x, y, x.se, y.se, u, v, u.se, v.se)
         },
         predicted = {
           lapply(x$ssm, function(.) .$predicted) %>%
             do.call(rbind, .) %>%
             tibble::as_tibble() %>%
             dplyr::arrange(id) %>%
             dplyr::select(id, date, lat, lon, x, y, x.se, y.se, u, v, u.se, v.se)
         },
         data = {
           lapply(x$ssm, function(.) .$data) %>%
             do.call(rbind, .) %>%
             tibble::as_tibble() %>%
             dplyr::arrange(id)
         })

}
