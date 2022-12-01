##' @title Find observations with duplicate dates
##'
##' @param x input data from `format_data()`
##' @param min.dt minimum allowable time difference in s between observations; 
##' `dt < min.dt` will be ignored by the SSM
##' @keywords internal
##' @md

pf_dup_dates <- function(x, min.dt) {
  
  ##  flag any duplicate date records,
  
  x$keep <- with(x, difftime(date, c(as.POSIXct(NA), date[-nrow(x)]), 
                             units = "secs") > min.dt)
  x$keep <- with(x, ifelse(is.na(keep), TRUE, keep))
  
  return(x)
}