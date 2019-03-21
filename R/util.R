##' @title Utility functions
##'
##' @details Internal function, typically not called by user
##'
##' @export

amf <- function() {

data.frame(
    lc = factor(
      c("3", "2", "1", "0", "A", "B", "Z"),
      levels = c("3", "2", "1", "0", "A", "B", "Z"),
      ordered = TRUE
    ),
    amf_x = c(1, 1.54, 3.72, 13.51, 23.9, 44.22, 44.22),
    amf_y = c(1, 1.29, 2.55, 14.99, 22.0, 32.53, 32.53)
#    amf_x = c(1, 1.54, 3.72, 13.51, 23.9, 120, 120),
#    amf_y = c(1, 1.29, 2.55, 14.99, 22.0, 100, 100)
    )

}

