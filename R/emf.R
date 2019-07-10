##' @title emf
##'
##' @details Error Multiplication Factors for Argos (and GPS) locations. Called by \code{prefilter()}
##'
##' @importFrom tibble as_tibble
##' @importFrom dplyr mutate "%>%"
##'
##' @examples
##' emf()
##' @export

emf <- function() {

  gps_emf <- 0.1

  df <- data.frame(
    emf.x = c(gps_emf, 1, 1.54, 3.72, 13.51, 23.9, 44.22, 44.22),
    emf.y = c(gps_emf, 1, 1.29, 2.55, 14.99, 22.0, 32.53, 32.53),
    lc = c("G", "3", "2", "1", "0", "A", "B", "Z")
  ) %>%
    mutate(lc = as.character(lc))

  df %>%
    as_tibble()

}
