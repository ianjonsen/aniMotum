##' @title plot
##'
##' @description visualise foieGras SSM fits to track data
##'
##' @param x a foieGras fitted object
##' @param what specify which location estimates to display on time-series plots: fitted or predicted
##' @param outlier include all extreme outliers flagged by prefilter in plots (logical)
##' @param ... additional arguments to be ignored
##' @importFrom ggplot2 ggplot geom_point geom_path aes_string ggtitle theme_bw theme element_blank
##' @importFrom ggplot2 element_text xlab
##' @importFrom gridExtra grid.arrange
##' @importFrom magrittr "%>%"
##' @method plot foieGras
##'
##' @examples
##' ## load example foieGras fit object (to save time)
##' data(fit)
##' plot(fit$ssm[[1]])
##'
##' @export

plot.foieGras <- function(x, what = c("fitted","predicted"), outlier = FALSE, ...)
{

  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }

  what <- match.arg(what)

  f_sf <- x$fitted
  p_sf <- x$predicted
  if(!outlier) {
    d_sf <- x$data %>% filter(keep)
  } else {
    d_sf <- x$data
  }

  xy <- f_sf %>% st_coordinates(.) %>%
    as.data.frame(.)
  names(xy) <- c("x", "y")
  ll <- f_sf %>%
    st_transform(4326) %>%
    st_coordinates(.) %>%
    as.data.frame(.)
  names(ll) <- c("lon", "lat")
  f_df <- f_sf
  st_geometry(f_df) <- NULL
  f_df <- cbind(f_df, ll, xy) %>%
    select(id, date, lon, lat, x, y, x.se, y.se)

  xy <- p_sf %>% st_coordinates(.) %>%
    as.data.frame(.)
  names(xy) <- c("x", "y")
  ll <- p_sf %>%
    st_transform(4326) %>%
    st_coordinates(.) %>%
    as.data.frame(.)
  names(ll) <- c("lon", "lat")
  p_df <- p_sf
  st_geometry(p_df) <- NULL
  p_df <- cbind(p_df, ll, xy) %>%
    select(id, date, lon, lat, x, y, x.se, y.se)

  xy <- st_coordinates(d_sf) %>%
    as.data.frame(.)
  names(xy) <- c("x","y")
  ll <- d_sf %>%
    st_transform(4326) %>%
    st_coordinates(.) %>%
    as.data.frame(.)
  names(ll) <- c("lon", "lat")
  d_df <- d_sf
  st_geometry(d_df) <- NULL
  d_df <- cbind(d_df, ll, xy) %>%
    select(id, date, lon, lat, x, y)

  p1 <- ggplot() +
    geom_point(data = d_df, aes_string("lon", "lat"), shape = 19, col = grey(0.85)) +
    geom_path(data = f_df, aes_string("lon", "lat"), lwd = 0.25, col = "firebrick") +
    geom_point(data = f_df, aes_string("lon", "lat"), size = 0.75, shape = 20, col = "firebrick") +
    theme_bw() +
    ggtitle(label = paste0("id: ", f_sf$id[1], "   model: ", x$pm, "  time.step: ", x$ts, " h"), subtitle = "fitted states") +
    theme(title = element_text(size = 9), plot.subtitle = element_text(size = 8))

  p2 <- ggplot() +
    geom_point(data = d_df, aes_string("lon", "lat"), shape = 19, col = grey(0.85)) +
    geom_path(data = p_df, aes_string("lon", "lat"), lwd = 0.25, col = "dodgerblue") +
    geom_point(data = p_df, aes_string("lon", "lat"), size = 0.75, shape = 20, col = "dodgerblue") +
    theme_bw() +
    ggtitle(label = " ", subtitle = "predicted states") +
    theme(title = element_text(size = 9), plot.subtitle = element_text(size = 8))


  p3 <- ggplot() +
    geom_point(data = d_df, aes_string("date", "lon"), shape = 19, col = grey(0.85)) +
    geom_point(data = switch(what, fitted = f_df, predicted = p_df),
               aes_string("date", "lon"), size = 0.75, shape = 20,
               col = switch(what, fitted = "firebrick", predicted = "dodgerblue")) +
    theme_bw() +
    theme(title = element_text(size = 9), plot.subtitle = element_text(size = 8)) +
    xlab(element_blank())

  p4 <- ggplot() +
    geom_point(data = d_df, aes_string("date", "lat"), shape = 19, col = grey(0.85)) +
    geom_point(data = switch(what, fitted = f_df, predicted = p_df),
               aes_string("date", "lat"), size = 0.75, shape = 20,
               col = switch(what, fitted = "firebrick", predicted = "dodgerblue")) +
    theme_bw() +
    theme(title = element_text(size = 9), plot.subtitle = element_text(size = 8)) +
    xlab(element_blank())

  grid.arrange(p1, p2, p3, p4, layout_matrix = matrix(
    c(1, 2, 1, 2, 3, 3, 4, 4),
    nrow = 4,
    ncol = 2,
    byrow = T
    ))
}
