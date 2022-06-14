##' @title aes_lst
##' 
##' @description set aesthetics as a named list for: 1) mapping i) estimated 
##' locations; ii) estimated confidence ellipses; iii) estimated track lines; 
##' iv) observed locations; v) land masses; vi) water bodies; and 2) colour 
##' palettes for i) behavioural indices or ii) individual animal tracks
##' 
##' @param est logical; turns on estimated locations (default = TRUE)
##' @param conf logical; turns on estimated confidence ellipses. Default varies 
##' depending on whether behavioural index is being mapped &/or if single versus
##' multiple tracks are being mapped.
##' @param line logical; turns on estimated track line(s) (default varies)
##' @param mp logical; turns on move persistence index (default = TRUE, if present in model fit)
##' @param obs logical; turns on observed locations (default = FALSE)
##' @param shape gpplot2 shape value (integer: 0, 25) for estimated & observed 
##' locations 
##' @param size ggplot2 size value for estimated locations & track lines, and 
##' observed locations
##' @param col colour for estimated locations and track lines, and observed 
##' locations
##' @param fill fill colour for estimated locations & confidence ellipses, 
##' observed locations, land polygons, and water
##' @param alpha transparency for specified fills/colours
##' @param mp_pal continuous colour palette for move persistence values 
##' @param id_pal discrete colour palette for track id's
##' @param date_pal continuous colour palette for displaying date along track
##' 
##' @return a named list, with 9 elements, of map components and aesthetics  
##' 1. elements 1-5 - map components: `est`, `conf`, `line`, `mp`, `obs`
##' 2. element 6: a data.frame named `df` containing ggplot2 aesthetics: `shape`,  
##'    `size`, `col`, `fill`, and `alpha`. `df` has 6 rows, corresponding to the map
##'    features:  estimated locations, confidence ellipses, track lines, observed
##'    locations, land polygons, and water  
##' 3. element 7: `mp_pal`  
##' 4. element 8: `id_pal`
##' 5. element 9: `date_pal`
##' @importFrom grDevices hcl.colors
##' @examples 
##' # generate custom aes list
##' aes <- aes_lst(conf = FALSE, mp_pal = hcl.colors(n=100, palette = "RdBu"))
##' 
##' # modify aesthetics
##' aes$df$size[1] <- 1.5
##' aes$df$fill[6] <- grey(0.9)
##' 
##' @export

aes_lst <- function(est = TRUE, conf = TRUE, line = FALSE, mp = TRUE, obs = FALSE,
                    shape = c(19, NA, NA, 17, NA, NA),
                    size = c(1.25, NA, 0.2, 0.8, NA, NA),
                    col = c("dodgerblue", NA, "grey50", "orange", NA, NA),
                    fill = c("dodgerblue", "dodgerblue", NA, "orange", "grey60", "grey85"),
                    alpha = c(1, 0.4, 1, 1, 1, NA),
                    mp_pal = hcl.colors(100, palette = "Plasma", rev = FALSE),
                    id_pal = "Harmonic",
                    date_pal = hcl.colors(100, palette = "Viridis", rev = FALSE)
) {
  
  stopifnot("aesthetic vectors must have length = 6
            type aes_lst()$df to see required final aes structure" = 
              all(length(shape) == 6, 
                  length(size) == 6, 
                  length(col) == 6, 
                  length(fill) == 6,
                  length(alpha) == 6))
  
  list(est = est,
       conf = conf,
       line = line,
       mp = mp,
       obs = obs,
       df = data.frame(
         feature = c("estimated locations",
                     "confindence ellipses",
                     "track lines",
                     "observed locations",
                     "land polygons",
                     "water"),
         shape = shape,
         size = size,
         col = col,
         fill = fill,
         alpha = alpha
       ),
       mp_pal = mp_pal,
       id_pal = id_pal,
       date_pal = date_pal
  )
  
}
