##' @title fmap
##'
##' @description map foieGras fitted or predicted locations, with or without
##' Argos observations, optionally apply a different projection
##'
##' @param x a \code{foieGras} ssm fit object with class `fG_ssm`
##' @param y optionally, a \code{foieGras} mpm fit object with class `fG_mpm`; default is NULL
##' @param what specify which location estimates to map: fitted or predicted
##' @param conf include confidence regions around estimated location (logigal; default = TRUE, unless y is an mpm fit object then conf is FALSE)
##' @param obs include Argos observations on map (logical; default = FALSE)
##' @param by.date when mapping single tracks, should locations be coloured by date (logical; default = FALSE)
##' @param crs `proj4string` for re-projecting locations, if NULL the
##' default projection ("+proj=merc") for the fitting the SSM will be used
##' @param ext.rng factors to extend the plot range in x and y dimensions
##' (can exceed 1)
##' @param size size of estimated location points; optionally a vector of length 2, with size of observed locations given by 2nd value (ignored if obs = FALSE)
##' @param col colour of observed locations (ignored if obs = FALSE)
##' @importFrom ggplot2 ggplot geom_sf aes aes_string ggtitle xlim ylim unit element_text theme 
##' @importFrom ggplot2 element_blank scale_colour_manual scale_colour_gradientn scale_fill_gradientn scale_fill_manual element_line
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_as_sfc st_buffer st_crs st_coordinates st_cast st_multipolygon st_polygon st_union
##' @importFrom utils data
##' @importFrom grDevices extendrange grey
##' @importFrom dplyr summarise "%>%" group_by mutate
##' @importFrom wesanderson wes_palette
##' @importFrom assertthat assert_that
##' @export

fmap <- function(x, y = NULL,
                     what = c("fitted", "predicted"),
                     conf = TRUE,
                     obs = FALSE,
                     by.date = FALSE,
                     crs = NULL,
                     ext.rng = c(0.05, 0.05),
                     size = 0.25,
                     col = "black")
{
  what <- match.arg(what)

  assert_that(inherits(x, "fG_ssm"), msg = "x must be a foieGras ssm fit object with class `fG_ssm`")
  if(!inherits(y, "fG_mpm") & !is.null(y)) stop("y must either be NULL or a foieGras mpm fit object with class `fG_mpm`")
  
  if(inherits(x, "fG_ssm")) {
    if(length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) == 1) {
      if(!is.null(y)) {
        conf <- FALSE
        ## increase geom_point size if left at default and y is supplied
        if(size[1] == 0.25) size[1] <- 1
        sf_locs <- try(join(x, y, what.ssm = what), silent = TRUE)
        if(inherits(sf_locs, "try-error")) stop("number of rows in ssm object do not match the number of rows in mpm object, try modifying the `what` argument")
      } else {
        sf_locs <- grab(x, what=what)
        if(conf) locs <- grab(x, what = what, as_sf = FALSE)
      }
      
    } else if(length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) > 1) {
      stop("individual foieGras ssm fit objects with differing projections not currently supported")
    }

  } 
  
  if(conf) {  
    locs.lst <- split(locs, locs$id)
    conf_poly <- lapply(locs.lst, function(x) {
      conf <- lapply(1:nrow(x), function(i)
        with(x, elps(x[i], y[i], x.se[i], y.se[i], 90))
      )
      lapply(conf, function(x) st_polygon(list(x))) %>%
        st_multipolygon()
    })
    sf_conf <- st_as_sfc(conf_poly) %>%
      st_as_sf(crs = st_crs(sf_locs)) %>%
      mutate(id = unique(locs$id))
    ## dissolve individual polygons where they overlap one another
    sf_conf <- st_union(sf_conf, by_feature = TRUE) 
    }
  
  if (is.null(crs)) prj <- st_crs(sf_locs)
  else {
    prj <- crs
    if(!is.character(prj)) stop("\ncrs must be a proj4string, 
                                \neg. `+proj=stere +lat_0=-90 +lon_0=0 +ellps=WGS84 +units=km +no_defs`")
    
    if(length(grep("+units=km", prj, fixed = TRUE)) == 0) {
      cat("\nconverting units from m to km to match SSM output")
      prj <- paste(prj, "+units=km")
    }
    sf_locs <- st_transform(sf_locs, crs = prj)
    if(conf) sf_conf <- st_transform(sf_conf, crs = prj)
  }
  
  if (obs) {
    sf_data <- grab(x, "data") %>%
      st_transform(., crs = prj)
    bounds <- st_bbox(sf_data)
  } else {
    bounds <- st_bbox(sf_locs)
  }
 
  bounds[c("xmin","xmax")] <- extendrange(bounds[c("xmin","xmax")], f = ext.rng[1])
  bounds[c("ymin","ymax")] <- extendrange(bounds[c("ymin","ymax")], f = ext.rng[2])

  # sf_lines <- sf_locs %>%
  #   group_by(id) %>%
  #   summarise(do_union = FALSE) %>%
  #   st_cast("MULTILINESTRING")

  ## get coastline
  coast <- sf::st_as_sf(rworldmap::countriesLow) %>%
    st_transform(crs = prj)
  
  p <- ggplot() +
    geom_sf(data = coast,
            fill = grey(0.6),
            lwd=0) +
    xlim(bounds[c("xmin","xmax")]) +
    ylim(bounds[c("ymin","ymax")])

  if(obs)
    p <- p + geom_sf(data = sf_data, colour = col, size = ifelse(length(size) == 2, size[2], size), shape = 9, alpha = 0.75)

  if(nrow(x) > 1) {
    if(conf) {
      p <- p + geom_sf(data = sf_conf, 
                       aes_string(fill = "id"), 
                       colour = NA, 
                       lwd = 0, 
                       alpha = 0.4, 
                       show.legend = FALSE)
      }

    if(is.null(y)) {
      p <- p + geom_sf(data = sf_locs,
              aes_string(colour = "id"),
              size = ifelse(length(size) == 2, size[1], size),
              show.legend = "point"
                     ) +
      scale_colour_manual(values = 
                            wes_palette(name = "Zissou1", 
                                        n = nrow(x), 
                                        type = "continuous")
                          ) 
    } else {
      p <- p + geom_sf(data = sf_locs,
                       aes_string(colour = "g"),
                       size = ifelse(length(size) == 2, size[1], size)
      ) +
        scale_colour_gradientn(colours = 
                              rev(wes_palette(name = "Zissou1", 
                                          type = "continuous")),
                              name = expression(gamma[t]),
                              limits = c(0,1)
        ) 
    }
      
        p <- p + theme_minimal() + 
        theme(legend.position = "bottom",
            legend.text = element_text(size = 8, vjust = 0)
            )
      
      if(conf) {
        p <- p + scale_fill_manual(values = 
                                     wes_palette(name = "Zissou1", 
                                                 n = nrow(x), 
                                                 type = "continuous")
        )
      }
      
  } else {
    ##FIXME:: need to add conf ellipses here too but not sure how to colour them,
    ##FIXME:: given the track is coloured by date - perhaps make this optional but
    ##FIXME:: if on then conf ellipse is a single colour, or colour ellipses by date
    ##FIXME:: but don't dissolve them with st_union, otherwise colouring by date won't work...
    if(by.date) lab_dates <- with(sf_locs, pretty(seq(min(date), max(date), l = 5))) %>% as.Date()

    if(conf & !by.date) {
      p <- p + geom_sf(data = sf_conf, 
                       fill = wes_palette("Zissou1", n=20, "continuous")[4],
                       colour = NA, 
                       lwd = 0, 
                       alpha = 0.5, 
                       show.legend = FALSE)
    } else if(conf & by.date) {
      p <- p + geom_sf(data = sf_conf, 
                       fill = grey(0.5),
                       colour = NA, 
                       lwd = 0, 
                       alpha = 0.25, 
                       show.legend = FALSE)
    }
    # p <- p +
    #   geom_sf(data = sf_lines,
    #                  colour = "dodgerblue",
    #                  size = 0.1) +
    if(by.date) {
      p <- p + geom_sf(data = sf_locs,
                    aes(colour = as.numeric(as.Date(date))),
                     size = ifelse(length(size) == 2, size[1], size)
                     ) +
        scale_colour_gradientn(breaks = as.numeric(lab_dates), 
                               colours = wes_palette(name = "Zissou1", 
                                                     type = "continuous"), 
                               labels = lab_dates)
    } else {
      p <- p + geom_sf(data = sf_locs,
                       colour = wes_palette("Zissou1", n=5, "discrete")[2],
                       size = ifelse(length(size) == 2, size[1], size)
      )
      
    }
      
      p <- p + labs(title = paste("id:", x$id)) +
        theme_minimal() +
        theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 8, vjust = 0),
            legend.key.width = unit(0.12, "npc"),
            legend.key.height = unit(0.025, "npc"),
            panel.grid = element_line(size = 0.2)
      )
  }

  return(p)
}
