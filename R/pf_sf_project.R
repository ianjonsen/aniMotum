##' @title handle spatial projection
##' 
##' @description project from longlat to merc or respect user-supplied 
##' projection (if not longlat) & ensure that longitudes straddling -180,180 or 
##' 0,360 are shifted appropriately.
##'
##' @param x data from `pf_sda_filter`
##' @param prj optional proj4 string to project unprojected data to, in place
##' of the default Mercator grid. Supplied by `fit_ssm(projection = )`. It is
##' used ONLY when the incoming data are NOT an sf object. Any sf object is
##' respected exactly as supplied, longlat or projected, so data prepared
##' upstream (by ArgosQC, for instance) reach the model in the projection they
##' were prepared in and are never put through a second projection.
##' @importFrom sf st_as_sf st_crs st_transform st_is_longlat
##' @keywords internal
##' @md

pf_sf_project <- function(x, prj = NULL) {
  
  if(!inherits(x, "sf")) {
    ##  if lon spans -180,180 then shift to
    ##    0,360; else if lon spans 360,0 then shift to
    ##    -180,180 ... have to do this on keep subset only
    
    xx <- subset(x, keep)
    
    if("lon" %in% names(x)) {
      coords <- c("lon", "lat")
      sf_locs <- st_as_sf(x, coords = coords, 
                          crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
      
      if (!is.null(prj)) {
        ## caller-supplied projection; longitude wrapping is handled by the
        ## central meridian it carries
        prj <- prj

      } else if (any(diff(wrap_lon(xx$lon, 0)) > 300)) {
        prj <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
      } else if (any(diff(wrap_lon(xx$lon,-180)) < -300) ||
                 any(diff(wrap_lon(xx$lon,-180)) > 300)) {
        prj <- "+proj=merc +lon_0=180 +datum=WGS84 +units=km +no_defs"
      } else {
        prj <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
      }
      
      sf_locs <-  st_transform(sf_locs, st_crs(prj))
      
    } else {
      coords <- c("x", "y")
      sf_locs <- st_as_sf(x, coords = coords, 
                          crs = st_crs("+proj=merc +units=m +datum=WGS84 +no_defs"))
      prj <- st_crs(sf_locs)$proj4string
      sf_locs <- st_transform(sf_locs, sub("units=m", "units=km", prj, fixed = TRUE))
    }
    
  } else {
    ## if input data projection is longlat then set prj merc, otherwise respect 
    ##     user-supplied projection
    ## An sf object is always respected, whatever `prj` says. aniMotum only
    ## chooses a projection for itself when the data arrive unprojected - that
    ## is, not as an sf object at all. Data prepared upstream (by ArgosQC, for
    ## instance) must reach the model in the projection they were prepared in,
    ## and must never be put through a second projection.
    if(st_is_longlat(x)) {
      prj <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
    } else {
      prj <- st_crs(x)$proj4string  
    }
  
    # if data CRS units are m then change to km, otherwise optimiser may choke
    if (grepl("units=m", prj, fixed = TRUE)) {
      message("Converting projection units from m to km for efficient optimization")
      prj <- sub("units=m", "units=km", prj, fixed = TRUE)
    }
    ## drop the lon,lat columns added by `pf_sda_filter` when the speed filter
    ##   is on. They are absent when `spdf = FALSE`, and `x[, -integer(0)]`
    ##   would select NO columns, silently discarding id, date, lc and the
    ##   error variables, so the emptiness has to be tested for.
    ll <- which(names(x) %in% c("lon","lat"))
    sf_locs <- if(length(ll) > 0) x[, -ll] else x
    sf_locs <- st_transform(sf_locs, prj)
  }
  
  return(sf_locs)
}