##' @title handle spatial projection
##' 
##' @description project from longlat to merc or respect user-supplied 
##' projection (if not longlat) & ensure that longitudes straddling -180,180 or 
##' 0,360 are shifted appropriately.
##'
##' @param x data from `pf_sda_filter`
##' @importFrom sf st_as_sf st_crs st_transform st_is_longlat
##' @keywords internal
##' @md

pf_sf_project <- function(x) {
  
  if(!inherits(x, "sf")) {
    ##  if lon spans -180,180 then shift to
    ##    0,360; else if lon spans 360,0 then shift to
    ##    -180,180 ... have to do this on keep subset only
    
    xx <- subset(x, keep)
    
    if("lon" %in% names(x)) {
      coords <- c("lon", "lat")
      sf_locs <- st_as_sf(x, coords = coords, 
                          crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
      
      if (any(diff(wrap_lon(xx$lon, 0)) > 300)) {
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
    ll <- which(names(x) %in% c("lon","lat"))
    sf_locs <- x[, -ll]
    sf_locs <- st_transform(sf_locs, prj)
  }
  
  return(sf_locs)
}