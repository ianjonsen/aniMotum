##' @title Prepare Argos data for fitting state-space model
##'
##' @description \code{prefilter} (1) determines Argos data type (LS or KF);
##' (2) converts dates to POSIXt & identifies observations with duplicate dates;
##' (3) orders observations in time; (4) removes duplicate observations;
##' (5) removes observations occurring within 60 s of one another (keeps first);
##' (6) shifts longitudes that straddle -180,180 to 0,360 and vice-versa;
##' (7) projects lonlat coords to mercator x,y coords (in km);
##' (8) adds location error multiplication factors based on Argos location
##' class (for type LS);
##' (9) uses a \code{trip::sda} to identify potential outlier locations. 
##' \code{trip::sda} is a fast, vectorized version of \code{argosfilter::sdafilter}
##' see \code{?argosfilter::sdafilter} for details on implementation
##'
##' @details called by \code{fit_ssm}.
##'
##' @param x input data, must have 5 (LS), or 8 (KF) columns (see details)
##' @param vmax max travel rate (m/s)
##' @param ang angles of outlier location "spikes" (default is \code{c(15,25)} deg);
##' \code{ang = NA} turns off \code{trip::sda} filter in favour of 
##' \code{trip::speedfilter}
##' @param distlim lengths of outlier location "spikes" in km (default is 
##' \code{c(2.5, 5)} m); \code{distlim = NA} turns off \code{trip::sda} filter 
##' in favour of \code{trip::speedfilter}. Either \code{ang = NA} or 
##' \code{distlim = NA} are sufficient.
##' @param spdf turn speed filter on/off (logical; default is TRUE)
##' @param min.dt minimum allowable time difference in s between observations; 
##' \code{dt < min.dt} will be ignored by the SSM
##' @param emf optionally supplied data.frame of error multiplication factors 
##' for Argos location quality classes. see Details
##' @importFrom sf st_as_sf st_set_crs st_transform st_is_longlat st_crs
##' @importFrom trip sda speedfilter trip
##' @importFrom tibble as_tibble
##'
##' @return an sf object with all observations passed from \code{data} and the 
##' following appended columns
##' \item{\code{keep}}{logical indicating whether observation should be ignored 
##' by \code{sfilter} (FALSE)}
##' \item{\code{obs.type}}{flag indicating whether KF or LS measurement model 
##' applies}
##' \item{\code{emf_x}}{error multiplication factors for \code{x} direction}
##' \item{\code{emf_y}}{error multiplication factors for \code{y} direction}
##' \item{\code{geometry}}{sf POINT object giving \code{x,y} coordinates in km}
##'
##' @keywords internal

prefilter <-
  function(x,
           vmax = 5,
           ang = c(15,25),
           distlim = c(2500, 5000),
           spdf = TRUE,
           min.dt = 60,
           emf = NULL
           ) {

    ## check args
    if(!(is.numeric(vmax) & vmax > 0)) 
      stop("vmax must be a positive, non-zero value representing an upper speed threshold in m/s")
    if(!any((is.numeric(ang) & length(ang) == 2) || is.na(ang))) 
      stop("ang must be either a vector of c(min, max) angles in degrees defining extreme steps to be removed from trajectory, or NA")
    if(!any((is.numeric(distlim) & length(distlim) == 2) || is.na(distlim))) 
      stop("distlim must be either a vector of c(min, max) in m defining distances of extreme steps to be removed from trajectory, or NA")
    if(!is.logical(spdf)) stop("spdf must either TRUE to turn on, or FALSE to turn off speed filtering")
    if(!(is.numeric(min.dt) & min.dt >= 0)) stop("min.dt must be a positive, numeric value representing the minimum time difference between observed locations in s")
    if(!any(is.null(emf) || (is.data.frame(emf) & nrow(emf) > 0))) 
      stop("emf must be either NULL to use default emf (type emf() to see values), or a data.frame (see ?emf for details")  

    if(length(unique(x$id)) > 1) 
      stop("Multiple individual tracks in Data, use `fit_ssm(..., pf = TRUE)`")


  # ## add GL error columns, if missing
  # if((ncol(x) != 10 & !inherits(x, "sf")) | (ncol(x) != 9 & inherits(x, "sf")) | 
  #    all(!names(x) %in% c("lonerr","laterr"))) {
  #   x <- x %>%
  #     mutate(lonerr = ifelse(lc == "GL", 0.5, NA), 
  #            laterr = ifelse(lc == "GL", 1, NA))
  # }
  
  ##  flag any duplicate date records,
  ##  flag records as KF, LS, GPS or GLS
  x$keep <- with(x, difftime(date, 
                             c(as.POSIXct(NA), date[-nrow(x)]), units = "secs") 
                 > min.dt)
  x$keep <- with(x, ifelse(is.na(keep), TRUE, keep))
  x$obs.type <- NA
  x$obs.type <- with(x, 
                     ifelse(!is.na(smaj) & !is.na(smin) & !is.na(eor), 
                            "KF", obs.type))
  x$obs.type <- with(x, ifelse(lc %in% c(3,2,1,0,"A","B","Z") & 
                                 (is.na(smaj) | is.na(smin) | is.na(eor)), 
                               "LS", obs.type))
  x$obs.type <- with(x, ifelse(lc == "G" & 
                                 (is.na(smaj) | is.na(smin) |is.na(eor)), 
                               "GPS", obs.type))
  x$obs.type <- with(x, ifelse(lc == "GL" & 
                                 (is.na(smaj) | is.na(smin) | is.na(eor)) & 
                                 (!is.na(lonerr) & !is.na(laterr)), 
                               "GLS", obs.type))

  ##  if any records with smaj/smin = 0 then set to NA and obs.type to "LS"
  ## convert error ellipse smaj & smin from m to km and eor from deg to rad
  x$smaj <- with(x, ifelse(smaj == 0 | smin == 0, NA, smaj)) / 1000
  x$smin <- with(x, ifelse(smin == 0 | is.na(smaj), NA, smin)) / 1000
  x$eor <- with(x, ifelse(is.na(smaj) & is.na(smin), NA, eor)) / 180 *pi
  x$obs.type <- with(x,ifelse(any(is.na(smaj), is.na(smin), is.na(eor)) & 
                                all(obs.type != "GLS", obs.type != "GPS"), 
                              "LS", obs.type))
  x$lonerr <- with(x, lonerr * 6378.137 / 180 * pi) # convert from lon/lat to km 
  x$laterr <- with(x, laterr * 6378.137 / 180 * pi)
  
  ## check if incoming data is an sf object & if so whether it is longlat or
  ##  some other projection. If longlat then project to Mercator, if other proj
  ##  then keep as is.
  if(inherits(x, "sf") & st_is_longlat(x)) {
    
  }
  
  ## Use trip::sda to identify outlier locations
  if (spdf) {
    if(inherits(x, "sf") && st_is_longlat(x)) {

      xy <- as.data.frame(st_coordinates(x))
      names(xy) <- c("lon","lat")
      x <- cbind(x, xy)

    } else if(inherits(x, "sf") && !st_is_longlat(x)) {

      xy <- st_transform(x, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
      xy <- as.data.frame(st_coordinates(xy))
      names(xy) <- c("lon","lat")
      x <- cbind(x, xy)
    } 
    x.tr <- subset(x, keep)[, c("lon","lat","date","id","lc","smaj","smin",
                                "eor","lonerr","laterr","keep","obs.type")]
    names(x.tr)[1:2] <- c("x","y")
    x.tr <- suppressWarnings(trip(as.data.frame(x.tr), TORnames = c("date", "id"), 
                                  correct_all = FALSE))
    
    if(any(is.na(ang))) ang <- c(0,0)
    if(any(is.na(distlim))) distlim <- c(0,0)
    
    tmp <-
      suppressWarnings(try(
        sda(
          x.tr,
          smax = vmax * 3.6,    # convert m/s to km/h
          ang = ang,
          distlim = distlim / 1000     # convert m to km
        ),
      silent = TRUE)
      )
    ## screen potential sdafilter errors
    if (inherits(tmp, "try-error")) {

      warning(
        paste(
          "\ntrip::sda produced an error on id",
          x$id[1],
          "using trip::speedfilter instead"
        ),
        immediate. = TRUE
      )

      tmp <-
        suppressWarnings(try(
          speedfilter(x.tr,
            max.speed = vmax * 3.6    # convert m/s to km/h
        ),
        silent = TRUE)
        )

    if (inherits(tmp, "try-error")) {

        warning(
          paste(
            "\ntrip::speedfilter also produced an error on id",
            x$id[1],
            "can not apply speed filter prior to SSM filtering"
          ),
          immediate. = TRUE
        )
      }
    }
    x[x$keep, "keep"] <- tmp
  }
  
  if(!inherits(x, "sf")) {
    ##  if lon spans -180,180 then shift to
    ##    0,360; else if lon spans 360,0 then shift to
    ##    -180,180 ... have to do this on keep subset only

    xx <- subset(x, keep)

    ## projection not provided by user so guess at best projection
    sf_locs <- st_as_sf(x, coords = c("lon", "lat"), 
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
    ## if input data projection is longlat then set prj merc, otherwise respect 
    ##     user-supplied projection
    if(st_is_longlat(x)) {
      prj <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
    } else {
      prj <- st_crs(x)$input  
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

  ## add LS error info to corresponding records
  ## set emf's = NA if obs.type %in% c("KF","GL") - not essential but for clarity
  if(is.null(emf)) {
      tmp <- emf()
  } else if(is.data.frame(emf)) {
      tmp <- emf
  } 
  
  out <- sf_locs
  out$lc <- with(out, as.character(lc))
  out <- merge(out, tmp, by = "lc", all.x = TRUE, sort = FALSE)
  out <- out[order(out$date), c("id","date","lc","smaj","smin","eor",
                                "lonerr","laterr","keep","obs.type",
                                "emf.x","emf.y","geometry")]
  out$emf.x <- with(out, ifelse(obs.type %in% c("KF","GLS"), NA, emf.x))
  out$emf.y <- with(out, ifelse(obs.type %in% c("KF","GLS"), NA, emf.y))
  
  if (sum(is.na(out$lc)) > 0)
    stop(
      "\n NA's found in location class values"
    )
  
  out

}
