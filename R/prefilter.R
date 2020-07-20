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
##' @param data input data, must have 5 (LS), or 8 (KF) columns (see details)
##' @param vmax max travel rate (m/s)
##' @param ang angles of outlier location "spikes" (default is \code{c(15,25)} deg); \code{ang = NA} turns off \code{trip::sda} filter in favour of \code{trip::speedfilter}
##' @param distlim lengths of outlier location "spikes" (default is \code{c(2500, 5000)} m); \code{distlim = NA} turns off \code{trip::sda} filter in favour of \code{trip::speedfilter}. Either \code{ang = NA} or \code{distlim = NA} are sufficient.
##' @param spdf turn speed filter on/off (logical; default is TRUE)
##' @param min.dt minimum allowable time difference in s between observations; \code{dt < min.dt} will be ignored by the SSM
##' @param emf optionally supplied data.frame of error multiplication factors for Argos location quality classes. see Details
##' @importFrom lubridate ymd_hms
##' @importFrom dplyr mutate arrange select left_join lag rename "%>%" everything
##' @importFrom sf st_as_sf st_set_crs st_transform st_is_longlat st_crs
##' @importFrom trip sda speedfilter trip
##' @importFrom tibble as_tibble
##' @importFrom stringr str_detect str_replace
##' @importFrom assertthat assert_that
##'
##' @return an sf object with all observations passed from \code{data} and the following appended columns
##' \item{\code{keep}}{logical indicating whether observation should be ignored by \code{sfilter} (FALSE)}
##' \item{\code{obs.type}}{flag indicating whether KF or LS measurement model applies}
##' \item{\code{emf_x}}{error multiplication factors for \code{x} direction}
##' \item{\code{emf_y}}{error multiplication factors for \code{y} direction}
##' \item{\code{geometry}}{sf POINT object giving \code{x,y} coordinates in km}
##'
##' @keywords internal

prefilter <-
  function(data,
           vmax = 5,
           ang = c(15,25),
           distlim = c(2500, 5000),
           spdf = TRUE,
           min.dt = 60,
           emf = NULL
           ) {

    ## check args
    assert_that(is.numeric(vmax) & vmax > 0, 
                msg = "vmax must be a positive, non-zero value representing an upper speed threshold in m/s")
    assert_that(any((is.numeric(ang) & length(ang) == 2) || is.na(ang)), 
                msg = "ang must be either a vector of c(min, max) angles in degrees defining extreme steps to be removed from trajectory, or NA")
    assert_that(any((is.numeric(distlim) & length(distlim) == 2) || is.na(distlim)),
                msg = "distlim must be either a vector of c(min, max) in m defining distances of extreme steps to be removed from trajectory, or NA")
    assert_that(is.logical(spdf), 
                msg = "spdf must either TRUE to turn on, or FALSE to turn off speed filtering")
    assert_that(is.numeric(min.dt) & min.dt >= 0,
                msg = "min.dt must be a positive, numeric value representing the minimum time difference between observed locations in s")
    assert_that(any(is.null(emf) || (is.data.frame(emf) & nrow(emf) > 0)),
                msg = "emf must be either NULL to use default emf (type emf() to see values), or a data.frame (see ?emf for details")  

  d <- data
  
  ## check data
  if (!inherits(d, "sf")) {
    if (!ncol(d) %in% c(5, 7, 8))
      stop("\nData can only have 5 (for LS data), 7 (for geolocation data), or 8 (for KF(S) data) columns")

    if ((ncol(d) == 5 &
         !isTRUE(all.equal(
           names(d), c("id", "date", "lc", "lon", "lat")
         ))) ||
        (ncol(d) == 7 &
         !isTRUE(all.equal(
           names(d),
           c("id", "date", "lc", "lon", "lat", "lonerr", "laterr")
         ))) ||
        (ncol(d) == 8 &
         !isTRUE(all.equal(
           names(d),
           c("id", "date", "lc", "lon", "lat", "smaj", "smin", "eor")
         ))))
      stop("\nUnexpected column names in Data, type `?fit_ssm` for details")
  } else if(inherits(d, "sf") && inherits(st_geometry(d), "sfc_POINT")){
    if((ncol(d) == 7 &
        !isTRUE(all.equal(
          names(d), c("id", "date", "lc", "smaj", "smin", "eor", "geometry")))) ||
      (ncol(d) ==  6 & 
        !isTRUE(all.equal(
          names(d), c("id", "date", "lc", "lonerr", "laterr", "geometry")))) ||
      (ncol(d) == 4 & !isTRUE(all.equal(
        names(d), c("id", "date", "lc", "geometry")))))
      stop("\nUnexpected column names in Data, type`?fit_ssm` for details")

    if(is.na(st_crs(d))) stop("\nCRS info is missing from input data sf object")
  }

  if(length(unique(d$id)) > 1) stop("Multiple individual tracks in Data, use `fit_ssm(..., pf = TRUE)`")

  if(!is.null(d$id)) d <- d %>% mutate(id = as.character(id))

  ## add KF error columns, if missing
  if((ncol(d) %in% c(4,5,7) & !inherits(d, "sf")) | (ncol(d) == 6 & inherits(d, "sf"))) {
    d <- d %>%
      mutate(smaj = NA, smin = NA, eor = NA)
  } 
  ## add GL error columns, if missing
  if((ncol(d) != 10 & !inherits(d, "sf")) | (ncol(d) != 9 & inherits(d, "sf"))) {
    d <- d %>%
      mutate(lonerr = NA, laterr = NA)
  }

  ##  convert dates to POSIXt
  ##  order records by time,
  ##  flag any duplicate date records,
  ##  flag records as either KF or LS,
  d <- d %>%
    mutate(date = ymd_hms(date, tz = "GMT")) %>%
    arrange(date) %>%
    mutate(keep = difftime(date, lag(date), units = "secs") > min.dt) %>%
    mutate(keep = ifelse(is.na(keep), TRUE, keep)) %>%
    mutate(obs.type = NA) %>%
    mutate(obs.type = ifelse(!is.na(smaj) & !is.na(smin) & !is.na(eor), "KF", obs.type)) %>%
    mutate(obs.type = ifelse(lc %in% c(3,2,1,0,"A","B","Z") & (is.na(smaj) | is.na(smin) |is.na(eor)), "LS", obs.type)) %>%
    mutate(obs.type = ifelse(lc == "G" & (is.na(smaj) | is.na(smin) |is.na(eor)), "GPS", obs.type)) %>%
    mutate(obs.type = ifelse(lc == "GL" & (is.na(smaj) | is.na(smin) |is.na(eor)) & (!is.na(lonerr) & !is.na(laterr)), "GLS", obs.type))
 
  
  ##  if any records with smaj/smin = 0 then set to NA and obs.type to "LS"
  ## convert error ellipse smaj & smin from m to km and eor from deg to rad
  d <- d %>%
    mutate(smaj = ifelse(smaj == 0 | smin == 0, NA, smaj),
           smin = ifelse(smin == 0 | is.na(smaj), NA, smin),
           eor = ifelse(is.na(smaj) & is.na(smin), NA, eor),
           obs.type = ifelse(is.na(smaj) & is.na(smin) & (obs.type != "GLS" & obs.type != "GPS"), "LS", obs.type)) %>%
    mutate(smaj = smaj/1000,
           smin = smin/1000,
           eor = eor/180 * pi) %>%
    mutate(lonerr = lonerr * 6366.71 / 180 * pi,
           laterr = laterr * 6366.71 / 180 * pi) # convert from lon/lat to km (crude)
  
  ## Use trip::sda to identify outlier locations
  if (spdf) {
    if(inherits(d, "sf") && st_is_longlat(d)) {

      xy <- st_coordinates(d) %>%
        as_tibble() %>%
        rename(lon = X, lat = Y)
      d <- bind_cols(d, xy)

    } else if(inherits(d, "sf") && !st_is_longlat(d)) {

      xy <- st_transform(d, crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) %>%
        st_coordinates() %>%
        as_tibble() %>%
        rename(lon = X, lat = Y)
      d <- bind_cols(d, xy)

    } 
    d.tr <- subset(d, keep) %>% 
      select(lon,lat,date,id, everything()) %>% 
      rename(x = lon, y = lat)
    d.tr <- suppressWarnings(trip(d.tr, TORnames = c("date", "id"), correct_all = FALSE))
    
    if(any(is.na(ang))) ang <- c(0,0)
    if(any(is.na(distlim))) distlim <- c(0,0)
    
    tmp <-
      suppressWarnings(try(
        sda(
          d.tr,
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
          d$id[1],
          "using trip::speedfilter instead"
        ),
        immediate. = TRUE
      )

      tmp <-
        suppressWarnings(try(
          speedfilter(d.tr,
            max.speed = vmax * 3.6    # convert m/s to km/h
        ),
        silent = TRUE)
        )

    if (inherits(tmp, "try-error")) {

        warning(
          paste(
            "\ntrip::speedfilter also produced an error on id",
            d$id[1],
            "can not apply speed filter prior to SSM filtering"
          ),
          immediate. = TRUE
        )
      }
    }
    d[d$keep, "keep"] <- tmp
  }
  
  if(!inherits(d, "sf")) {
    ##  if lon spans -180,180 then shift to
    ##    0,360; else if lon spans 360,0 then shift to
    ##    -180,180 ... have to do this on keep subset only
    dd <- subset(d, keep)

    ## build user-specified projection
    mlon <- mean(dd$lon) %>% round(., 2)

    ## projection not provided by user so guess at best projection
    sf_locs <- st_as_sf(d, coords = c("lon", "lat"), crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

    if (any(diff(wrap_lon(dd$lon, 0)) > 300)) {
      prj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"
    } else if (any(diff(wrap_lon(dd$lon,-180)) < -300) ||
               any(diff(wrap_lon(dd$lon,-180)) > 300)) {
      prj <- "+proj=merc +lon_0=180 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"
    } else {
      prj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"
    }

    sf_locs <- sf_locs %>% st_transform(., st_crs(prj))

  } else {
    prj <- st_crs(d)
    # if data CRS units are m then change to km, otherwise optimiser may choke
    if (str_detect(prj$input, "units=m")) {
      prj$input <-
        str_replace(prj$input, "units=m", "units=km")
    }
    sf_locs <- d %>%
      select(-lon,-lat) %>%
      st_transform(st_crs(prj))
  }

  ## add LS error info to corresponding records
  ## set emf's = NA if obs.type %in% c("KF","GL") - not essential but for clarity
  if(is.null(emf)) {
      tmp <- emf()
  } else if(is.data.frame(emf)) {
      tmp <- emf
  } else {
      stop("\n supplied emf must be a data.frame. see `?prefilter`")
    }

  out <- sf_locs %>%
    mutate(lc = as.character(lc)) %>%
    left_join(., tmp, by = "lc") %>%
    mutate(
      emf.x = ifelse(obs.type %in% c("KF","GLS"), NA, emf.x),
      emf.y = ifelse(obs.type %in% c("KF","GLS"), NA, emf.y)
    ) %>%
    select(everything(), geometry)

  if (sum(is.na(out$lc)) > 0)
    stop(
      "\n NA's found in location class values"
    )
  
  return(out)

}
