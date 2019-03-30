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
##' (9) uses a argosfilter::sdafilter to identify potential outlier locations
##' (by distance only) to be ignored when fitting the \code{ctrw} model
##'
##' @details called by \code{fit_ssm}.
##'
##' @param data input data - must have 5 (LS), or 8 (KF) columns (see details)
##' @param vmax max travel rate (m/s) - see ?argosfilter::sdafilter for details
##' @param ang angles of outlier location "spikes" - see ?argosfilter::sdafilter for details
##' @param distlim lengths of outlier location "spikes" - see ?argosfilter::sdafilter for details
##' @param spdf turn speed filter on/off (logical; default is TRUE)
##' @param min.dt minimum allowable time difference between observations; dt < min.dt will be ignored by the SSM
##' @importFrom lubridate ymd_hms
##' @importFrom dplyr mutate distinct arrange filter select left_join lag rename
##' @importFrom magrittr "%>%"
##' @importFrom sf st_as_sf st_set_crs st_transform st_is_longlat st_crs
##' @importFrom argosfilter sdafilter
##' @importFrom tibble as_tibble
##' @importFrom stringr str_detect str_replace
##'
##' @return an sf object with all observations passed from \code{data} and the following appended columns
##' \item{\code{keep}}{logical indicating whether observation should be ignored by \code{sfilter} (FALSE)}
##' \item{\code{obs.type}}{flag indicating whether KF or LS measurement model applies}
##' \item{\code{amf_x}}{Argos error multiplication factor for x direction}
##' \item{\code{amf_y}}{Argos error multiplication factor for y direction}
##' \item{\code{geometry}}{sf POINT object giving x,y coordinates in km}
##'
##' @examples
##' data(ellie)
##' pf <- prefilter(ellie, vmax=10, ang=c(15,25), min.dt=120)
##' pf
##'
##' @export

prefilter <-
  function(data,
           vmax = 50,
           ang = -1,
           distlim = c(2500, 5000),
           spdf = TRUE,
           min.dt = 60) {

  d <- data
  # check input data
  if (!inherits(d, "sf")) {
    if (!ncol(d) %in% c(5, 8))
      stop("\nData can only have 5 (for LS data) or 8 (for KF data) columns")

    if ((ncol(d) == 5 &
         !isTRUE(all.equal(
           names(d), c("id", "date", "lc", "lon", "lat")
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
      (ncol(d) == 4 & !isTRUE(all.equal(
        names(d), c("id", "date", "lc", "geometry")))))
      stop("\nUnexpected column names in Data, type`?fit_ssm` for details")

    if(is.na(st_crs(d))) stop("\nCRS info is missing from input data sf object")
  }

  if(length(unique(d$id)) > 1) stop("Multiple individual tracks in Data, use `fit_ssm`")

  if(!is.null(d$id)) d <- d %>% mutate(id = as.character(id))

  ## add KF error columns, if missing
  if(ncol(d) %in% c(4,5)) {
    d <- d %>%
      mutate(smaj = NA, smin = NA, eor = NA)
  }

  ##  convert dates to POSIXt
  ##  flag any duplicate date records,
  ##  order records by time,
  ##  set lc to ordered factor,
  ##  flag records as either KF or LS,

  d <- d %>%
    mutate(date = ymd_hms(date, tz = "GMT")) %>%
    mutate(keep = difftime(date, lag(date), units = "secs") > min.dt) %>%
    mutate(keep = ifelse(is.na(keep), TRUE, keep)) %>%
    arrange(order(date)) %>%
    mutate(lc = factor(lc, levels = c(3,2,1,0,"A","B","Z"), ordered = TRUE)) %>%
    mutate(obs.type = ifelse(is.na(smaj) | is.na(smin) | is.na(eor), "LS", "KF"))

  ##  if any records with smaj/smin = 0 then set to NA and obs.type to "LS"
  ## convert error ellipse smaj & smin from m to km and eor from deg to rad
  d <- d %>%
    mutate(smaj = ifelse(smaj == 0 | smin == 0, NA, smaj),
           smin = ifelse(smin == 0 | is.na(smaj), NA, smin),
           eor = ifelse(is.na(smaj) | is.na(smin), NA, eor),
           obs.type = ifelse(is.na(smaj) & is.na(smin), "LS", obs.type)) %>%
    mutate(smaj = smaj/1000,
           smin = smin/1000,
           eor = eor/180 * pi)

  ## Use argosfilter::sdafilter to identify outlier locations
  if (spdf) {
    if(inherits(d, "sf") && st_is_longlat(d)) {
      xy <- st_coordinates(d) %>%
        as_tibble() %>%
        rename(lon = X, lat = Y)
      d <- bind_cols(d, xy)
    } else if(inherits(d, "sf") && !st_is_longlat(d)) {
      xy <- st_transform(d, 4326) %>%
        st_coordinates() %>%
        as_tibble() %>%
        rename(lon = X, lat = Y)
      d <- bind_cols(d, xy)
    }
    filt <- rep("not", nrow(d))
    tmp <-
      suppressWarnings(try(with(
        subset(d, keep),
        sdafilter(
          lat,
          lon,
          date,
          lc,
          vmax = vmax,
          ang = ang,
          distlim = distlim
        )
      ),
      silent = TRUE)
      )
    ## screen potential sdafilter errors
    if (!inherits(tmp, "try-error"))
    {
      filt[d$keep] <- tmp
      d <- d %>%
        mutate(keep = ifelse(filt == "removed", FALSE, keep))
    } else if (inherits(tmp, "try-error")) {
      warning(
        paste(
          "\nargosfilter::sdafilter produced an error on id",
          d$id[1],
          "unable to apply speed filter"
        ),
        immediate. = TRUE
      )
    }
  }

  if(!inherits(d, "sf")) {
    ##  if lon spans -180,180 then shift to
    ##    0,360; else if lon spans 360,0 then shift to
    ##    -180,180 ... have to do this on keep subset only
    dd <- subset(d, keep)

    ## build user-specified projection
    mlon <- mean(dd$lon) %>% round(., 2)

    ## projection not provided by user so guess at best projection
    sf_locs <- st_as_sf(d, coords = c("lon", "lat"), crs = 4326)

    if (any(diff(wrap_lon(dd$lon, 0)) > 300)) {
      prj <- "+init=epsg:3395 +units=km +lon_0=0"
    } else if (any(diff(wrap_lon(dd$lon,-180)) < -300) ||
               any(diff(wrap_lon(dd$lon,-180)) > 300)) {
      prj <- "+init=epsg:3395 +units=km +lon_0=180"
    } else {
      prj <- "+init=epsg:3395 +units=km"
    }

    sf_locs <- sf_locs %>% st_transform(., prj)

    if (max(dd$lat) <= -60) {
      prj <- paste0("+init=epsg:3031 +units=km +lon_0=", mlon)
      sf_locs <- sf_locs %>% st_transform(., prj)
    } else if (min(dd$lat) >= 60) {
      prj <- paste0("+init=epsg:3995 +units=km +lon_0=", mlon)
      sf_locs <- sf_locs %>% st_transform(., prj)
    }


  } else {
    prj <- st_crs(d)
    # if data CRS units are m then change to km, otherwise optimiser may choke
    if (str_detect(prj$proj4string, "units=m")) {
      prj$proj4string <-
        str_replace(prj$proj4string, "units=m", "units=km")
    }
    sf_locs <- d %>%
      select(-lon,-lat) %>%
      st_transform(prj)
  }

  ## add LS error info to corresponding records
  ## set amf's = NA if obs.type == "KF" - not essential but for clarity
  tmp <- amf()
  out <- sf_locs %>%
    left_join(., tmp, by = "lc") %>%
    mutate(
      amf_x = ifelse(obs.type == "KF", NA, amf_x),
      amf_y = ifelse(obs.type == "KF", NA, amf_y)
    )

  if (sum(is.na(out$lc)) > 0)
    stop(
      "\n NA's found in location class values,\n
      perhaps your input lc's != c(3,2,1,0,`A`,`B`,`Z`)?"
    )

  return(out)

}
