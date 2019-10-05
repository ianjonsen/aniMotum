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
##' (9) uses a \code{argosfilter::sdafilter} to identify potential outlier locations
##' (by distance only) to be ignored when fitting the \code{ctrw} model
##'
##' @details called by \code{fit_ssm}.
##'
##' @param data input data - must have 5 (LS), or 8 (KF) columns (see details)
##' @param vmax max travel rate (m/s) - see \code{?argosfilter::sdafilter} for details
##' @param ang angles of outlier location "spikes" - see \code{?argosfilter::sdafilter} for details
##' @param distlim lengths of outlier location "spikes" - see \code{?argosfilter::sdafilter} for details
##' @param spdf turn speed filter on/off (logical; default is TRUE)
##' @param min.dt minimum allowable time difference between observations; \code{dt < min.dt} will be ignored by the SSM
##' @param emf optionally supplied data.frame of error multiplication factors for Argos location quality classes. see Details
##' @importFrom lubridate ymd_hms
##' @importFrom dplyr mutate distinct arrange filter select left_join lag rename "%>%" everything
##' @importFrom sf st_as_sf st_set_crs st_transform st_is_longlat st_crs
##' @importFrom argosfilter sdafilter vmask
##' @importFrom tibble as_tibble
##' @importFrom stringr str_detect str_replace
##'
##' @details User-specified Error Multiplication Factors (emf). emf's must be provided as a data.frame with the following columns:
##'
##' \code{emf.x} {emf values for the \code{x} direction}
##'
##' \code{emf.y} {emf values for \code{y} direction}
##'
##' \code{lc} {location class designations}
##'
##' The location class designations can be the standard Argos lc values: 3, 2, 1, 0, A, B, Z or other values. The number of classes specified is flexible though may not be amenable to a large number of classes. Whatever class designations are chosen must also appear in the input data \code{lc} column. A GPS location class ("G") is provided by default and assumes that GPS locations are 10 x more precise than Argos lc 3 locations.
##'
##' @return an sf object with all observations passed from \code{data} and the following appended columns
##' \item{\code{keep}}{logical indicating whether observation should be ignored by \code{sfilter} (FALSE)}
##' \item{\code{obs.type}}{flag indicating whether KF or LS measurement model applies}
##' \item{\code{emf_x}}{error multiplication factors for \code{x} direction}
##' \item{\code{emf_y}}{error multiplication factors for \code{y} direction}
##' \item{\code{geometry}}{sf POINT object giving \code{x,y} coordinates in km}
##'
##' @examples
##' data(ellie)
##' pf <- prefilter(ellie, vmax=4, ang=c(15,25), min.dt=120)
##' pf
##'
##' @export

prefilter <-
  function(data,
           vmax = 50,
           ang = -1,
           distlim = c(2500, 5000),
           spdf = TRUE,
           min.dt = 60,
           emf = NULL
           ) {

  d <- data
  # check input data
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
    mutate(obs.type = ifelse((is.na(smaj) | is.na(smin) | is.na(eor)) & lc != "G", "LS", 
                             ifelse((!is.na(smaj) | !is.na(smin) | !is.na(eor)) & lc != "G", "KF", "GL")))
 
  ##  if any records with smaj/smin = 0 then set to NA and obs.type to "LS"
  ## convert error ellipse smaj & smin from m to km and eor from deg to rad
  d <- d %>%
    mutate(smaj = ifelse(smaj == 0 | smin == 0, NA, smaj),
           smin = ifelse(smin == 0 | is.na(smaj), NA, smin),
           eor = ifelse(is.na(smaj) | is.na(smin), NA, eor),
           obs.type = ifelse(is.na(smaj) & is.na(smin) & obs.type != "GL", "LS", obs.type)) %>%
    mutate(smaj = smaj/1000,
           smin = smin/1000,
           eor = eor/180 * pi) %>%
    mutate(lonerr = lonerr * 6366.71 / 180 * pi,
           laterr = laterr * 6366.71 / 180 * pi) # convert from lon/lat to km (crude)
  
  ## Use argosfilter::sdafilter to identify outlier locations
  if (spdf) {
    if(inherits(d, "sf") && st_is_longlat(d)) {

      xy <- st_coordinates(d) %>%
        as_tibble() %>%
        rename(lon = X, lat = Y)
      d <- bind_cols(d, xy)

    } else if(inherits(d, "sf") && !st_is_longlat(d)) {

      xy <- st_transform(d, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
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
          "using argosfilter::vmask instead"
        ),
        immediate. = TRUE
      )

      tmp <-
        suppressWarnings(try(with(
          subset(d, keep),
          vmask(
            lat,
            lon,
            date,
            vmax = vmax
          )
        ),
        silent = TRUE)
        )

      if (!inherits(tmp, "try-error"))
      {
        filt[d$keep] <- tmp
        d <- d %>%
          mutate(keep = ifelse(filt == "removed", FALSE, keep))

      } else if (inherits(tmp, "try-error")) {

        warning(
          paste(
            "\nargosfilter::vmask also produced an error on id",
            d$id[1],
            "can not apply speed filter prior to SSM filtering"
          ),
          immediate. = TRUE
        )
      }
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
    sf_locs <- st_as_sf(d, coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

    if (any(diff(wrap_lon(dd$lon, 0)) > 300)) {
      prj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"
    } else if (any(diff(wrap_lon(dd$lon,-180)) < -300) ||
               any(diff(wrap_lon(dd$lon,-180)) > 300)) {
      prj <- "+proj=merc +lon_0=180 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"
    } else {
      prj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"
    }

    sf_locs <- sf_locs %>% st_transform(., prj)

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
      emf.x = ifelse(obs.type %in% c("KF","GL"), NA, emf.x),
      emf.y = ifelse(obs.type %in% c("KF","GL"), NA, emf.y)
    ) %>%
    select(everything(), geometry)

  if (sum(is.na(out$lc)) > 0)
    stop(
      "\n NA's found in location class values"
    )
  
  return(out)

}
