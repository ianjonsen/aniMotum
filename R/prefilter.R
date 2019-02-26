##' @title Prepare Argos data for fitting a ct ssm
##'
##' @description \code{prefilter()} (1) determines Argos data type (LS or KF); (2) converts dates to POSIXt;
##' identifies observations with duplicate dates; (3) orders observations in time;
##' (4) removes duplicate observations; (5) removes observations occurring within 60 s of one another (keeps first);
##' (6) shifts longitudes that straddle -180,180 to 0,360 and vice-versa; (7) projects lonlat coords to mercator x,y
##' coords (in km); (8) adds location error multiplication factors based on Argos location class (for type LS);
##' and (9) uses a argosfilter::sdafilter to identify potential outlier locations (by distance only) to be ignored when fitting
##' the \code{ctrw} model
##'
##' @details Internal function
##'
##' @param d input data - must have 5 (LS), or 8 (KF) columns (see details)
##' @param vmax max travel rate (m/s) passed to argosfilter::sdafilter to define outlier locations
##' @param min.dt minimum allowable time difference between observations; dt < min.dt will be ignored by the SSM
##' @importFrom lubridate ymd_hms
##' @importFrom stats loess
##' @importFrom dplyr mutate distinct arrange filter select %>% left_join lag
##' @importFrom sf st_as_sf st_set_crs st_transform
##' @importFrom argosfilter sdafilter
##'
##' @export

prefilter <- function(d, vmax = 10, min.dt = 1) {

  # check input data
  if(!ncol(d) %in% c(5,8)) stop("Data can only have 5 (for LS data) or 8 (for KF data) columns")

  if((ncol(d) == 5 &
      !isTRUE(all.equal(names(d), c("id", "date", "lc", "lon", "lat")))) ||
     (ncol(d) == 8 &
      !isTRUE(all.equal(
        names(d),
        c("id", "date", "lc", "lon", "lat", "smaj", "smin", "eor")
      )))) stop("Unexpected column names in Data, type `?prefilter` for details on data format")

  if(length(unique(d$id)) > 1) stop("Multiple individual tracks in Data, use fit_ssm")

  if(!is.null(d$id)) d <- d %>% mutate(id = as.character(id))

  ## add KF error columns, if missing
  if(ncol(d) == 5) {
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
  filt <- rep("not", nrow(d))
  tmp <- suppressWarnings(try(with(subset(d, keep), sdafilter(lat, lon, date, lc, ang=-1, vmax=vmax)), silent = TRUE))
  ## screen potential sdafilter errors
  if(!inherits(tmp, "try-error")) {
    filt[d$keep] <- tmp
    d <- d %>%
      mutate(keep = ifelse(filt == "removed", FALSE, keep))
  } else if(inherits(tmp, "try-error")) {
    warning(paste("\nargosfilter::sdafilter produced an error on id", d$id[1], "unable to apply speed filter"), immediate. = TRUE)
  }

  ##  if lon spans -180,180 then shift to
  ##    0,360; else if lon spans 360,0 then shift to
  ##    -180,180 ... have to do this on keep subset only
  dd <- subset(d, keep)
  dd$project <- "polar_s"

  switch(dd$project[1],
  polar_s = {
    sf_locs <- st_as_sf(dd, coords=c("lon","lat")) %>%
      st_set_crs("+proj=longlat +ellps=WGS84") %>%
      st_transform(., "+init=epsg:3031 +units=km")
  },
  polar_n = {
    sf_locs <- st_as_sf(dd, coords=c("lon","lat")) %>%
      st_set_crs("+proj=longlat +ellps=WGS84") %>%
      st_transform(., "+init=epsg:3995 +units=km")
  },
  lon180 = {
    sf_locs <- st_as_sf(dd, coords=c("lon","lat")) %>%
      st_set_crs("+proj=longlat +ellps=WGS84") %>%
      st_transform(., "+init=epsg:4326 +lon_0=180 +units=km")
  },
  lon_360 = {
    sf_locs <- st_as_sf(dd, coords=c("lon","lat")) %>%
      st_set_crs("+proj=longlat +ellps=WGS84") %>%
      st_transform(., "+init=epsg:4326 +lon_0=0 +units=km")
  }
  )
  prj <- NULL

  # if (max(abs(diff(dd$lon))) > 300) {
  #   if (max(abs(wrap_lon(dd$lon, 0) - wrap_lon(dd$lon,-180))) < 0.00001) {
  #     prj <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
  #   } else if (max(abs(wrap_lon(dd$lon, 0) - wrap_lon(dd$lon, -180))) == 360){
  #     prj <- "+proj=merc +lon_0=180 +datum=WGS84 +units=km +no_defs"
  #   }
  # } else {
  #   prj <- "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
  # }
  #
  # d[, c("x", "y")] <- as_tibble(project(as.matrix(d[, c("lon", "lat")]), proj = prj))

  ## add LS error info to corresponding records
  ## set amf's = NA if obs.type == "KF" - not essential but for clarity
  tmp <- amf()
    out <- sf_locs %>%
      left_join(., tmp, by = "lc") %>%
      mutate(amf_x = ifelse(obs.type == "KF", NA, amf_x),
             amf_y = ifelse(obs.type == "KF", NA, amf_y))

    if(sum(is.na(out$lc)) > 0) stop("\n NA's found in location class values,\n
                                  perhaps your input lc's != c(3,2,1,0,`A`,`B`,`Z`)?")

  #d <- d %>% select(id, date, lc, lon, lat, smaj, smin, eor, x, y, amf_x, amf_y, obs.type, keep)
  return(out)

}
