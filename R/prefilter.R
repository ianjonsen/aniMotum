##' @title Prepare Argos data for fitting state-space model
##'
##' @description \code{prefilter} does the following:
##' (1) flags (`keep = FALSE`) observations with duplicate
##'  dates & removes subsequent observations occurring within min.dt s of one another 
##' (2) determines Argos data type (LS, KF, G, or GL);
##' (3) uses `sda` filter to identify extreme locations. `sda` is a 
##' fast, vectorized version of the now CRAN-archived `argosfilter::sdafilter`. 
##' `sda` is a native implementation of that found in the currently archived `trip`
##' package (by MD Sumner: https://github.com/Trackage/trip).
##' (4) projects lonlat coords to mercator x,y coords (in km) & shifts longitudes
##' that straddle -180,180 to 0,360 and vice-versa;
##' (5) adds location error multiplication factors based on Argos location
##' class (for type LS);
##'
##' @details called by \code{fit_ssm}.
##'
##' @param x input data, must have 5 (LS), or 8 (KF) columns (see details)
##' @param vmax max travel rate (m/s)
##' @param ang angles of outlier location "spikes" (default is \code{c(15,25)} deg);
##' \code{ang = NA} turns off \code{sda} filter in favour of 
##' \code{speedfilter}
##' @param distlim lengths of outlier location "spikes" in km (default is 
##' \code{c(2.5, 5)} m); \code{distlim = NA} turns off \code{sda} filter 
##' in favour of \code{speedfilter}. Either \code{ang = NA} or 
##' \code{distlim = NA} are sufficient.
##' @param spdf turn speed filter on/off (logical; default is TRUE)
##' @param min.dt minimum allowable time difference in s between observations; 
##' \code{dt < min.dt} will be ignored by the SSM
##' @param emf optionally supplied data.frame of error multiplication factors 
##' for Argos location quality classes. see Details
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

    ##  1. flag duplicate date (delta(date) < min.dt) records
    x <- pf_dup_dates(x, min.dt)
    
    ##  2. determine observation type: LS, KF, GPS or GLS
    x <- pf_obs_type(x)

    ##  3. identify extreme locations with a speed/distance/angle filter
    x <- pf_sda_filter(x, spdf, vmax, ang, distlim)
  
    ##  4. project from longlat to merc or respect user-supplied projection & 
    ##       ensure that longitudes straddling -180,180 or 0,360 are shifted 
    ##       appropriately
    x <- pf_sf_project(x)

    ##  5. add location error multiplication factors and finalise data structure
    ##      for use by sfilter()
    x <- pf_add_emf(x, emf)
    
    return(x)
}
