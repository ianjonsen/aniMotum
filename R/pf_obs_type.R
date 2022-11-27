##' @title Determine observation type: LS, KF, GPS, or GLS
##'
##' @param x data from `pf_dup_dates()`
##' @keywords internal
##' @md

pf_obs_type <- function(x) {
  
  ## determine observation type: LS, KF, GPS or GLS
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
  ##  convert error ellipse smaj & smin from m to km and eor from deg to rad
  x$smaj <- with(x, ifelse(smaj == 0 | smin == 0, NA, smaj)) / 1000
  x$smin <- with(x, ifelse(smin == 0 | is.na(smaj), NA, smin)) / 1000
  x$eor <- with(x, ifelse(is.na(smaj) & is.na(smin), NA, eor)) / 180 * pi
  
  x$obs.type <- with(x, ifelse(is.na(smaj) & is.na(smin) & is.na(eor) & 
                                 (obs.type != "GLS" & obs.type != "GPS"), 
                               "LS", obs.type))
  
  ## convert GLS errors from degrees lon/lat to km
  x$lonerr <- with(x, lonerr * 6378.137 / 180 * pi)  
  x$laterr <- with(x, laterr * 6378.137 / 180 * pi)
  
  return(x)
}