##' @title Add error multiplication factors for Argos LS data
##' 
##' @description Adds location error multiplication factors based on Argos location
##' class (for type LS) & finalises prefiltered data for use by `sfilter()`
##'
##' @param x data from `pf_sf_project`
##' @param emf optionally supplied data.frame of error multiplication factors 
##' for Argos location quality classes. Relevant to Argos LS and GPS data only
##' @keywords internal
##' @md

pf_add_emf <- function(x, emf) {
  
  ## add LS error info to corresponding records
  ## set emf's = NA if obs.type %in% c("KF","GL") - not essential but for clarity
  if(is.null(emf)) {
    tmp <- emf()
  } else if(is.data.frame(emf)) {
    tmp <- emf
  } 

  x$lc <- with(x, as.character(lc))
  x <- merge(x, tmp, by = "lc", all.x = TRUE, sort = FALSE)
  
  
  x <- x[order(x$date), c("id","date","lc","smaj","smin","eor",
                                "lonerr","laterr","keep","obs.type",
                                "emf.x","emf.y","geometry")]
  x$emf.x <- with(x, ifelse(obs.type %in% c("KF","GLS"), NA, emf.x))
  x$emf.y <- with(x, ifelse(obs.type %in% c("KF","GLS"), NA, emf.y))
  
  if (sum(is.na(x$lc)) > 0)
    stop(
      "\n NA's found in location class values"
    )
  
  return(x)
}