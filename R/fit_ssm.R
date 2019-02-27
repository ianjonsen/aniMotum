##' @title Fit a Continuous-time state-space model to filter Argos data
##'
##' @description fits either a simple random walk or a correlated random walk (a random walk on velocity) in continuous time to filter Argos KF and/or LS data
##' and predict locations at user-specified time intervals (regular or irregular)
##'
##' @param d a data frame of observations including Argos KF error ellipse info
##' @param vmax max travel rate (m/s) passed to argosfilter::sdafilter to define outlier locations
##' @param min.dt minimum allowable time difference between observations; dt <= min.dt will be ignored by the SSM
##' @param project specify projection for observations and estimated locations, if unspecified (\code{project = NULL}; default) an educated guess is made that may not be optimal. Current options are: \code{merc} and \code{polar}, which transform coordinates from geographic space to a global Mercator projection (epsg 3395) or to a polar stereographic projection (epsg 3031 for S hemisphere, epsg 3995 for N) centred on the mean observed longitude, respectively. Currently, the specified option will be applied to all tracks in a multi-individual fit, whereas educated guesses are made for each individual separately.
##' @param pf just pre-filter the data, do not fit the ctrw (default is FALSE)
##' @param ... arguments passed to sfilter, described below:
##' @param model fit either a simple Random Walk ("rw") or Correlated Random Walk ("crw") as a continuous-time process model
##' @param time.step the regular time interval, in hours, to predict to. Alternatively, a vector of prediction times, possibly not regular, must be specified as a data.frame with id and POSIXt dates.
##' @param fit.to.subset fit the SSM to the data subset determined by prefilter (default is TRUE)
##' @param optim numerical optimizer to be used ("nlminb" or "optim")
##' @param verbose report progress during minimization
##'
##' @return a list with components
##' \item{\code{call}}{the matched call}
##' \item{\code{predicted}}{a data.frame of predicted location states}
##' \item{\code{fitted}}{a data.frame of fitted locations}
##' \item{\code{par}}{model parameter summmary}
##' \item{\code{data}}{the input data.frame}
##' \item{\code{subset}}{the input subset vector}
##' \item{\code{mem}}{the measurement error model used}
##' \item{\code{opt}}{the object returned by the optimizer}
##' \item{\code{tmb}}{the TMB object}
##' \item{\code{rep}}{TMB sdreport}
##' \item{\code{aic}}{the calculated Akaike Information Criterion}
##' \item{\code{time}}{the processing time for sfilter}
##'
##' @examples
##' \dontrun{
##' require(dplyr)
##' ## fit RW model to Argos data with KF error ellipse data
##' ## use a 6-h time.step
##' data(ellie)
##' fkf <- fit_ssm(ellie, time.step = 6)
##'
##' ## summary plot of fitted locations as mercator x,y
##' plot(fkf$ssm[[1]], proj="xy")
##'
##' ## summary plot of fitted locations as longlat
##' plot(fkf$ssm[[1]])
##'
##' ## fit CRW model to multiple individuals with Argos LS data
##' data(rope)
##' fls <- fit_ssm(rope, model = "crw", time.step = 3)
##'
##' ## summary plot of fitted longlat locations for individual 3
##' plot(fls$ssm[[3]])
##'
##' ## summary plot of predicted longlat locations for individual 3
##' plot(fls$ssm[[3]], est="predicted")
##' }
##' @importFrom dplyr group_by do rowwise %>% ungroup select mutate slice
##' @importFrom tibble as_tibble
##'
##' @export
fit_ssm <- function(d,
                    vmax = 10,
                    min.dt = 60,
                    project = NULL,
                    pf = FALSE,
                    ...
                    )
{

  ##FIXME: add a 3rd option 'user' to allow user to supply data with their own
  ##        projection as an sf data.frame, so long as the units are in km.
  ##        This option turns off all sf-enabled code during the pre-filtering
  ##        stage

  if(!is.null(project)) {
    if(!project %in% c("merc", "polar"))
      stop("\nprojection strings must be one of 'merc' or 'polar'")
  }

  cat("prefiltering data...\n")
  fit <- d %>%
    group_by(id) %>%
    do(pf = prefilter(., vmax = vmax, min.dt = min.dt, project = project))

  if(pf){
    pfd <- lapply(fit$pf, function(.) .$data)
    fit <- do.call(rbind, pfd) %>%
      as_tibble()
  } else {
    cat("\nfitting SSM...\n")
    fit <- fit %>%
      do(ssm = try(sfilter(.$pf, ...), silent = TRUE))

    fail <- which(sapply(fit$ssm, length) == 6 || sapply(fit$ssm, length) == 1)
    if (length(fail) > 0) {
      cat(sprintf("\n%d optimisation failures\n", length(fail)))
    }

    fit <- fit %>%
      ungroup() %>%
      mutate(id = sapply(.$ssm, function(x)
        x$data$id[1])) %>%
      select(id, ssm)
  }
  fit
}
