##' @title Fit a Continuous-time state-space model to filter Argos data
##'
##' @description fits either a simple random walk or a correlated random walk (a random walk on velocity) in continuous time to filter Argos KF and/or LS data
##' and predict locations at user-specified time intervals (regular or irregular)
##'
##' @param d a data frame of observations including Argos KF error ellipse info
##' @param vmax max travel rate (m/s) passed to argosfilter::sdafilter to define outlier locations
##' @param ang angles of outlier location "spikes" - see ?argosfilter::sdafilter for details
##' @param distlim lengths of outlier location "spikes" - see ?argosfilter::sdafilter for details
##' @param min.dt minimum allowable time difference between observations; dt <= min.dt will be ignored by the SSM
##' @param pf just pre-filter the data, do not fit the ctrw (default is FALSE)
##' @param ... arguments passed to sfilter, described below:
##' @param model fit either a simple Random Walk ("rw") or Correlated Random Walk ("crw") as a continuous-time process model
##' @param time.step the regular time interval, in hours, to predict to. Alternatively, a vector of prediction times, possibly not regular, must be specified as a data.frame with id and POSIXt dates.
##' @param parameters a list of initial values for all model parameters and unobserved states, default is to let sfilter specifiy these. Only play with this if you know what you are doing...
##' @param fit.to.subset fit the SSM to the data subset determined by prefilter (default is TRUE)
##' @param optim numerical optimizer to be used ("nlminb" or "optim")
##' @param verbose report progress during minimization
##' @param inner.control list of control settings for the inner optimization (see ?TMB::MakeADFUN for additional details)
##'
##' @return a list with components
##' \item{\code{call}}{the matched call}
##' \item{\code{predicted}}{an sf tbl of predicted location states}
##' \item{\code{fitted}}{an sf tbl of fitted locations}
##' \item{\code{par}}{model parameter summmary}
##' \item{\code{data}}{an augmented sf tbl of the input data}
##' \item{\code{inits}}{a list of initial values}
##' \item{\code{pm}}{the process model fit, either "rw" or "crw"}
##' \item{\code{ts}}{time time.step in h used}
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
                    vmax = 50,
                    ang = -1,
                    distlim = c(2500,5000),
                    spdf = TRUE,
                    min.dt = 60,
                    pf = FALSE,
                    ...
                    )
{


  if(!is.numeric(vmax)) stop("\nvmax must be a numeric value in m/s")
  if(!is.numeric(ang)) stop("\nang must be a numeric value in degrees, or -1 to ignore")
  if(!is.numeric(distlim)) stop("\ndistlim must be two numeric values in m")
  if(!is.numeric(min.dt)) stop("\nmin.dt must be a numeric value in s")

  cat("prefiltering data...\n")
  fit <- d %>%
    group_by(id) %>%
    do(pf = prefilter(., vmax = vmax, ang = ang, distlim = distlim, spdf = spdf, min.dt = min.dt))

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
      mutate(converged = sapply(.$ssm, function(x)
        x$opt$convergence == 0)) %>%
      select(id, ssm, converged)
  }
  return(fit)
}
