##' @title Fit a Continuous-time state-space model to filter Argos data
##'
##' @description fits either a simple random walk or a correlated random walk (a random walk on velocity) in continuous time to filter Argos KF or LS data
##' and predict locations at user-specified time intervals (regular or irregular)
##'
##' @param d a data frame of observations including Argos KF error ellipse info
##' @param vmax max travel rate (m/s) passed to argosfilter::sdafilter to define outlier locations
##' @param min.dt minimum allowable time difference between observations; dt <= min.dt will be ignored by the SSM
##' @param time.step the regular time interval, in hours, to predict to. Alternatively, a vector of prediction times, possibly not regular, must be specified as a data.frame with id and POSIXt dates.
##' @param ... arguments passed to sfilter, described below:
##' @param fit.to.subset fit the SSM to the data subset determined by prefilter (default is TRUE)
##' @param psi estimate scaling parameter for the KF measurement error model error ellipses (0 = no psi, default; 1 = single psi for semi-minor axis)
##' @param pf just pre-filter the data, do not fit the ctrw (default is FALSE)
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
##' data(ellie)
##' ## fit KF measurement error model
##' fkf <- fit_ssm(ellie, time.step = 12, psi = 0)
##'
##' ## fit KFp measurement error model
##' fkfp <- fit_ssm(ellie, time.step = 12, psi = 1)
##'
##' ## fit LS measurement error model
##' fls <- fit_ssm(ellie[, 1:5], time.step = 12)
##' }
##' @importFrom dplyr group_by do rowwise %>% ungroup select mutate slice
##' @importFrom tibble as_tibble
##'
##' @export
fit_ssm <- function(d,
                    vmax = 10,
                    min.dt = 60,
                    pf = FALSE,
                    time.step,
                    ...
                    )
{
  if(is.null(time.step)) print("\nNo time.step specified, using 6 h as a default time step")
  else if(length(time.step) > 1 & !is.data.frame(time.step))
    stop("\ntime.step must be a data.frame with id's when specifying multiple prediction times")
  else if(length(time.step) > 1 & is.data.frame(time.step)) {
    if(sum(!names(time.step) %in% c("id","date")) > 0) stop("\n time.step names must be `id` and `date`")
  }

  fit <- d %>%
    group_by(id) %>%
    do(pf = prefilter(., vmax = vmax, min.dt = min.dt))

  if(pf){
    fit <- do.call(rbind, fit$pf) %>%
      as_tibble()
  } else {
    fit <- fit %>%
      rowwise() %>%
      do(ssm = try(sfilter(.$pf, time.step = time.step, ...), silent = TRUE))

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
