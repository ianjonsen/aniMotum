##' @title Fit a continuous-time state-space model to filter Argos satellite geolocation data
##'
##' @description fits either a simple random walk or a correlated random walk
##' (a random walk on velocity) in continuous time to filter Argos KF and/or LS data
##' and predict locations at user-specified time intervals (regular or irregular)
##'
##' @param d a data frame of observations including Argos KF error ellipse info
##' @param vmax max travel rate (m/s) passed to argosfilter::sdafilter to define
##'  outlier locations
##' @param ang angles of outlier location "spikes" - see ?argosfilter::sdafilter
##'  for details
##' @param distlim lengths of outlier location "spikes" - see
##' ?argosfilter::sdafilter for details
##' @param spdf (logical) turn argosfilter::sdafilter on (default; TRUE) or off
##' @param min.dt minimum allowable time difference between observations;
##' dt <= min.dt will be ignored by the SSM
##' @param pf just pre-filter the data, do not fit the SSM (default is FALSE)
##' @param model fit either a simple random walk ("rw") or correlated random walk
##' ("crw") as a continuous-time process model
##' @param time.step the regular time interval, in hours, to predict to.
##' Alternatively, a vector of prediction times, possibly not regular, can be
##' specified as a data.frame with id and POSIXt dates.
##' @param emf optionally suppled vector of matrix of error multiplication factors for location quality classes. see Details in \code{?prefilter}
##' @param map an optional named list of parameters (as factors) to be fixed in the estimation (see ?TMB::MakeADFun for further details)
##' @param parameters a list of initial values for all model parameters and
##' unobserved states, default is to let sfilter specifiy these. Only play with
##' this if you know what you are doing...
##' @param fit.to.subset fit the SSM to the data subset determined by prefilter
##' (default is TRUE)
##' @param optim numerical optimizer to be used ("nlminb" or "optim"); if optim is chosen, the L-BFGS-B method is used, currently without box constraints
##' @param verbose report progress during minimization; 0 for silence; 1 for progress bar only; 2 for minimizer trace and no progress bar
##' @param control list of control parameters for the outer optimization (type ?nlminb or ?optim for details)
##' @param inner.control list of control parameters for the inner optimization
##' (see ?TMB::MakeADFUN for additional details)
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
##' ## fit rw model to a single individual with Argos KF data
##' data(ellie)
##' fit <- fit_ssm(ellie, model = "rw", time.step = 24)
##' plot(fit$ssm[[1]])
##'
##' ## fit crw model to multiple individuals with Argos LS data
##' \donttest{
##' data(rope)
##' fls <- fit_ssm(rope, model = "crw", time.step = 12)
##'
##' ## simple diagnostic plot for individual 3,
##' ## showing predicted value time-series
##' plot(fls$ssm[[3]], what = "predicted")
##'}
##'
##' @importFrom dplyr group_by do rowwise ungroup select mutate slice "%>%"
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
                    model = "rw",
                    time.step = 6,
                    emf = NULL,
                    map = NULL,
                    parameters = NULL,
                    fit.to.subset = TRUE,
                    optim = "optim",
                    verbose = 1,
                    control = NULL,
                    inner.control = NULL
                    )
{

  param <- c(
    "sigma", "rho_p", "D", "psi", "tau", "rho_o"
    )

  if(!is.numeric(vmax)) stop("\nvmax must be a numeric value in m/s")
  if(!is.numeric(ang)) stop("\nang must be a numeric value in degrees, or -1 to ignore")
  if(!is.numeric(distlim)) stop("\ndistlim must be two numeric values in m")
  if(!is.numeric(min.dt)) stop("\nmin.dt must be a numeric value in s")
  if(!is.null(map) & (!inherits(map, "list") | is.null(names(map)))) {
    stop("\nmap must be a named list with names corresponding to model parameters")
  } else if(!is.null(map) && !names(map) %in% param) {
    stop("\nparameter names in map do not match valid parameter names: \n  sigma, rho_p, D, psi, tau, rho_o")
  }

  ## deal with cases where data supplied for prediction times
  if(length(time.step) > 1 & !is.data.frame(time.step)) {
    stop("\ntime.step must be a data.frame with id's when specifying multiple prediction times")
  } else if(length(time.step) > 1 & is.data.frame(time.step)) {
    if(sum(!names(time.step) %in% c("id","date")) > 0) stop("\n time.step names must be `id` and `date`")
  }


  if(verbose %in% c(0,2)) options(dplyr.show_progress = FALSE)
  if(verbose == 1)
    cat("\nprefiltering data...\n")
  fit <- d %>%
    group_by(id) %>%
    do(pf = prefilter(
      .,
      vmax = vmax,
      ang = ang,
      distlim = distlim,
      spdf = spdf,
      min.dt = min.dt,
      emf = emf
    ))

  if(pf){
    pfd <- lapply(fit$pf, function(.) .)
    fit <- try(do.call(rbind, pfd))
    if(inherits(fit, "try-error")) stop("\n Cannot rbind multiple guessed projections in pre-filtered output. \n
                                        Supply data as an `sf` object with a common projection across individuals.\n")
  } else {
    if(verbose == 1)
      cat("\nfitting SSM...\n")
    if (verbose %in% 0:1)
      verb <-  FALSE
    else
      verb <- TRUE

    fit <- fit %>%
      do(ssm = try(sfilter(
        .$pf,
        model = model,
        time.step = time.step,
        map = map,
        parameters = parameters,
        fit.to.subset = fit.to.subset,
        optim = optim,
        verbose = verb,
        control = control,
        inner.control = inner.control
      ),
      silent = TRUE)
      )

    fit <- fit %>%
      ungroup(.) %>%
      mutate(id = sapply(.$ssm, function(x)
        x$data$id[1])) %>%
      mutate(converged = sapply(.$ssm, function(x)
        if(length(x) == 14) {
        x$opt$convergence == 0
          } else if(length(x) < 14) {
            FALSE
          })) %>%
      select(., id, ssm, converged)
  }
  class(fit) <- append("tbl_fG", class(fit))
  return(fit)
}
