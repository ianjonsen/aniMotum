##' @title Fit a continuous-time state-space model to filter Argos satellite geolocation data
##'
##' @description fits either a simple random walk or a correlated random walk
##' (a random walk on velocity) in continuous time to filter Argos LS, and/or KF/KS location data, 
##' processed light-level geolocation data (GLS), and/or GPS data. Location data of different types can
##' combined in a single data frame (see details). Predicts locations at user-specified 
##' time intervals (regular or irregular).
##'
##' @param d a data frame of observations including Argos KF error ellipse info (when present)
##' @param vmax max travel rate (m/s) passed to \code{\link{sda}} to identify
##'  outlier locations
##' @param ang angles (deg) of outlier location "spikes" 
##' @param distlim lengths (m) of outlier location "spikes" 
##' @param spdf (logical) turn \code{trip::sda} on (default; TRUE) or off
##' @param min.dt minimum allowable time difference between observations;
##' \code{dt <= min.dt} will be ignored by the SSM
##' @param pf just pre-filter the data, do not fit the SSM (default is FALSE)
##' @param model fit either a simple random walk ("rw") or correlated random walk
##' ("crw") as a continuous-time process model
##' @param time.step options: 1) the regular time interval, in hours, to predict to; 
##' 2) a vector of prediction times, possibly not regular, must be
##' specified as a data.frame with id and POSIXt dates; 3) NA - turns off prediction and locations are only estimated at observation times. 
##' @param scale scale location data for more efficient optimization. This should rarely be needed (default = FALSE)
##' @param emf optionally supplied data.frame of error multiplication factors for Argos location quality classes. Default behaviour is to use the factors supplied in foieGras::emf()
##' @param map a named list of parameters as factors that are to be fixed during estimation, e.g., \code{list(psi = factor(NA))}
##' @param parameters a list of initial values for all model parameters and
##' unobserved states, default is to let sfilter specify these. Only play with
##' this if you know what you are doing
##' @param fit.to.subset fit the SSM to the data subset determined by \code{prefilter}
##' (default is TRUE)
##' @param control list of control settings for the outer optimizer (see \code{ssm_control} for details)
##' @param inner.control list of control settings for the inner optimizer (see \code{\link{MakeADFun}} for additional details)
##' @param verbose `r lifecycle::badge("deprecated")` use ssm_control(verbose = 1) instead, see \code{ssm_control} for details
##' @param optim `r lifecycle::badge("deprecated")` use ssm_control(optim = "optim") instead, see \code{ssm_control} for details
##' @param optMeth `r lifecycle::badge("deprecated")` use ssm_control(method = "L-BFGS-B") instead, see \code{ssm_control} for details
##' @param lpsi `r lifecycle::badge("deprecated")` use ssm_control(lower = list(lpsi = -Inf)) instead, see \code{ssm_control} for details
##'
##' @details \code{d} is a \code{data.frame}, \code{tibble}, or \code{sf-tibble} with 5, 7 or 8 columns, depending on the tracking data type. 
##' Argos Least-Squares and GPS data should have 5 columns in the following order: "id", "date", "lc", "lon", "lat". Where "date" can be a POSIX
##' object or text string in YYYY-MM-DD HH:MM:SS format. If a text string is supplied then the time zone is assumed to be "GMT". lc (location class)
##' can include the following values: 3, 2, 1, 0, A, B, Z, G, or GL. The latter two are for GPS and GLS locations, respectively. Class Z values are 
##' assumed to have the same error variances as class B. By default, class G (GPS) locations are assumed to have error variances 10x smaller than
##' Argos class 3 variances, but unlike Argos error variances the GPS variances are the same for longitude and latitude. 
##' 
##' See \code{\link{emf}} for details on how to modify these assumptions. 
##' 
##' Argos Kalman Filter (or Kalman Smoother) data should have 8 columns, including the 
##' above 5 plus "smaj", "smin", "eor" that contain Argos error ellipse variables (in m for "smaj", "smin" and deg for "eor"). 
##' 
##' Light-level geolocation (GLS) locations can be modelled provided each longitude and latitude has a corresponding standard error. These data 
##' should have 7 columns, including the above 5 plus "lonerr", "laterr" (in degrees). In this case, all lc values should be set to "GL". 
##' 
##' Multiple location data types can be combined in a single data frame (see the vignette for examples). 
##' 
##' When data are provided as an \code{sf-tibble}, the user-specified projection is respected. Otherwise, longlat data are re-projected internally 
##' to a global Mercator grid and provided as the default output. An un-projected \code{tibble} of lon,lat and x,y location estimates can be 
##' obtained by using \code{\link{grab}} with the argument \code{as_sf = FALSE}.
##' 
##' @return a list with components
##' \item{\code{call}}{the matched call}
##' \item{\code{predicted}}{an sf tbl of predicted location states}
##' \item{\code{fitted}}{an sf tbl of fitted locations}
##' \item{\code{par}}{model parameter summary}
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
##' @references
##' Jonsen ID, Patterson TA, Costa DP, et al. (2020) A continuous-time state-space model for rapid quality-control of Argos locations from animal-borne tags. Movement Ecology 8:31 https://doi.org/10.1186/s40462-020-00217-7
##' 
##' Jonsen ID, McMahon CR, Patterson TA, et al. (2019) Movement responses to environment: fast inference of variation among southern elephant seals with a mixed effects model. Ecology. 100(1):e02566 https://doi.org/10.1002/ecy.2566
##' 
##' @seealso \code{\link{sfilter}}
##' 
##' @examples
##' ## fit crw model to Argos LS data
##' ## se = FALSE to speed up ex
##' fit <- fit_ssm(sese1, vmax = 4, model = "crw", time.step = 48, 
##' control = ssm_control(se = FALSE)) 
##' 
##' ## time series plots of fitted value fit to data
##' plot(fit, what = "fitted", type = 1, ask = FALSE)
##'
##' ## track plots of predicted value fit to data
##' plot(fit, what = "predicted", type = 2, ask = FALSE)
##'
##' @importFrom dplyr tibble mutate "%>%"
##' @importFrom purrr map
##' @importFrom assertthat assert_that
##' @importFrom lifecycle deprecate_stop deprecate_warn
##'
##' @export
fit_ssm <- function(d,
                    vmax = 5,
                    ang = c(15,25),
                    distlim = c(2500,5000),
                    spdf = TRUE,
                    min.dt = 60,
                    pf = FALSE,
                    model = "crw",
                    time.step = NA,
                    scale = FALSE,
                    emf = NULL,
                    map = NULL,
                    parameters = NULL,
                    fit.to.subset = TRUE,
                    control = ssm_control(),
                    inner.control = NULL,
                    verbose = NULL,
                    optim = NULL,
                    optMeth = NULL,
                    lpsi = NULL
                    )
{

## check args - most args handled by prefilter() & sfilter()
  assert_that(is.data.frame(d), 
              msg = "x must be a data.frame, tibble or sf-tibble, see `?fit_ssm for details`")
  assert_that(is.logical(pf), 
              msg = "pf must be either FALSE (fit model) or TRUE (only run prefilter)")

## warnings for deprecated arguments
  if(!is.null(verbose)) {
    deprecate_warn("0.7-5", "fit_ssm(verbose)", 
                                       details = "use `control = ssm_control(verbose)` instead")
    control$verbose <- verbose
  }
  if(!is.null(optim)) {
    deprecate_warn("0.7-5", "fit_ssm(optim)", 
                                     details = "use `control = ssm_control(optim)` instead")
    if(optim %in% c("nlminb", "optim")) control$optim <- optim
    else stop("invalid optimiser specified, see ?ssm_control for options")
  }
  if(!is.null(optMeth)) {
    deprecate_warn("0.7-5", "fit_ssm(optMeth)", 
                                       details = "use `control = ssm_control(method)` instead")
    if(optMeth %in% c("L-BFGS-B", "BFGS", "Nelder-Mead", "CG", "SANN", "Brent"))
      control$method <- optMeth
    else stop("invalid optimisation method specified, see ?ssm_control for options")
  }
  if(!is.null(lpsi)) {
    deprecate_warn("0.7-5", "fit_ssm(lpsi)", 
                                    details = "use `control = ssm_control(lower)` instead")
    control$lower <- list(l_psi = lpsi)
  }
  
  fit <- d %>%
    split(., .$id) %>%
    map(~ prefilter( 
        data = .x,
        vmax = vmax,
        ang = ang,
        distlim = distlim,
        spdf = spdf,
        min.dt = min.dt,
        emf = emf))

  if(pf){
    fit <- try(fit %>% do.call(rbind, .))
    
    if(inherits(fit, "try-error")) 
    stop("\n Cannot bind tibbles with multiple guessed projections in pre-filtered output. \n
            Supply data as an `sf` object with a common projection across individuals.\n")
    
  } else {
    if(control$verbose == 1)
      cat(paste0("fitting ", model, "...\n"))
    
    fit <- fit %>%
      map(~ sfilter(
        x = .x,
        model = model,
        time.step = time.step,
        parameters = parameters,
        map = map,
        fit.to.subset = fit.to.subset,
        control = control,
        inner.control = inner.control
      )
      )

    fit <- tibble(id = names(fit), ssm = fit) %>%
      mutate(converged = sapply(.$ssm, function(x) 
        if(length(x) == 15) {
          x$opt$convergence == 0
        } else if(length(x) < 15) {
          FALSE
        })) %>%
      mutate(pdHess = sapply(.$ssm, function(x) 
        length(x) == 15
      )) %>%
      mutate(pmodel = sapply(.$ssm, function(x) x$pm))
  }

  class(fit) <- append("fG_ssm", class(fit))
  return(fit)
}
