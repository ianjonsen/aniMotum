##' @title Fit a behavioural model to animal tracks
##'
##' @description One interface to the behavioural models in aniMotum, chosen
##' by `model`:
##' 1. `"mpm"`: move persistence model, a random walk with time-varying move
##' persistence g (see [fit_mpm()]);
##' 2. `"mrm"`: movement rate model, a Brownian random walk with a
##' time-varying rate, reported as a speed (km/h);
##' 3. `"hmm"`: hidden Markov model with gamma step lengths and von Mises
##' turning angles, fitted with the `hmmTMB` package.
##'
##' With `reps`, each model is fitted to `reps` tracks imputed with
##' [impute_tracks()], from an SSM fit or directly from Kinéis locations and
##' their error radii, and the results are pooled across imputations (Rubin's
##' rules), so they carry location uncertainty.
##'
##' @param x an `ssm_df` fit object from [fit_ssm()] (`rw`, `crw` or `mp`,
##' with regular predicted locations), or a data frame of Kinéis locations
##' (see [impute_tracks()]). For `model = "mpm"` without `reps`, also a data
##' frame of locations treated as data (see [fit_mpm()]).
##' @param model the behavioural model: `"mpm"`, `"mrm"` or `"hmm"`
##' (see description).
##' @param reps number of imputed tracks (default 20). NULL fits the model
##' once to the point-estimate track, treating the locations as data; only
##' `model = "mpm"` allows this.
##' @param time.step for Kinéis locations, the time step (h) of the regular
##' grid (required; see [impute_tracks()]).
##' @param max_gap for Kinéis locations, the longest gap (h) interpolated
##' across (see [impute_tracks()]).
##' @param vmax for Kinéis locations, the maximum travel rate (m/s) of a
##' speed-distance-angle filter applied before imputation, to drop extreme
##' locations (see [impute_tracks()]). NULL (default): no filter.
##' @param crs the projection (proj4 string, units km) of the imputed tracks.
##' If NULL (default), chosen from the extent of the data by [auto_crs()]
##' (see [impute_tracks()]).
##' @param kineis for Kinéis locations, the Kinéis error model (see
##' [impute_tracks()]).
##' @param control a list from [mpm_control()]: optimiser settings for `"mpm"`
##' and `"mrm"`, and, for every model, `verbose` (0 = silent; 1, default =
##' progress bar).
##' @param ... arguments for the chosen model only:
##' 1. `"mpm"`: `joint` (logical, default TRUE: one random-walk SD for g,
##' `sigma_g`, shared by all tracks, the `jmpm`; FALSE: each track fitted
##' separately, the `mpm`), `what`, `coords` and `inner.control` (see
##' [fit_mpm()]);
##' 2. `"mrm"`: none;
##' 3. `"hmm"`: `n_states` (default 2), `step_par`, `angle_par` (starting
##' values) and `silent` (suppress `hmmTMB` output, default TRUE).
##'
##' @return a list with class `behaviour_fit` and a class for the model:
##' 1. `"mpm"`, class `mpm_df`: `fitted` (the track's `lon`, `lat`, g, its
##' standard error and 95% interval), `par`, `convergence`, `tmb` (without
##' `reps`), and the settings (see [fit_mpm()]);
##' 2. `"mrm"`, class `mrm_mi`: `fitted` (the track's `lon`, `lat`, the log
##' movement rate and its standard error, the rate (km h^-1/2), the speed
##' (km/h) at the time step, and their 95% intervals, and the fraction of
##' missing information), `par` (`sigma_rate`, the SD of the random walk on
##' the log rate per h^1/2; `rho_p`), `reps`, `n_failed`, `source` and
##' `time.step`;
##' 3. `"hmm"`, class `hmm_mi`: `states` (the track's `lon`, `lat`, step
##' length and turning angle, state probabilities averaged across
##' imputations, the most probable state and the agreement between
##' imputations), `par` (each imputation's parameters), `par_summary` and
##' `par_working` (pooled estimates on the natural and working scales),
##' `reps`, `n_states`, `n_failed`, `n_pooled`, `source` and `time.step`.
##'
##' `plot()` and, for `"mpm"`, [map()] draw each kind of fit.
##'
##' @details
##' \strong{Move persistence model.} Displacements follow a correlated random
##' walk whose autocorrelation, move persistence g (0 to 1), varies in time
##' as a random walk held within 0 to 1 (see [fit_mpm()]).
##'
##' \strong{Movement rate model.} Displacements between consecutive locations
##' follow a Brownian random walk whose rate varies in time:
##' `d(t) ~ N(0, exp(2 * log_rate(t)) * dt * R)`, with `R` the unit-diagonal
##' correlation matrix of east and north displacements (km, on the ground),
##' and `log_rate` a random walk with SD `sigma_rate * sqrt(dt)`. Low rates
##' indicate area-restricted movement, high rates travel; the rate does not
##' separate speed from tortuosity. `speed` is the root-mean-square net
##' displacement over one time step divided by the time step,
##' `rate * sqrt(2 / time.step)` (km/h): it depends on the time step, so
##' speeds from fits at different time steps are not comparable.
##'
##' \strong{Hidden Markov model.} Step lengths (km, great circle) and turning
##' angles of each imputed track are modelled with gamma and von Mises
##' distributions, a stationary initial state distribution and no covariates;
##' all tracks share one set of parameters, and each track segment is a
##' separate series. States are numbered by mean step length (state 1 has the
##' shortest steps) in every imputation, which prevents label switching.
##' Working-scale estimates are pooled with Rubin's rules.
##'
##' The time step should be long enough that movement between locations is
##' large relative to location error (e.g. 6 to 24 h for Kinéis or Argos
##' data).
##'
##' \strong{Adding a model.} Each model is an entry in
##' `behaviour_models()`: a fitting function taking the shared arguments
##' (`x`, `reps`, `time.step`, `max_gap`, `vmax`, `crs`, `kineis`,
##' `control`) and its own, and returning a list with a class of its own.
##'
##' @examples
##' \dontrun{
##' fit <- fit_ssm(ellie, model = "crw", time.step = 12)
##' g <- fit_bm(fit, model = "mpm", reps = 20)
##' r <- fit_bm(fit, model = "mrm", reps = 20)
##' h <- fit_bm(fit, model = "hmm", reps = 20, n_states = 2)
##' plot(g, type = "track", pages = 1)
##'
##' ## directly from Kinéis locations, without an SSM
##' g <- fit_bm(kineis_locations, model = "mpm", reps = 20,
##'              time.step = 12, max_gap = 48, vmax = 4)
##' }
##'
##' @export
##' @md
fit_bm <- function(x,
                          model = c("mpm", "mrm", "hmm"),
                          reps = 20,
                          time.step = NULL,
                          max_gap = NULL,
                          vmax = NULL,
                          crs = NULL,
                          kineis = ssm_control()$kineis,
                          control = mpm_control(),
                          ...) {
  models <- behaviour_models()
  model <- match.arg(model, names(models))
  m <- models[[model]]
  if (is.null(reps) && !isTRUE(m$point_fit))
    stop("model = \"", model, "\" is fitted by multiple imputation: give reps (e.g. 20)",
         call. = FALSE)

  ## arguments for the chosen model only
  dots <- list(...)
  own <- setdiff(names(formals(m$fit)), behaviour_shared_arguments())
  unknown <- setdiff(names(dots), own)
  if (length(unknown) > 0 || (length(dots) > 0 && is.null(names(dots))))
    stop("argument(s) not used by model = \"", model, "\": ",
         paste(if (length(unknown)) unknown else "(unnamed)", collapse = ", "),
         if (length(own)) paste0(". Its own arguments: ", paste(own, collapse = ", ")) else
           ". It has no arguments of its own",
         call. = FALSE)

  out <- do.call(m$fit, c(list(x = x, reps = reps, time.step = time.step,
                               max_gap = max_gap, vmax = vmax, crs = crs,
                               kineis = kineis, control = control), dots))
  class(out) <- append(class(out), "behaviour_fit", after = 1)
  out
}

## arguments every behavioural model's fitting function takes
behaviour_shared_arguments <- function() {
  c("x", "reps", "time.step", "max_gap", "vmax", "crs", "kineis", "control")
}

## the behavioural models available to fit_bm(). To add a model, add
##  an entry: `fit`, a function of the shared arguments (see
##  behaviour_shared_arguments()) and the model's own, returning a list with a
##  class of its own; `point_fit`, TRUE if the model can be fitted without
##  imputation (reps = NULL); and `description`
behaviour_models <- function() {
  list(
    mpm = list(
      description = "move persistence model",
      point_fit = TRUE,
      fit = function(x, reps, time.step, max_gap, vmax, crs, kineis, control,
                     joint = TRUE, what = "predicted", coords = 3:4, inner.control = NULL) {
        fit_mpm(x, what = what, model = if (isTRUE(joint)) "jmpm" else "mpm",
                coords = coords, control = control, inner.control = inner.control,
                reps = reps, time.step = time.step, max_gap = max_gap,
                kineis = kineis, crs = crs, vmax = vmax)
      }),
    mrm = list(
      description = "movement rate model",
      point_fit = FALSE,
      fit = function(x, reps, time.step, max_gap, vmax, crs, kineis, control) {
        fit_mrm(x, reps = reps, time.step = time.step, max_gap = max_gap,
                kineis = kineis, control = control, vmax = vmax, crs = crs)
      }),
    hmm = list(
      description = "hidden Markov model",
      point_fit = FALSE,
      fit = function(x, reps, time.step, max_gap, vmax, crs, kineis, control,
                     n_states = 2, step_par = NULL, angle_par = NULL, silent = TRUE) {
        fit_hmm(x, reps = reps, n_states = n_states, time.step = time.step,
                max_gap = max_gap, kineis = kineis, step_par = step_par,
                angle_par = angle_par, silent = silent,
                progress = control$verbose >= 1, vmax = vmax, crs = crs)
      })
  )
}
