##' @title Fit a hidden Markov model to SSM-predicted tracks, by multiple imputation
##'
##' @description Fits an \code{n_states}-state hidden Markov model (HMM) to
##' step lengths and turning angles of the regular-interval locations predicted
##' by an SSM fit. Location uncertainty is carried into the HMM by multiple
##' imputation: \code{reps} tracks are simulated from the SSM fit's joint
##' posterior with [sim_post()], an HMM is fitted to each with
##' \code{hmmTMB}, and the results are pooled across imputations.
##'
##' @param x an `ssm_df` fit object from a `crw` or `rw` model, with regular
##' predicted locations (`time.step` a positive number). The time step should
##' be long enough that movement between predicted locations is large relative
##' to location error (e.g. 6-24 h for Kinéis or Argos data).
##' @param reps number of imputed tracks (default 20).
##' @param n_states number of behavioural states (default 2).
##' @param step_par optional list of starting values for the step-length
##' distribution (`mean`, `sd`, each of length `n_states`, km). By default,
##' quantiles of the step lengths of the SSM-predicted track.
##' @param angle_par optional list of starting values for the turning-angle
##' distribution (`mu`, `kappa`, each of length `n_states`). By default
##' `mu = 0` and `kappa` increasing from 0.5 to 5 across states.
##' @param silent logical; suppress hmmTMB fitting output (default TRUE).
##'
##' @return a list with class `hmm_mi`:
##' 1. `states`: a tibble of the SSM-predicted locations (`id`, `date`, `lon`,
##' `lat`), the step length (km) and turning angle (radians; positive for a
##' left, counter-clockwise turn) of the predicted track, the
##' probability of each state averaged across imputations (`p1`, `p2`, ...),
##' the most probable state (`state`), and the proportion of imputations whose
##' Viterbi state equals `state` (`agreement`);
##' 2. `par`: a tibble of the estimated parameters for each imputation:
##' step-length mean and SD (km), turning-angle mean and concentration, and the
##' transition probability matrix (`tpm_ij`, from state i to state j);
##' 3. `par_summary`: the mean and 2.5\% and 97.5\% quantiles of each parameter
##' across imputations;
##' 4. `reps`, `n_states`, `n_failed`: the number of imputations, states and
##' imputations whose HMM fit failed.
##'
##' @details The HMM has gamma-distributed step lengths, von Mises turning
##' angles, a stationary initial state distribution, and no covariates. All
##' tracks in `x` share one set of HMM parameters.
##'
##' States are ordered by mean step length, so state 1 has the
##' shortest steps (e.g. area-restricted movement) and state `n_states` the
##' longest (e.g. travel), in every imputation. This prevents label switching
##' when results are pooled.
##'
##' The spread of the parameter estimates across imputations reflects location
##' uncertainty only. It does not include the HMM's own estimation
##' uncertainty, so the intervals in `par_summary` are not full confidence
##' intervals. [sim_post()] also does not include uncertainty in the SSM's
##' movement parameters.
##'
##' Requires the `hmmTMB` package.
##'
##' @examples
##' \dontrun{
##' fit <- fit_ssm(ellie, model = "crw", time.step = 12)
##' h <- fit_hmm(fit, reps = 20)
##' h$par_summary
##' h$states
##' }
##'
##' @importFrom dplyr tibble bind_rows group_by summarise
##' @importFrom stats quantile median
##' @export
##' @md
fit_hmm <- function(x,
                    reps = 20,
                    n_states = 2,
                    step_par = NULL,
                    angle_par = NULL,
                    silent = TRUE) {

  if (!requireNamespace("hmmTMB", quietly = TRUE))
    stop("fit_hmm() requires the hmmTMB package: install.packages(\"hmmTMB\")", call. = FALSE)
  if (!inherits(x, "ssm_df"))
    stop("x must be an ssm_df fit object from fit_ssm()", call. = FALSE)
  if (!all(x$pmodel %in% c("crw", "rw")))
    stop("x must be a `crw` or `rw` SSM fit", call. = FALSE)
  if (!all(x$converged))
    stop("all SSM fits must have converged; drop non-converged fits first, e.g. x[x$converged, ]",
         call. = FALSE)
  if (any(vapply(x$ssm, function(s) !is.numeric(s$ts) || is.na(s$ts), logical(1))))
    stop("x must have regular predicted locations: fit with a positive numeric time.step",
         call. = FALSE)
  if (!is.numeric(reps) || length(reps) != 1 || reps < 1)
    stop("reps must be a single number >= 1", call. = FALSE)
  if (!is.numeric(n_states) || length(n_states) != 1 || n_states < 2)
    stop("n_states must be a single number >= 2", call. = FALSE)

  ## SSM-predicted (estimated) track: step lengths, turning angles, and
  ##  starting values for the HMM
  est <- grab(x, "predicted", as_sf = FALSE)
  est <- as.data.frame(est)[, c("id", "date", "lon", "lat")]
  est <- hmm_moves(est)

  if (is.null(step_par)) {
    q <- quantile(est$step, seq(0.25, 0.75, length.out = n_states), na.rm = TRUE, names = FALSE)
    q <- pmax(q, 1e-3)
    step_par <- list(mean = q, sd = q)
  }
  if (is.null(angle_par)) {
    angle_par <- list(mu = rep(0, n_states),
                      kappa = seq(0.5, 5, length.out = n_states))
  }

  ## imputed tracks
  sims <- sim_post(x, what = "predicted", reps = reps, sim_only = TRUE)
  sims <- bind_rows(lapply(seq_len(nrow(sims)), function(i) {
    s <- as.data.frame(sims$psims[[i]])
    s$id <- sims$id[i]
    s
  }))

  fits <- lapply(sort(unique(sims$rep)), function(r) {
    d <- hmm_moves(sims[sims$rep == r, c("id", "date", "lon", "lat")])
    hmm_fit_one(d, n_states, step_par, angle_par, silent)
  })
  ok <- !vapply(fits, is.null, logical(1))
  if (!any(ok)) stop("the HMM fit failed for every imputation", call. = FALSE)
  fits <- fits[ok]

  ## pool state probabilities and Viterbi states across imputations
  probs <- Reduce(`+`, lapply(fits, function(f) f$probs)) / length(fits)
  colnames(probs) <- paste0("p", seq_len(n_states))
  state <- max.col(probs, ties.method = "first")
  viterbi <- do.call(cbind, lapply(fits, function(f) f$viterbi))
  agreement <- rowMeans(viterbi == state)

  states <- tibble(est[, c("id", "date", "lon", "lat", "step", "angle")],
                   as.data.frame(probs),
                   state = state,
                   agreement = agreement)

  par <- bind_rows(lapply(seq_along(fits), function(i) tibble(rep = i, fits[[i]]$par)))
  long <- data.frame(parameter = rep(names(par)[-1], each = nrow(par)),
                     value = unlist(par[, -1], use.names = FALSE))
  par_summary <- summarise(group_by(long, parameter),
                           mean = mean(value),
                           lower = quantile(value, 0.025, names = FALSE),
                           upper = quantile(value, 0.975, names = FALSE))
  par_summary <- par_summary[match(names(par)[-1], par_summary$parameter), ]

  out <- list(states = states,
              par = par,
              par_summary = par_summary,
              reps = reps,
              n_states = n_states,
              n_failed = sum(!ok))
  class(out) <- append("hmm_mi", class(out))
  out
}

## step lengths (km, great circle) and turning angles (radians, -pi to pi;
##  positive = left, counter-clockwise turn) along each track, ordered by date
##  within id
hmm_moves <- function(d) {
  d <- d[order(d$id, d$date), ]
  rad <- pi / 180
  out <- lapply(split(d, d$id), function(s) {
    n <- nrow(s)
    lon <- s$lon * rad
    lat <- s$lat * rad
    step <- c(NA, 2 * 6371.0088 * asin(sqrt(sin(diff(lat) / 2)^2 +
      cos(lat[-n]) * cos(lat[-1]) * sin(diff(lon) / 2)^2)))
    bearing <- c(NA, atan2(sin(diff(lon)) * cos(lat[-1]),
                           cos(lat[-n]) * sin(lat[-1]) - sin(lat[-n]) * cos(lat[-1]) * cos(diff(lon))))
    ## bearings are clockwise from north, so a left turn reduces the bearing
    angle <- c(NA, -diff(bearing))
    angle <- (angle + pi) %% (2 * pi) - pi
    s$step <- step
    s$angle <- angle
    s
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}

## fit one HMM with hmmTMB; states ordered by mean step length. Returns NULL
##  if the fit fails.
hmm_fit_one <- function(d, n_states, step_par, angle_par, silent) {
  data <- data.frame(ID = factor(d$id), time = d$date, step = d$step, angle = d$angle)
  ## gamma step lengths must be positive
  data$step[!is.na(data$step) & data$step <= 0] <- 1e-6
  res <- try({
    ## stationary initial state distribution: no extra parameters, rather
    ##  than one initial distribution per track
    hid <- hmmTMB::MarkovChain$new(data = data, n_states = n_states,
                                   initial_state = "stationary")
    obs <- hmmTMB::Observation$new(data = data,
                                   dists = list(step = "gamma2", angle = "vm"),
                                   n_states = n_states,
                                   par = list(step = step_par, angle = angle_par))
    hmm <- hmmTMB::HMM$new(obs = obs, hid = hid)
    if (silent) {
      utils::capture.output(hmm$fit(silent = TRUE))
    } else {
      hmm$fit(silent = FALSE)
    }
    hmm
  }, silent = TRUE)
  if (inherits(res, "try-error")) return(NULL)
  hmm <- res

  p <- hmm$par()
  obspar <- p$obspar[, , 1]
  tpm <- p$tpm[, , 1]
  step_mean <- obspar["step.mean", ]
  ord <- order(step_mean)

  probs <- hmm$state_probs()[, ord, drop = FALSE]
  viterbi <- match(hmm$viterbi(), ord)
  tpm <- tpm[ord, ord, drop = FALSE]

  par <- c(stats::setNames(obspar["step.mean", ord], paste0("step_mean_", seq_len(n_states))),
           stats::setNames(obspar["step.sd", ord], paste0("step_sd_", seq_len(n_states))),
           stats::setNames(obspar["angle.mu", ord], paste0("angle_mu_", seq_len(n_states))),
           stats::setNames(obspar["angle.kappa", ord], paste0("angle_kappa_", seq_len(n_states))))
  tpm_names <- outer(seq_len(n_states), seq_len(n_states), function(i, j) paste0("tpm_", i, j))
  par <- c(par, stats::setNames(as.vector(tpm), as.vector(tpm_names)))

  list(probs = probs, viterbi = viterbi, par = as.data.frame(as.list(par)))
}
