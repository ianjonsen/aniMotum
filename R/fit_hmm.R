##' @title Fit a hidden Markov model to tracks, by multiple imputation (internal; use fit_bm())
##'
##' @description Fits an \code{n_states}-state hidden Markov model (HMM) to
##' step lengths and turning angles of regular-interval tracks. Location
##' uncertainty is carried into the HMM by multiple imputation: \code{reps}
##' tracks are imputed with [impute_tracks()], from an SSM fit or directly from
##' Kinéis locations, an HMM is fitted to each with \code{hmmTMB}, and the
##' results are pooled across imputations.
##'
##' @param x an `ssm_df` fit object (`rw`, `crw` or `mp`) with regular
##' predicted locations, or a data frame of Kinéis locations (see
##' [impute_tracks()]). The time step should be long enough that movement
##' between locations is large relative to location error (e.g. 6-24 h for
##' Kinéis or Argos data).
##' @param reps number of imputed tracks (default 20).
##' @param time.step for Kinéis locations, the time step (h) of the regular grid
##' (see [impute_tracks()]).
##' @param max_gap for Kinéis locations, the longest gap (h) interpolated
##' across (see [impute_tracks()]).
##' @param kineis for Kinéis locations, the Kinéis error model (see
##' [impute_tracks()]).
##' @param crs the projection of the imputed tracks (see [impute_tracks()]).
##' @param vmax for Kinéis locations, the maximum travel rate (m/s) of a
##' speed-distance-angle filter applied before imputation, to drop extreme
##' locations (see [impute_tracks()]). NULL (default): no filter
##' @param n_states number of behavioural states (default 2).
##' @param step_par optional list of starting values for the step-length
##' distribution (`mean`, `sd`, each of length `n_states`, km). By default,
##' quantiles of the step lengths of the SSM-predicted track.
##' @param angle_par optional list of starting values for the turning-angle
##' distribution (`mu`, `kappa`, each of length `n_states`). By default
##' `mu = 0` and `kappa` increasing from 0.5 to 5 across states.
##' @param silent logical; suppress hmmTMB fitting output (default TRUE).
##' @param progress logical; show a progress bar over imputations (default
##' TRUE).
##'
##' @return a list with class `hmm_mi`:
##' 1. `states`: a tibble of the point-estimate track (`id`, `segment`,
##' `date`, `lon`, `lat`: the SSM-predicted locations, or the Kinéis locations
##' interpolated without perturbation), the step length (km) and turning angle
##' (radians; positive for a
##' left, counter-clockwise turn) of the predicted track, the
##' probability of each state averaged across imputations (`p1`, `p2`, ...),
##' the most probable state (`state`), and the proportion of imputations whose
##' Viterbi state equals `state` (`agreement`);
##' 2. `par`: a tibble of the estimated parameters for each imputation:
##' step-length mean and SD (km), turning-angle mean and concentration, and the
##' transition probability matrix (`tpm_ij`, from state i to state j);
##' 3. `par_summary`: the pooled estimate of each parameter and its 95\%
##' interval, from Rubin's rules on the working (link) scale, back-transformed
##' with hmmTMB's inverse links; `fraction_missing` is the share of the
##' working-scale variance due to location uncertainty. Transition
##' probabilities have intervals for 2 states only (with more states, each
##' probability depends on several working parameters; see `par_working`);
##' 4. `par_working`: the pooled working-scale coefficients (observation
##' parameters, and transition parameters `Si>Sj` = log(p_ij / p_ii)), with
##' their total standard errors, degrees of freedom and 95\% intervals;
##' 5. `reps`, `n_states`, `n_failed`, `n_pooled`: the number of imputations,
##' states, imputations whose HMM fit failed, and imputations pooled for the
##' parameters (those with finite working-scale standard errors); `source`
##' (`"ssm"` or `"kineis"`) and `time.step`.
##'
##' @details The HMM has gamma-distributed step lengths, von Mises turning
##' angles, a stationary initial state distribution, and no covariates. All
##' tracks in `x` share one set of HMM parameters. Each track segment (see
##' [impute_tracks()]) is a separate series, so no step spans a data gap.
##'
##' States are ordered by mean step length, so state 1 has the
##' shortest steps (e.g. area-restricted movement) and state `n_states` the
##' longest (e.g. travel), in every imputation. This prevents label switching
##' when results are pooled.
##'
##' Parameters are pooled with Rubin's rules: for each working-scale
##' coefficient, the total variance is the mean within-imputation variance
##' (from hmmTMB's `sdreport`) plus (1 + 1/m) times the between-imputation
##' variance, with Rubin's degrees of freedom. Imputations whose working-scale
##' standard errors are not all finite (Hessian not positive-definite) are
##' excluded from parameter pooling but contribute state probabilities.
##'
##' Requires the `hmmTMB` package.
##'
##' @examples
##' \dontrun{
##' fit <- fit_ssm(ellie, model = "crw", time.step = 12)
##' h <- fit_bm(fit, model = "hmm", reps = 20)
##' h$par_summary
##' h$states
##'
##' ## directly from Kinéis locations, without an SSM
##' h <- fit_bm(kineis_locations, model = "hmm", time.step = 12)
##' }
##'
##' @importFrom dplyr tibble bind_rows group_by summarise
##' @importFrom stats quantile median
##' @keywords internal
##' @md
fit_hmm <- function(x,
                    reps = 20,
                    n_states = 2,
                    time.step = NULL,
                    max_gap = NULL,
                    kineis = ssm_control()$kineis,
                    step_par = NULL,
                    angle_par = NULL,
                    silent = TRUE,
                    progress = TRUE,
                    vmax = NULL,
                    crs = NULL) {

  if (!requireNamespace("hmmTMB", quietly = TRUE))
    stop("fit_hmm() requires the hmmTMB package: install.packages(\"hmmTMB\")", call. = FALSE)
  if (!is.numeric(n_states) || length(n_states) != 1 || n_states < 2)
    stop("n_states must be a single number >= 2", call. = FALSE)

  imp <- impute_tracks(x, reps = reps, time.step = time.step, max_gap = max_gap,
                       kineis = kineis, vmax = vmax, crs = crs)

  ## segments need >= 3 locations (one turning angle) to enter the HMM
  seg <- paste(imp$id, imp$segment)
  n.seg <- table(seg[imp$rep == 0])
  short <- names(n.seg)[n.seg < 3]
  if (length(short) > 0) {
    message(length(short), " track segment(s) with fewer than 3 locations dropped")
    imp <- imp[!seg %in% short, ]
  }

  ## point-estimate track: step lengths, turning angles, and starting values
  ##  for the HMM
  est <- hmm_moves(as.data.frame(imp[imp$rep == 0, c("id", "segment", "date", "lon", "lat")]))

  if (is.null(step_par)) {
    q <- quantile(est$step, seq(0.25, 0.75, length.out = n_states), na.rm = TRUE, names = FALSE)
    q <- pmax(q, 1e-3)
    step_par <- list(mean = q, sd = q)
  }
  if (is.null(angle_par)) {
    angle_par <- list(mu = rep(0, n_states),
                      kappa = seq(0.5, 5, length.out = n_states))
  }

  sims <- as.data.frame(imp[imp$rep > 0, ])
  pb <- progress_bar(reps, progress)
  fits <- lapply(sort(unique(sims$rep)), function(r) {
    d <- hmm_moves(sims[sims$rep == r, c("id", "segment", "date", "lon", "lat")])
    f <- hmm_fit_one(d, n_states, step_par, angle_par, silent)
    pb$tick()
    f
  })
  pb$close()
  ok <- !vapply(fits, is.null, logical(1))
  if (!any(ok)) stop("the HMM fit failed for every imputation", call. = FALSE)
  fits <- fits[ok]

  ## pool state probabilities and Viterbi states across imputations
  probs <- Reduce(`+`, lapply(fits, function(f) f$probs)) / length(fits)
  colnames(probs) <- paste0("p", seq_len(n_states))
  state <- max.col(probs, ties.method = "first")
  viterbi <- do.call(cbind, lapply(fits, function(f) f$viterbi))
  agreement <- rowMeans(viterbi == state)

  states <- tibble(est[, c("id", "segment", "date", "lon", "lat", "step", "angle")],
                   as.data.frame(probs),
                   state = state,
                   agreement = agreement)

  par <- bind_rows(lapply(seq_along(fits), function(i) tibble(rep = i, fits[[i]]$par)))

  ## Rubin's rules on the working scale, using imputations whose working-scale
  ##  standard errors are all finite
  pooled <- hmm_pool(fits, n_states)
  par_summary <- pooled$natural
  par_working <- pooled$working

  out <- list(states = states,
              par = par,
              par_summary = par_summary,
              par_working = par_working,
              n_pooled = pooled$n_pooled,
              reps = reps,
              n_states = n_states,
              n_failed = sum(!ok),
              source = attr(imp, "source"),
              time.step = attr(imp, "time.step"))
  class(out) <- append("hmm_mi", class(out))
  out
}

## step lengths (km, great circle) and turning angles (radians, -pi to pi;
##  positive = left, counter-clockwise turn) along each track segment, ordered
##  by date within id and segment
hmm_moves <- function(d) {
  if (!"segment" %in% names(d)) d$segment <- 1L
  d <- d[order(d$id, d$segment, d$date), ]
  rad <- pi / 180
  out <- lapply(split(d, list(d$id, d$segment), drop = TRUE, lex.order = TRUE), function(s) {
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
  out <- out[order(out$id, out$segment, out$date), ]
  row.names(out) <- NULL
  out
}

## fit one HMM with hmmTMB; states ordered by mean step length. Returns NULL
##  if the fit fails.
hmm_fit_one <- function(d, n_states, step_par, angle_par, silent) {
  ## each track segment is a separate series
  data <- data.frame(ID = factor(paste(d$id, d$segment, sep = "_segment_"),
                                 levels = unique(paste(d$id, d$segment, sep = "_segment_"))),
                     time = d$date, step = d$step, angle = d$angle)
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
    ## NaN standard-error warnings (Hessian not positive-definite) are
    ##  handled below: such fits are excluded from parameter pooling
    if (silent) {
      utils::capture.output(suppressWarnings(hmm$fit(silent = TRUE)))
    } else {
      hmm$fit(silent = FALSE)
    }
    hmm
  }, silent = TRUE)
  if (inherits(res, "try-error")) return(NULL)
  hmm <- res

  decoded <- try(list(probs = hmm$state_probs(), viterbi = hmm$viterbi()), silent = TRUE)
  if (inherits(decoded, "try-error")) return(NULL)

  p <- hmm$par()
  obspar <- p$obspar[, , 1]
  tpm <- p$tpm[, , 1]
  step_mean <- obspar["step.mean", ]
  ord <- order(step_mean)

  probs <- decoded$probs[, ord, drop = FALSE]
  viterbi <- match(decoded$viterbi, ord)
  tpm <- tpm[ord, ord, drop = FALSE]

  par <- c(stats::setNames(obspar["step.mean", ord], paste0("step_mean_", seq_len(n_states))),
           stats::setNames(obspar["step.sd", ord], paste0("step_sd_", seq_len(n_states))),
           stats::setNames(obspar["angle.mu", ord], paste0("angle_mu_", seq_len(n_states))),
           stats::setNames(obspar["angle.kappa", ord], paste0("angle_kappa_", seq_len(n_states))))
  tpm_names <- outer(seq_len(n_states), seq_len(n_states), function(i, j) paste0("tpm_", i, j))
  par <- c(par, stats::setNames(as.vector(tpm), as.vector(tpm_names)))

  ## working-scale coefficients and standard errors, relabelled so state k
  ##  is the k-th shortest-step state: observation coefficients are named
  ##  variable.parameter.stateK.(Intercept), transition coefficients Si>Sj
  ##  (log(p_ij / p_ii)); a state's new label is its rank in ord
  cf <- hmm$coeff_fe()
  est <- c(cf$obs[, 1], cf$hid[, 1])
  nm <- c(rownames(cf$obs), rownames(cf$hid))
  rep <- try(hmm$tmb_rep(), silent = TRUE)
  se <- if (inherits(rep, "try-error")) rep(NA_real_, length(est)) else
    suppressWarnings(sqrt(diag(rep$cov.fixed)))[seq_along(est)]
  new_label <- match(seq_len(n_states), ord)
  nm <- vapply(nm, function(z) {
    if (grepl("^S[0-9]+>S[0-9]+", z)) {
      ij <- as.integer(regmatches(z, gregexpr("[0-9]+", sub("\\..*$", "", z)))[[1]])
      paste0("S", new_label[ij[1]], ">S", new_label[ij[2]])
    } else {
      k <- as.integer(sub("^.*\\.state([0-9]+)\\..*$", "\\1", z))
      sub("\\.state[0-9]+\\.", paste0(".state", new_label[k], "."), z)
    }
  }, character(1), USE.NAMES = FALSE)
  nm <- sub("\\.\\(Intercept\\)$", "", nm)
  working <- data.frame(name = nm, estimate = unname(est), se = unname(se))
  working <- working[order(working$name), ]

  ## inverse links of the observation parameters, by variable and parameter
  dists <- hmm$obs()$dists()
  invlink <- lapply(dists, function(dd) dd$invlink())

  list(probs = probs, viterbi = viterbi, par = as.data.frame(as.list(par)),
       working = working, invlink = invlink)
}

## pool HMM parameters across imputations with Rubin's rules on the working
##  scale, and back-transform to the natural scale
hmm_pool <- function(fits, n_states) {
  ok <- vapply(fits, function(f) all(is.finite(f$working$se)), logical(1))
  n_pooled <- sum(ok)
  if (n_pooled == 0) {
    warning("no imputation has finite working-scale standard errors; parameters not pooled",
            call. = FALSE)
    return(list(natural = NULL, working = NULL, n_pooled = 0))
  }
  f <- fits[ok]
  nm <- f[[1]]$working$name
  est <- do.call(cbind, lapply(f, function(z) z$working$estimate[match(nm, z$working$name)]))
  se <- do.call(cbind, lapply(f, function(z) z$working$se[match(nm, z$working$name)]))
  w <- pool_rubin(est, se)
  working <- tibble(parameter = nm, estimate = w$estimate, se = w$se, df = w$df,
                    lower = w$lower, upper = w$upper, fraction_missing = w$fraction_missing)

  ## observation parameters: variable.parameter.stateK, through the inverse
  ##  links (monotone, so interval ends transform directly)
  invlink <- f[[1]]$invlink
  obs <- !grepl("^S[0-9]+>S[0-9]+$", nm)
  parts <- strsplit(nm[obs], ".", fixed = TRUE)
  natural <- lapply(seq_along(parts), function(i) {
    v <- parts[[i]][1]; p <- parts[[i]][2]; k <- sub("state", "", parts[[i]][3])
    g <- invlink[[v]][[p]]
    r <- working[obs, ][i, ]
    tibble(parameter = paste0(v, "_", p, "_", k),
           estimate = g(r$estimate), lower = g(r$lower), upper = g(r$upper),
           fraction_missing = r$fraction_missing)
  })

  ## transition probabilities: multinomial logit with the diagonal as reference
  wt <- matrix(0, n_states, n_states)
  for (z in which(!obs)) {
    ij <- as.integer(regmatches(nm[z], gregexpr("[0-9]+", nm[z]))[[1]])
    wt[ij[1], ij[2]] <- working$estimate[z]
  }
  tpm <- exp(wt) / rowSums(exp(wt))
  tp <- lapply(seq_len(n_states), function(i) lapply(seq_len(n_states), function(j) {
    lower <- upper <- fm <- NA_real_
    if (n_states == 2) {
      ## p_ij = plogis(w_ij) for i != j; p_ii = 1 - p_ij
      z <- which(nm == paste0("S", i, ">S", if (i == j) 3 - i else j))
      b <- stats::plogis(c(working$lower[z], working$upper[z]))
      if (i == j) b <- rev(1 - b)
      lower <- b[1]; upper <- b[2]; fm <- working$fraction_missing[z]
    }
    tibble(parameter = paste0("tpm_", i, j), estimate = tpm[i, j],
           lower = lower, upper = upper, fraction_missing = fm)
  }))
  natural <- bind_rows(c(natural, unlist(tp, recursive = FALSE)))
  ## same parameter order as the per-imputation table
  natural <- natural[order(match(sub("_[0-9]+$", "", natural$parameter),
                                 c("step_mean", "step_sd", "angle_mu", "angle_kappa", "tpm")),
                           natural$parameter), ]
  list(natural = natural, working = working, n_pooled = n_pooled)
}
