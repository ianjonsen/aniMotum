##' @title Fit a joint (hierarchical) move persistence state-space model
##'
##' @description Fits an `mp` state-space model to several individual tracks at
##' once, sharing parameters among individuals according to a
##' [aniMotum::share_control] specification. Called by [aniMotum::fit_ssm] when
##' `model = "jmp"`.
##'
##' @details The model is written in R using RTMB rather than as a C++
##' template. A joint fit builds one AD object for the whole data set, so
##' RTMB's taping cost is paid once rather than once per individual.
##'
##' By default the measurement model and `sigma_g` are pooled, the process
##' innovation scale is hierarchical, and `rho_p` is estimated separately per
##' individual. See [aniMotum::share_control] for the reasoning behind each of
##' those choices, and for the combinations that are refused.
##'
##' @param x a named list of prefiltered sf-tibbles, one per individual, as
##' produced by [aniMotum::prefilter]
##' @param time.step prediction interval in hours, a data.frame of prediction
##' times, or NA
##' @param share a parameter sharing specification from
##' [aniMotum::share_control]
##' @param parameters optional named list of initial values
##' @param map optional named list of parameters to fix during estimation
##' @param fit.to.subset fit to the prefiltered subset of observations
##' @param control a list of control values from [aniMotum::ssm_control]
##' @param inner.control control settings for the inner optimiser
##' @param ho_lookup optional haulout lookup assembled by [aniMotum::fit_ssm]
##' @param group_lookup optional data.frame with `id` and a grouping variable
##' @param init one of `"individual"` (default) to initialise from separate
##' per-individual `mp` fits, or `"moment"` for moment-based starting values.
##' Per-individual initialisation costs an extra pass over the data but
##' substantially improves convergence, because it supplies starting states
##' that are consistent with the starting parameters
##'
##' @return a named list of `mp_ssm` objects, one per individual, all sharing
##' the single joint fit's optimiser output, TMB object and sdreport
##'
##' @importFrom sf st_as_sf st_set_crs st_crs st_transform st_coordinates
##' @importFrom stats nlminb optim setNames plogis median sd
##'
##' @keywords internal

jsfilter <- function(x,
                     time.step = NA,
                     share = share_control(),
                     parameters = NULL,
                     map = NULL,
                     fit.to.subset = TRUE,
                     control = ssm_control(),
                     inner.control = NULL,
                     ho_lookup = NULL,
                     group_lookup = NULL,
                     init = c("individual", "moment")) {

  st <- proc.time()
  call <- match.call()
  init <- match.arg(init)

  if (!requireNamespace("RTMB", quietly = TRUE))
    stop("the RTMB package is required to fit joint models.\n",
         "  install.packages(\"RTMB\")", call. = FALSE)

  if (!is.list(x) || length(x) < 2)
    stop("a joint model requires at least 2 individual tracks", call. = FALSE)

  if (share$tau == "hierarchical" || share$psi == "hierarchical")
    stop("hierarchical measurement parameters are not implemented.\n",
         "  This is deliberate: a random effect on the measurement model is\n",
         "  only identifiable when the process model is pooled, and in that\n",
         "  configuration it has little to recommend it over pooling the\n",
         "  measurement model directly. See ?share_control.", call. = FALSE)

  A <- length(x)
  ids <- names(x)

  ## ------------------------------------------------------------------
  ## per-individual data preparation
  ## ------------------------------------------------------------------
  prep <- lapply(x, function(xx)
    ssm_prep(xx, time.step = time.step, fit.to.subset = fit.to.subset,
             control = control, ho_lookup = ho_lookup))

  ni <- sapply(prep, function(p) ncol(p$Y))
  i2 <- cumsum(ni)
  i1 <- c(1L, i2[-A] + 1L)
  N <- sum(ni)
  ind <- rep(seq_len(A), ni)

  ## ------------------------------------------------------------------
  ## projection check
  ##
  ## prefilter() projects to a global Mercator grid in km by default. Mercator
  ## is conformal, so x and y are scaled equally at a point and the anisotropy
  ## of the process covariance is unaffected. But the scale factor is
  ## sec(latitude), and sigma has units of distance per unit time, so apparent
  ## movement scale is inflated by sec(latitude). Both the span across
  ## individuals and the span within an individual matter.
  ## ------------------------------------------------------------------
  chk <- try({
    lapply(x, function(xx) {
      g <- st_coordinates(st_transform(xx, 4326))
      range(abs(g[, 2]), na.rm = TRUE)
    })
  }, silent = TRUE)

  if (!inherits(chk, "try-error")) {
    lat.rng <- range(unlist(chk), na.rm = TRUE)

    if (all(is.finite(lat.rng))) {
      is.merc <- grepl("merc", tolower(paste(st_crs(x[[1]])$proj4string,
                                             st_crs(x[[1]])$wkt)))
      sec <- 1 / cos(lat.rng * pi / 180)
      ratio <- sec[2] / sec[1]
      w.ratio <- max(sapply(chk, function(r) {
        s <- 1 / cos(r * pi / 180)
        s[2] / s[1]
      }))

      if (is.merc && ratio > 1.3) {
        warning("these data span ", round(lat.rng[1], 1), " to ",
                round(lat.rng[2], 1), " degrees absolute latitude on a ",
                "Mercator grid.\n",
                "  The scale factor is sec(latitude), so apparent movement ",
                "scale differs by a factor\n  of ", round(ratio, 1),
                " across the data set",
                if (w.ratio > 1.3)
                  paste0(", and by up to ", round(w.ratio, 1),
                         " within a single track") else "", ".\n",
                "  Across individuals this is absorbed into the ",
                "among-individual variance of sigma,\n  where it cannot be ",
                "told apart from biological variation. Supply the data as an\n",
                "  sf object in an equal-area projection if the ",
                "population-level sigma or its\n  variance is to be ",
                "interpreted. g_t and sigma_g are unaffected, being ",
                "scale-free.",
                call. = FALSE, immediate. = TRUE)
      }
    }
  }

  ## ------------------------------------------------------------------
  ## measurement parameter groups
  ## ------------------------------------------------------------------
  if (is.null(share$group)) {
    grp <- rep(1L, A)
  } else {
    if (is.null(group_lookup))
      stop("share_control(group = \"", share$group, "\") was specified but the ",
           "variable was not found in the input data", call. = FALSE)
    gl <- group_lookup[match(ids, group_lookup$id), share$group]
    if (any(is.na(gl)))
      stop("the grouping variable `", share$group, "` is missing for: ",
           paste(ids[is.na(gl)], collapse = ", "), call. = FALSE)
    grp <- as.integer(factor(as.character(gl)))
  }

  ## individual-level index for each shared parameter. "pooled" uses the group
  ## index, "individual" gives every animal its own value, so both are the same
  ## code path in the likelihood with a different index vector.
  idx_for <- function(mode) if (mode == "individual") seq_len(A) else grp
  g_tau <- as.integer(factor(idx_for(share$tau)))
  g_psi <- as.integer(factor(idx_for(share$psi)))
  g_rho_o <- as.integer(factor(idx_for(share$rho_o)))
  g_rho_p <- as.integer(factor(idx_for(share$rho_p)))
  g_sg <- as.integer(factor(idx_for(share$sigma_g)))

  G_tau <- max(g_tau); G_psi <- max(g_psi); G_rho_o <- max(g_rho_o)
  G_rho_p <- max(g_rho_p); G_sg <- max(g_sg)

  ## ------------------------------------------------------------------
  ## concatenate model data
  ## ------------------------------------------------------------------
  cat_v <- function(f) unlist(lapply(prep, f), use.names = FALSE)

  Y <- do.call(cbind, lapply(prep, function(p) p$Y))
  K <- do.call(rbind, lapply(prep, function(p) p$K))
  GLerr <- do.call(rbind, lapply(prep, function(p) p$GLerr))

  dt <- cat_v(function(p) p$dt)
  isd <- cat_v(function(p) p$isd)
  obs_mod <- cat_v(function(p) p$obs_mod)
  gap_flag <- cat_v(function(p) p$gap_flag)
  ho_flag <- cat_v(function(p) p$ho_flag)
  m <- cat_v(function(p) p$m)
  M <- cat_v(function(p) p$M)
  c_eor <- cat_v(function(p) p$c)

  obs <- which(isd == 1L)
  i_ls <- obs[obs_mod[obs] == 0L]
  i_kf <- obs[obs_mod[obs] == 1L]
  i_gl <- obs[obs_mod[obs] == 2L]

  if (length(i_kf)) {
    bad <- !is.finite(m[i_kf]) | !is.finite(M[i_kf]) | !is.finite(c_eor[i_kf])
    if (any(bad)) i_kf <- i_kf[!bad]
  }
  if (length(i_gl)) {
    bad <- !is.finite(GLerr[i_gl, 1]) | !is.finite(GLerr[i_gl, 2])
    if (any(bad)) i_gl <- i_gl[!bad]
  }
  m[!is.finite(m)] <- 0
  M[!is.finite(M)] <- 0
  c_eor[!is.finite(c_eor)] <- 0
  K[!is.finite(K)] <- 1
  GLerr[!is.finite(GLerr)] <- 1
  Y[!is.finite(Y)] <- 0

  dimnames(K) <- NULL
  dimnames(GLerr) <- NULL
  dimnames(Y) <- NULL

  sig_mode <- switch(share$sigma,
                     pooled = 0L,
                     hierarchical = if (share$hier.sigma == "shared") 1L else 2L,
                     individual = 3L)

  dat <- list(
    A = A, N = N,
    i1 = as.integer(i1), i2 = as.integer(i2),
    zeroA = numeric(A),
    Y = Y, dt = dt,
    gap_flag = gap_flag, ho_flag = ho_flag,
    K = K, m = m, M = M, c_eor = c_eor, GLerr = GLerr,
    i_ls = i_ls, i_kf = i_kf, i_gl = i_gl,
    n_ls = length(i_ls), n_kf = length(i_kf), n_gl = length(i_gl),
    r_tau = g_tau[ind], r_psi = g_psi[ind], r_rho_o = g_rho_o[ind],
    g_rho_p = g_rho_p, g_sg = g_sg,
    G_tau = G_tau,
    sig_mode = sig_mode,
    est_ho = if (share$ho_scale == "pooled") 1L else 0L,
    ho_scale_fixed = control$ho_scale
  )

  ## ------------------------------------------------------------------
  ## starting values
  ## ------------------------------------------------------------------
  if (is.null(parameters)) {

    lsig0 <- matrix(0, A, 2)
    lsg0 <- numeric(A)
    X0 <- lapply(prep, function(p) t(p$xs))
    lg0 <- lapply(prep, function(p) rep(0, ncol(p$Y)))

    ## crude moment starting values for sigma: the covariance of the
    ## first differences of the interpolated path
    for (i in seq_len(A)) {
      es <- prep[[i]]$xs[-1, , drop = FALSE] -
        prep[[i]]$xs[-nrow(prep[[i]]$xs), , drop = FALSE]
      sg <- sqrt(diag(stats::cov(es)))
      lsig0[i, ] <- log(pmax(1e-8, sg))
    }

    if (init == "individual") {
      if (control$verbose >= 1)
        cat(paste0("initialising from ", A, " individual mp fits...\n"))
      for (i in seq_len(A)) {
        fi <- try(mpfilter(x = x[[i]], model = "mp", time.step = time.step,
                           fit.to.subset = fit.to.subset,
                           control = ssm_control(verbose = 0,
                                                 gap.thresh = control$gap.thresh,
                                                 ho_scale = control$ho_scale),
                           ho_lookup = ho_lookup), silent = TRUE)
        if (!inherits(fi, "try-error") && length(fi) == 15) {
          pr <- fi$par
          if (all(c("sigma_x", "sigma_y") %in% rownames(pr)))
            lsig0[i, ] <- log(pmax(1e-8, pr[c("sigma_x", "sigma_y"),
                                            "Estimate"]))
          if ("sigma_g" %in% rownames(pr))
            lsg0[i] <- log(pmax(1e-8, pr["sigma_g", "Estimate"]))

          ## Take the smoothed states from the same fit. Initialising the
          ## parameters from a converged fit while leaving the states at their
          ## crude interpolated values makes the starting point internally
          ## inconsistent, and a tight fitted sigma then penalises the crude
          ## path enormously - the objective can reach 1e12 and the inner
          ## Newton problem starts where it cannot recover.
          rr <- try(summary(fi$rep, "random"), silent = TRUE)
          if (!inherits(rr, "try-error")) {
            xx <- rr[rownames(rr) == "X", 1]
            gg <- rr[rownames(rr) == "lg", 1]
            if (length(xx) == 2 * ni[i]) X0[[i]] <- matrix(xx, nrow = 2)
            if (length(gg) == ni[i]) lg0[[i]] <- gg
          }
        }
      }
    }

    lsig_pop0 <- colMeans(lsig0)
    sd0 <- stats::sd(rowMeans(lsig0))
    if (!is.finite(sd0) || sd0 <= 0) sd0 <- 0.5

    lsig_re0 <- switch(as.character(sig_mode),
                       "0" = numeric(A),
                       "1" = rowMeans(lsig0) - mean(rowMeans(lsig0)),
                       numeric(2 * A))
    if (sig_mode %in% c(2L, 3L))
      lsig_re0 <- c(lsig0[, 1] - lsig_pop0[1], lsig0[, 2] - lsig_pop0[2])

    parameters <- list(
      lsig_pop = lsig_pop0,
      l_sd_lsig = rep(log(sd0), 2),
      lsig_re = lsig_re0,
      l_rho_p = rep(0.1, G_rho_p),
      l_sigma_g = rep(mean(lsg0), G_sg),
      l_tau = rep(0, 2 * G_tau),
      l_psi = rep(0, G_psi),
      l_rho_o = rep(0, G_rho_o),
      l_ho_scale = 0,
      X = unname(do.call(cbind, X0)),
      lg = unlist(lg0, use.names = FALSE)
    )
  }

  ## ------------------------------------------------------------------
  ## map: switch off parameters the data cannot inform
  ## ------------------------------------------------------------------
  types_in <- function(gidx, ng) {
    out <- matrix(FALSE, ng, 3)
    for (i in seq_len(A)) {
      om <- prep[[i]]$obs_mod[prep[[i]]$isd == 1L]
      g <- gidx[i]
      if (any(om == 0L)) out[g, 1] <- TRUE
      if (any(om == 1L)) out[g, 2] <- TRUE
      if (any(om == 2L)) out[g, 3] <- TRUE
    }
    out
  }

  fac <- function(need) {
    out <- rep(NA_integer_, length(need))
    if (any(need)) out[need] <- seq_len(sum(need))
    factor(out)
  }

  t_tau <- types_in(g_tau, G_tau)
  t_psi <- types_in(g_psi, G_psi)
  t_rho_o <- types_in(g_rho_o, G_rho_o)

  automap <- list()
  automap$l_tau <- fac(rep(t_tau[, 1], 2))
  automap$l_psi <- fac(t_psi[, 2])
  automap$l_rho_o <- fac(t_rho_o[, 1] | t_rho_o[, 3])

  if (sig_mode == 0L) {
    automap$lsig_re <- factor(rep(NA, length(parameters$lsig_re)))
    automap$l_sd_lsig <- factor(c(NA, NA))
  } else if (sig_mode == 1L) {
    automap$l_sd_lsig <- factor(c(1, NA))
  } else if (sig_mode == 3L) {
    automap$l_sd_lsig <- factor(c(NA, NA))
  }

  if (share$ho_scale != "pooled" || !any(ho_flag == 1L))
    automap$l_ho_scale <- factor(NA)

  if (!is.null(map)) {
    names(map) <- ifelse(grepl("^l_", names(map)), names(map),
                         paste0("l_", names(map)))
    automap[names(map)] <- map
  }
  map <- automap[!sapply(automap, is.null)]

  ## ------------------------------------------------------------------
  ## build and minimise
  ## ------------------------------------------------------------------
  rnd <- c("X", "lg")
  if (sig_mode %in% c(1L, 2L)) rnd <- c(rnd, "lsig_re")

  if (is.null(inner.control) || !"smartsearch" %in% names(inner.control))
    inner.control <- list(smartsearch = TRUE)

  ## The objective at the starting values, in plain R with no AD and no Laplace
  ## approximation. A very large value means the starting states and starting
  ## parameters disagree, which the inner Newton problem usually will not
  ## survive.
  nll0 <- try(as.numeric(jmp_nll(dat)(parameters)), silent = TRUE)
  if (!inherits(nll0, "try-error")) {
    if (control$verbose >= 1)
      cat(paste0("objective at starting values: ",
                 format(nll0, digits = 4), "\n"))
    if (!is.finite(nll0))
      warning("the objective is not finite at the starting values. The model ",
              "cannot be fitted from here;\n  supply `parameters` directly.",
              call. = FALSE, immediate. = TRUE)
    else if (nll0 > 1e8)
      warning("the objective at the starting values is ",
              format(nll0, digits = 3), ", which is very large.\n",
              "  The starting states and starting parameters are probably ",
              "inconsistent, and the inner\n  optimiser may return NaN. Try ",
              "init = \"individual\".", call. = FALSE, immediate. = TRUE)
  }

  obj <- RTMB::MakeADFun(func = jmp_nll(dat),
                         parameters = parameters,
                         map = map,
                         random = rnd,
                         silent = control$verbose != 2,
                         inner.control = inner.control)

  obj$env$tracemgc <- control$verbose == 2

  ## parameter bounds, built by name from the active parameter vector. The map
  ## has already removed inactive parameters from obj$par, so no positional
  ## repair is needed.
  L <- setNames(rep(-Inf, length(obj$par)), names(obj$par))
  U <- setNames(rep(Inf, length(obj$par)), names(obj$par))
  L[names(L) %in% c("l_rho_p", "l_rho_o")] <- -7
  U[names(U) %in% c("l_rho_p", "l_rho_o")] <- 7
  L[names(L) == "l_sigma_g"] <- -10
  U[names(U) == "l_sigma_g"] <- 50
  L[names(L) == "l_ho_scale"] <- -9
  U[names(U) == "l_ho_scale"] <- 9
  L[names(L) == "l_sd_lsig"] <- -8
  U[names(U) == "l_sd_lsig"] <- 3

  if (!is.null(control$lower))
    for (nm in names(control$lower)) L[names(L) == nm] <- control$lower[[nm]]
  if (!is.null(control$upper))
    for (nm in names(control$upper)) U[names(U) == nm] <- control$upper[[nm]]

  ## Tighten the optimiser tolerances unless the user set their own.
  ## ssm_control()'s defaults (rel.tol 1e-3, x.tol 1.5e-2) are tuned for fast
  ## per-individual quality control. They are too loose for a hierarchical
  ## model: the among-individual variance sits in a low-curvature direction,
  ## and at a loosely converged point the numerical Hessian there is easily
  ## indefinite, which surfaces as convergence = 0 with pdHess = FALSE and NaN
  ## standard errors on the population parameters.
  if (control$optim == "nlminb" &&
      identical(control$control, ssm_control()$control)) {
    control$control$rel.tol <- 1e-10
    control$control$x.tol <- 1e-8
    if (control$verbose >= 1)
      cat("using tightened optimiser tolerances for the joint fit\n")
  }

  myfn <- function(p) {
    cat("\r", "pars:  ", round(p, 5), "     ")
    utils::flush.console()
    obj$fn(p)
  }
  fn <- if (control$verbose == 1) myfn else obj$fn

  oldw <- getOption("warn")
  options(warn = -1)

  opt <- switch(control$optim,
                nlminb = try(nlminb(obj$par, fn, obj$gr,
                                    control = control$control,
                                    lower = L, upper = U), silent = TRUE),
                optim = try(do.call(optim,
                                    args = list(par = obj$par, fn = fn,
                                                gr = obj$gr,
                                                method = control$method,
                                                control = control$control,
                                                lower = L, upper = U)),
                            silent = TRUE))
  if (control$verbose == 1) cat("\n")

  rep <- try(RTMB::sdreport(obj), silent = TRUE)
  options(warn = oldw)

  ## ------------------------------------------------------------------
  ## assemble one mp_ssm object per individual
  ## ------------------------------------------------------------------
  if (inherits(opt, "try-error") || inherits(rep, "try-error")) {
    out <- lapply(seq_len(A), function(i) {
      o <- list(call = call, data = x[[i]], inits = parameters,
                pm = "jmp", ts = time.step, tmb = obj, errmsg = opt)
      attr(o, "jdata") <- dat
      class(o) <- append("mp_ssm", class(o))
      o
    })
    names(out) <- ids
    warning("the optimiser or sdreport failed for the joint model. Try ",
            "share_control(sigma = \"pooled\"),\n  or fewer individuals.",
            call. = FALSE)
    return(out)
  }

  ## Did the hierarchical model collapse to full pooling?
  ##
  ## When there is no detectable among-individual variation, the maximum
  ## likelihood estimate of sd_lsig is zero and the optimiser drives l_sd_lsig
  ## to its lower bound. Two things follow that are easy to misread: the
  ## standard error reported for sd_lsig is not interpretable, because the
  ## estimate is on a boundary rather than at an interior optimum; and AICc
  ## over-penalises, because the usual penalty assumes an interior optimum.
  if (sig_mode %in% c(1L, 2L) && "l_sd_lsig" %in% names(opt$par)) {
    l.bound <- L[names(L) == "l_sd_lsig"][1]
    if (any(abs(opt$par[names(opt$par) == "l_sd_lsig"] - l.bound) < 1e-3))
      warning("the among-individual standard deviation of sigma has gone to ",
              "its lower bound.\n  There is no detectable among-individual ",
              "variation in movement scale in these\n  data, and the fit has ",
              "collapsed to the pooled model. The standard error\n  reported ",
              "for sd_lsig is not interpretable at a boundary, and AICc ",
              "over-penalises\n  this fit relative to share_control(sigma = ",
              "\"pooled\").", call. = FALSE, immediate. = TRUE)
  }

  ## report names for parameters the map switched off entirely. A NULL map
  ## entry means no map at all, i.e. fully estimated - not switched off.
  all_na <- function(f) !is.null(f) && all(is.na(suppressWarnings(
    as.integer(as.character(f)))))
  drop_rn <- character(0)
  if (all_na(map$l_psi)) drop_rn <- c(drop_rn, "psi")
  if (all_na(map$l_tau)) drop_rn <- c(drop_rn, "tau_x", "tau_y")
  if (all_na(map$l_rho_o)) drop_rn <- c(drop_rn, "rho_o")

  srep <- summary(rep, "report")
  rdm.all <- summary(rep, "random")
  X.all <- rdm.all[rownames(rdm.all) == "X", , drop = FALSE]
  lg.all <- rdm.all[rownames(rdm.all) == "lg", , drop = FALSE]

  npar <- length(opt[["par"]])
  nfit <- sum(isd == 1L)
  objv <- if (control$optim == "nlminb") opt[["objective"]] else opt[["value"]]
  AICc <- 2 * npar + 2 * objv + (2 * npar ^ 2 + 2 * npar) / (nfit - npar)

  out <- vector("list", A)

  for (i in seq_len(A)) {

    k <- i1[i]:i2[i]
    p <- prep[[i]]
    d.all <- p$d.all
    rr <- ((i1[i] - 1) * 2 + 1):(i2[i] * 2)

    loc <- X.all[rr, , drop = FALSE]
    lgi <- lg.all[k, , drop = FALSE]

    rdm <- as.data.frame(cbind(loc[seq(1, nrow(loc), by = 2), ],
                               loc[seq(2, nrow(loc), by = 2), ]),
                         row.names = seq_len(length(k)))[, c(1, 3, 2, 4)]
    names(rdm) <- c("x", "y", "x.se", "y.se")

    rdm$logit_g <- lgi[, 1]
    rdm$logit_g.se <- lgi[, 2]
    rdm$g <- plogis(lgi[, 1])

    rdm$id <- ids[i]
    rdm$date <- d.all$date
    rdm$isd <- d.all$isd

    ## Mask g during haulout. The model estimates lg continuously across
    ## haulout steps, which is required to connect the pre- and post-haulout
    ## gamma states through the likelihood, but those values reflect the animal
    ## being stationary rather than at-sea movement behaviour and would be
    ## confused with genuinely low move persistence at sea.
    hoi <- p$ho_flag
    if (any(hoi == 1L)) {
      rdm$g[hoi == 1L] <- NA_real_
      rdm$logit_g[hoi == 1L] <- NA_real_
      rdm$logit_g.se[hoi == 1L] <- NA_real_
    }
    rdm$ho <- as.integer(d.all$ho)

    rdm <- st_as_sf(rdm, coords = c("x", "y"), remove = FALSE)
    rdm <- st_set_crs(rdm, p$prj)
    rdm <- rdm[, c("id", "date", "x.se", "y.se",
                   "logit_g", "logit_g.se", "g", "isd")]

    fv <- subset(rdm, isd)[, -8]
    if (all(!is.na(time.step))) pv <- subset(rdm, !isd)[, -8] else pv <- NULL

    ## Parameter table. Any parameter estimated per group - which includes
    ## every parameter when it is specified as "individual" - is reported only
    ## at this individual's own group, so each animal's table carries one value
    ## per parameter rather than the whole population's.
    grouped <- list(sigma_x = i, sigma_y = i,
                    rho_p = g_rho_p[i], sigma_g = g_sg[i],
                    tau_x = g_tau[i], tau_y = g_tau[i],
                    psi = g_psi[i], rho_o = g_rho_o[i])

    rn.all <- rownames(srep)
    keep <- !rn.all %in% c(names(grouped), drop_rn)
    fxd <- srep[keep, , drop = FALSE]

    for (nm in names(grouped)) {
      rws <- which(rn.all == nm)
      if (!length(rws) || nm %in% drop_rn) next
      j <- grouped[[nm]]
      if (j > length(rws)) j <- 1L
      one <- srep[rws[j], , drop = FALSE]
      rownames(one) <- nm
      fxd <- rbind(fxd, one)
    }

    rn <- rownames(fxd)
    if (sum(rn == "sigma_pop") == 2)
      rn[rn == "sigma_pop"] <- c("sigma_pop_x", "sigma_pop_y")
    if (sum(rn == "sd_lsig") == 2)
      rn[rn == "sd_lsig"] <- c("sd_lsig_x", "sd_lsig_y")
    rownames(fxd) <- make.unique(rn)

    ## keep a stable, readable order
    ord <- c("sigma_pop_x", "sigma_pop_y", "sigma_pop", "sd_lsig",
             "sd_lsig_x", "sd_lsig_y", "sigma_x", "sigma_y", "sigma_g",
             "rho_p", "tau_x", "tau_y", "psi", "rho_o", "hos")
    fxd <- fxd[order(match(rownames(fxd), ord), na.last = TRUE), , drop = FALSE]

    o <- list(
      call = call,
      predicted = pv,
      fitted = fv,
      par = fxd,
      data = x[[i]],
      isd = d.all$isd,
      inits = parameters,
      pm = "jmp",
      ts = time.step,
      opt = opt,
      tmb = obj,
      rep = rep,
      AICc = AICc,
      optimiser = control$optim,
      time = proc.time() - st
    )
    attr(o, "jdata") <- dat
    class(o) <- append("mp_ssm", class(o))
    out[[i]] <- o
  }

  names(out) <- ids

  if (!rep$pdHess)
    warning("the joint Hessian was not positive-definite, so some standard ",
            "errors could not be\n  calculated. With a hierarchical model this ",
            "most often means the among-individual\n  variance of sigma is not ",
            "identifiable from these data - too few individuals, or\n  tracks ",
            "too short. Try share_control(sigma = \"pooled\").",
            call. = FALSE)

  out
}
