##' @title Fit a joint (hierarchical) continuous-time correlated random walk
##'
##' @description Fits a `crw` state-space model to several individual tracks at
##' once, sharing parameters among individuals according to a
##' [aniMotum::share_control] specification. Called by [aniMotum::fit_ssm] when
##' `model = "jcrw"`.
##'
##' @details The model is written in R using RTMB rather than as a C++ template.
##' A joint fit builds one AD object for the whole data set, so RTMB's taping
##' cost is paid once rather than once per individual, and the ability to step
##' into the likelihood matters most exactly where a hierarchical model is
##' hardest: diagnosing whether the among-individual variance is identifiable.
##'
##' Individuals are fitted with a common set of measurement parameters and
##' individual-level diffusion coefficients drawn from an estimated population
##' distribution. See [aniMotum::share_control] for the reasoning, and for the
##' parameter combinations that are refused.
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
##' @param group_lookup optional data.frame with `id` and a grouping variable,
##' assembled by [aniMotum::fit_ssm]
##' @param init one of `"individual"` (default) to initialise the joint fit
##' from separate per-individual `crw` fits, or `"moment"` to use moment-based
##' starting values. Per-individual initialisation costs an extra pass over the
##' data but substantially improves convergence of the joint model
##'
##' @return a named list of `ssm` objects, one per individual, all sharing the
##' single joint fit's optimiser output, TMB object and sdreport
##'
##' @importFrom sf st_as_sf st_set_crs st_crs st_transform st_coordinates
##' @importFrom stats nlminb optim setNames
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
         "  This is deliberate rather than an oversight: a random effect on the\n",
         "  measurement model is only identifiable when the process model is\n",
         "  pooled, and in that configuration the model has little to recommend\n",
         "  it over pooling the measurement model directly. See ?share_control.",
         call. = FALSE)

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
  i1 <- c(1L, i2[-A] + 1L)   ## head() would need utils in Imports
  N <- sum(ni)
  ind <- rep(seq_len(A), ni)          ## individual index for every row

  ## ------------------------------------------------------------------
  ## projection check
  ##
  ## prefilter() projects to a global Mercator grid in km by default. Mercator
  ## is conformal, so x and y are scaled equally at a point and the anisotropy
  ## of D is unaffected. But the scale factor is sec(latitude), and D carries
  ## units of distance squared per unit time, so apparent diffusion is inflated
  ## by sec(latitude)^2. Across individuals occupying different latitude bands
  ## that inflation varies substantially, and a hierarchical model will absorb
  ## it into the among-individual variance, where it is indistinguishable from
  ## biological variation.
  ## ------------------------------------------------------------------
  ## Both the span across individuals and the span within an individual matter.
  ## A single animal migrating across 20 degrees of latitude has its own D
  ## averaged over a scale factor that varies several-fold along its own track,
  ## which the individual filters are subject to as well.
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
      sec2 <- 1 / cos(lat.rng * pi / 180) ^ 2
      ratio <- sec2[2] / sec2[1]

      ## largest span within any single individual
      w.ratio <- max(sapply(chk, function(r) {
        s2 <- 1 / cos(r * pi / 180) ^ 2
        s2[2] / s2[1]
      }))

      if (is.merc && ratio > 1.5) {
        warning("these data span ", round(lat.rng[1], 1), " to ",
                round(lat.rng[2], 1), " degrees absolute latitude on a ",
                "Mercator grid.\n",
                "  Mercator is conformal, so the x,y anisotropy of D is ",
                "unaffected, but the scale\n  factor is sec(latitude) and D ",
                "has units of distance squared per unit time, so\n  apparent ",
                "diffusion is inflated by sec(latitude)^2. That inflation ",
                "varies by a factor\n  of ", round(ratio, 1), " across the ",
                "data set", if (w.ratio > 1.5)
                  paste0(", and by up to ", round(w.ratio, 1),
                         " within a single track") else "", ".\n",
                "  Across individuals it is absorbed into the ",
                "among-individual variance of D, where\n  it cannot be told ",
                "apart from biological variation. Supply the data as an sf\n",
                "  object in an equal-area projection if the population-level ",
                "D or its variance\n  is to be interpreted.",
                call. = FALSE, immediate. = TRUE)
      }
    }
  }

  ## ------------------------------------------------------------------
  ## measurement parameter groups
  ## ------------------------------------------------------------------
  if (is.null(share$group)) {
    grp <- rep(1L, A)
    grp.levels <- "all"
  } else {
    if (is.null(group_lookup))
      stop("share_control(group = \"", share$group, "\") was specified but the ",
           "variable was not found in the input data", call. = FALSE)
    gl <- group_lookup[match(ids, group_lookup$id), share$group]
    if (any(is.na(gl)))
      stop("the grouping variable `", share$group, "` is missing for: ",
           paste(ids[is.na(gl)], collapse = ", "), call. = FALSE)
    gf <- factor(as.character(gl))
    grp <- as.integer(gf)
    grp.levels <- levels(gf)
  }
  G <- length(unique(grp))

  ## individual-level indices for each measurement parameter. "pooled" uses the
  ## group index, "individual" gives every animal its own value, so both are the
  ## same code path in the likelihood with a different index vector.
  idx_for <- function(mode) if (mode == "individual") seq_len(A) else grp
  g_tau <- idx_for(share$tau)
  g_psi <- idx_for(share$psi)
  g_rho_o <- idx_for(share$rho_o)
  g_rho_p <- idx_for(share$rho_p)

  G_tau <- length(unique(g_tau))
  G_psi <- length(unique(g_psi))
  G_rho_o <- length(unique(g_rho_o))
  G_rho_p <- length(unique(g_rho_p))

  ## re-index to 1..G in case of gaps
  g_tau <- as.integer(factor(g_tau))
  g_psi <- as.integer(factor(g_psi))
  g_rho_o <- as.integer(factor(g_rho_o))
  g_rho_p <- as.integer(factor(g_rho_p))

  ## ------------------------------------------------------------------
  ## concatenate model data
  ## ------------------------------------------------------------------
  cat_v <- function(f) unlist(lapply(prep, f), use.names = FALSE)

  Y <- do.call(cbind, lapply(prep, function(p) p$Y))
  K <- do.call(rbind, lapply(prep, function(p) p$K))
  GLerr <- do.call(rbind, lapply(prep, function(p) p$GLerr))
  state0 <- do.call(rbind, lapply(prep, function(p) p$state0))

  dt <- cat_v(function(p) p$dt)
  isd <- cat_v(function(p) p$isd)
  obs_mod <- cat_v(function(p) p$obs_mod)
  gap_flag <- cat_v(function(p) p$gap_flag)
  ho_flag <- cat_v(function(p) p$ho_flag)
  m <- cat_v(function(p) p$m)
  M <- cat_v(function(p) p$M)
  c_eor <- cat_v(function(p) p$c)

  ## observation row indices by measurement model. Prediction rows contribute
  ## nothing and carry NA coordinates, so they are excluded here rather than
  ## being zeroed inside the likelihood.
  obs <- which(isd == 1L)
  i_ls <- obs[obs_mod[obs] == 0L]
  i_kf <- obs[obs_mod[obs] == 1L]
  i_gl <- obs[obs_mod[obs] == 2L]

  ## NA-safety: the ellipse and generic-location variables are only defined for
  ## their own observation types
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

  D_mode <- switch(share$D,
                   pooled = 0L,
                   hierarchical = if (share$hier.D == "shared") 1L else 2L,
                   individual = 3L)

  dimnames(state0) <- NULL
  dimnames(K) <- NULL
  dimnames(GLerr) <- NULL
  dimnames(Y) <- NULL

  dat <- list(
    A = A, N = N,
    i1 = as.integer(i1), i2 = as.integer(i2),
    zeroA = numeric(A),
    Y = Y, dt = dt, state0 = state0,
    gap_flag = gap_flag, ho_flag = ho_flag,
    K = K, m = m, M = M, c_eor = c_eor, GLerr = GLerr,
    i_ls = i_ls, i_kf = i_kf, i_gl = i_gl,
    n_ls = length(i_ls), n_kf = length(i_kf), n_gl = length(i_gl),
    r_tau = g_tau[ind], r_psi = g_psi[ind], r_rho_o = g_rho_o[ind],
    g_rho_p = g_rho_p,
    G_tau = G_tau,
    D_mode = D_mode,
    est_ho = if (share$ho_scale == "pooled") 1L else 0L,
    ho_scale_fixed = control$ho_scale
  )

  ## ------------------------------------------------------------------
  ## starting values
  ## ------------------------------------------------------------------
  if (is.null(parameters)) {

    lD0 <- matrix(1, A, 2)

    ## crude starting states: linearly interpolated locations and the
    ## corresponding finite-difference velocities
    mu0 <- lapply(prep, function(p) t(p$xs))
    v0 <- lapply(prep, function(p) t(p$v.init))

    if (init == "individual") {
      if (control$verbose >= 1)
        cat(paste0("initialising from ", A, " individual crw fits...\n"))
      for (i in seq_len(A)) {
        fi <- try(sfilter(x = x[[i]], model = "crw", time.step = time.step,
                          fit.to.subset = fit.to.subset,
                          control = ssm_control(verbose = 0,
                                                gap.thresh = control$gap.thresh,
                                                ho_scale = control$ho_scale),
                          ho_lookup = ho_lookup), silent = TRUE)
        if (!inherits(fi, "try-error") && length(fi) == 15) {
          pr <- fi$par
          if (all(c("D_x", "D_y") %in% rownames(pr)))
            lD0[i, ] <- log(pmax(1e-8, pr[c("D_x", "D_y"), "Estimate"]))

          ## Take the smoothed states from the same fit.
          ##
          ## Initialising D from the individual fits while leaving mu and v at
          ## their crude finite-difference values makes the starting point
          ## internally inconsistent, and badly so. The finite-difference
          ## velocities are far rougher than any fitted track, and a small
          ## fitted D penalises that roughness in proportion to 1/D. With D at
          ## its converged value rather than a loose default, the starting
          ## objective can reach 1e12 and the inner Newton problem begins in a
          ## region it cannot recover from, returning NaN rather than failing
          ## visibly. Taking mu and v from the same fit that supplied D keeps
          ## the whole starting point self-consistent.
          rr <- try(summary(fi$rep, "random"), silent = TRUE)
          if (!inherits(rr, "try-error")) {
            lo <- rr[rownames(rr) == "mu", 1]
            ve <- rr[rownames(rr) == "v", 1]
            if (length(lo) == 2 * ni[i] && length(ve) == 2 * ni[i]) {
              mu0[[i]] <- matrix(lo, nrow = 2)
              v0[[i]] <- matrix(ve, nrow = 2)
            }
          }
        }
      }
    }

    lD_pop0 <- colMeans(lD0)
    sd0 <- apply(lD0, 2, stats::sd)
    sd0[!is.finite(sd0) | sd0 <= 0] <- 0.5

    lD_re0 <- switch(as.character(D_mode),
                     "0" = numeric(A),
                     "1" = rowMeans(lD0) - mean(rowMeans(lD0)),
                     numeric(2 * A))
    if (D_mode %in% c(2L, 3L))
      lD_re0 <- c(lD0[, 1] - lD_pop0[1], lD0[, 2] - lD_pop0[2])

    parameters <- list(
      lD_pop = lD_pop0,
      l_sd_lD = log(c(mean(sd0), sd0[2])),
      lD_re = lD_re0,
      l_rho_p = rep(0.1, G_rho_p),
      l_tau = rep(0, 2 * G_tau),
      l_psi = rep(0, G_psi),
      l_rho_o = rep(0, G_rho_o),
      l_ho_scale = 0,
      mu = unname(do.call(cbind, mu0)),
      v = unname(do.call(cbind, v0))
    )
  }

  ## ------------------------------------------------------------------
  ## map: switch off parameters that the data cannot inform
  ## ------------------------------------------------------------------
  ## observation types present within each measurement group
  types_in <- function(gidx, ng) {
    out <- matrix(FALSE, ng, 3)   ## LS/GPS, KF, GL
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
  ## tau scales the LS/GPS error multiplication factors only
  need_tau <- t_tau[, 1]
  automap$l_tau <- fac(rep(need_tau, 2))
  ## psi scales the Argos error ellipse semi-minor axis only
  need_psi <- t_psi[, 2]
  automap$l_psi <- fac(need_psi)
  ## rho_o applies to LS/GPS and GL observations, not to error ellipses
  need_rho_o <- t_rho_o[, 1] | t_rho_o[, 3]
  automap$l_rho_o <- fac(need_rho_o)

  ## D random effects and their variance
  if (D_mode == 0L) {
    automap$lD_re <- factor(rep(NA, length(parameters$lD_re)))
    automap$l_sd_lD <- factor(c(NA, NA))
  } else if (D_mode == 1L) {
    automap$l_sd_lD <- factor(c(1, NA))
  } else if (D_mode == 3L) {
    automap$l_sd_lD <- factor(c(NA, NA))
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
  rnd <- c("mu", "v")
  if (D_mode %in% c(1L, 2L)) rnd <- c(rnd, "lD_re")

  if (is.null(inner.control) || !"smartsearch" %in% names(inner.control))
    inner.control <- list(smartsearch = TRUE)

  ## The objective at the starting values, evaluated in plain R with no AD and
  ## no Laplace approximation. A very large value means the starting states and
  ## the starting parameters disagree with each other, which the inner Newton
  ## problem will usually not survive.
  nll0 <- try(as.numeric(jcrw_nll(dat)(parameters)), silent = TRUE)
  if (!inherits(nll0, "try-error")) {
    if (control$verbose >= 1)
      cat(paste0("objective at starting values: ",
                 format(nll0, digits = 4), "\n"))
    if (!is.finite(nll0))
      warning("the objective is not finite at the starting values. The model ",
              "cannot be fitted from here;\n  supply `parameters` directly, or ",
              "try init = \"moment\".", call. = FALSE, immediate. = TRUE)
    else if (nll0 > 1e8)
      warning("the objective at the starting values is ",
              format(nll0, digits = 3), ", which is very large.\n",
              "  The starting states and starting parameters are probably ",
              "inconsistent with each other,\n  and the inner optimiser may ",
              "return NaN. Try init = \"moment\".",
              call. = FALSE, immediate. = TRUE)
  }

  obj <- RTMB::MakeADFun(func = jcrw_nll(dat),
                         parameters = parameters,
                         map = map,
                         random = rnd,
                         silent = control$verbose != 2,
                         inner.control = inner.control)

  obj$env$tracemgc <- control$verbose == 2

  ## parameter bounds, built by name from the active parameter vector. Unlike
  ## the individual filters this needs no positional repair, because the map
  ## has already removed inactive parameters from obj$par.
  L <- setNames(rep(-Inf, length(obj$par)), names(obj$par))
  U <- setNames(rep(Inf, length(obj$par)), names(obj$par))
  L[names(L) %in% c("l_rho_p", "l_rho_o")] <- -7
  U[names(U) %in% c("l_rho_p", "l_rho_o")] <- 7
  L[names(L) == "l_ho_scale"] <- -9
  U[names(U) == "l_ho_scale"] <- 9
  L[names(L) == "l_sd_lD"] <- -8
  U[names(U) == "l_sd_lD"] <- 3

  if (!is.null(control$lower))
    for (nm in names(control$lower)) L[names(L) == nm] <- control$lower[[nm]]
  if (!is.null(control$upper))
    for (nm in names(control$upper)) U[names(U) == nm] <- control$upper[[nm]]

  ## Tighten the optimiser tolerances unless the user set their own.
  ##
  ## ssm_control()'s defaults (rel.tol = 1e-3, x.tol = 1.5e-2) are tuned for
  ## fast per-individual quality control, where only the location states
  ## matter. They are too loose for a hierarchical model: the among-individual
  ## variance sits in a low-curvature direction of the likelihood, and at a
  ## loosely converged point the numerical Hessian in that direction is easily
  ## indefinite, which surfaces as convergence = 0 together with pdHess = FALSE
  ## and NaN standard errors on the population parameters.
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
                                    lower = L, upper = U)),
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
  ## assemble one ssm object per individual
  ## ------------------------------------------------------------------
  if (inherits(opt, "try-error") || inherits(rep, "try-error")) {
    out <- lapply(seq_len(A), function(i) {
      o <- list(call = call, data = x[[i]], inits = parameters,
                pm = "jcrw", ts = time.step, tmb = obj,
                errmsg = opt)
      ## the assembled data list is attached as an attribute rather than a list
      ## element so that the length of the object is unchanged. It lets the
      ## likelihood be called directly in plain R:
      ##   jcrw_nll(attr(fit$ssm[[1]], "jdata"))(fit$ssm[[1]]$inits)
      ## which evaluates with ordinary numerics, no AD and no Laplace
      ## approximation - the quickest way to tell a broken likelihood from a
      ## failed inner problem.
      attr(o, "jdata") <- dat
      class(o) <- append("ssm", class(o))
      o
    })
    names(out) <- ids
    warning("the optimiser or sdreport failed for the joint model. Try ",
            "share_control(D = \"pooled\") to remove the among-individual ",
            "variance, or fit fewer individuals.", call. = FALSE)
    return(out)
  }

  srep <- summary(rep, "report")
  rdm.all <- summary(rep, "random")
  loc.all <- rdm.all[rownames(rdm.all) == "mu", , drop = FALSE]
  vel.all <- rdm.all[rownames(rdm.all) == "v", , drop = FALSE]

  ## Did the hierarchical model collapse to full pooling?
  ##
  ## When there is no detectable among-individual variation, the maximum
  ## likelihood estimate of sd_lD is zero and the optimiser drives l_sd_lD to
  ## its lower bound. The fit is then the pooled model with one extra
  ## parameter, and two things follow that are easy to misread: the standard
  ## error reported for sd_lD is not interpretable, because the estimate is on
  ## a boundary rather than at an interior optimum; and AICc over-penalises,
  ## because the usual penalty assumes an interior optimum (a boundary
  ## parameter needs a mixture null distribution). Say so rather than leaving
  ## the user to notice.
  if (D_mode %in% c(1L, 2L) && "l_sd_lD" %in% names(opt$par)) {
    l.bound <- L[names(L) == "l_sd_lD"][1]
    if (any(abs(opt$par[names(opt$par) == "l_sd_lD"] - l.bound) < 1e-3))
      warning("the among-individual standard deviation of D has gone to its ",
              "lower bound.\n  There is no detectable among-individual ",
              "variation in D in these data, and the fit has\n  collapsed to ",
              "the pooled model. Its estimates should match ",
              "share_control(D = \"pooled\")\n  closely. The standard error ",
              "reported for sd_lD is not interpretable at a boundary,\n  and ",
              "AICc over-penalises this fit relative to the pooled model. Use ",
              "the pooled model.",
              call. = FALSE, immediate. = TRUE)
  }

  ## report names corresponding to parameters the map switched off entirely.
  ## NULL means the parameter has no map entry at all, i.e. it is fully
  ## estimated - the opposite of switched off.
  all_na <- function(f) !is.null(f) && all(is.na(suppressWarnings(
    as.integer(as.character(f)))))
  drop_rn <- character(0)
  if (all_na(map$l_psi)) drop_rn <- c(drop_rn, "psi")
  if (all_na(map$l_tau)) drop_rn <- c(drop_rn, "tau_x", "tau_y")
  if (all_na(map$l_rho_o)) drop_rn <- c(drop_rn, "rho_o")
  if (all_na(map$l_rho_p)) drop_rn <- c(drop_rn, "rho_p")

  npar <- length(opt[["par"]])
  nfit <- sum(isd == 1L)
  objv <- if (control$optim == "nlminb") opt[["objective"]] else opt[["value"]]
  AICc <- 2 * npar + 2 * objv + 2 * npar * (npar + 1) / (nfit - npar - 1)

  out <- vector("list", A)

  for (i in seq_len(A)) {

    k <- i1[i]:i2[i]
    p <- prep[[i]]
    d.all <- p$d.all
    rr <- ((i1[i] - 1) * 2 + 1):(i2[i] * 2)

    loc <- loc.all[rr, , drop = FALSE]
    vel <- vel.all[rr, , drop = FALSE]

    loc <- as.data.frame(cbind(loc[seq(1, nrow(loc), by = 2), ],
                               loc[seq(2, nrow(loc), by = 2), ]),
                         row.names = seq_len(length(k)))
    names(loc) <- c("x", "x.se", "y", "y.se")

    vel <- as.data.frame(cbind(vel[seq(1, nrow(vel), by = 2), ],
                               vel[seq(2, nrow(vel), by = 2), ]),
                         row.names = seq_len(length(k)))
    names(vel) <- c("u", "u.se", "v", "v.se")

    rdm <- cbind(loc, vel)
    rdm$id <- ids[i]
    rdm$date <- d.all$date
    rdm$isd <- d.all$isd
    rdm$ho <- as.integer(d.all$ho)
    rdm <- rdm[, c("id", "date", "x", "y", "x.se", "y.se",
                   "u", "v", "u.se", "v.se", "isd", "ho")]

    rdm <- st_as_sf(rdm, coords = c("x", "y"), remove = FALSE)
    rdm <- st_set_crs(rdm, p$prj)
    rdm <- rdm[, c("id", "date", "x.se", "y.se", "u", "v",
                   "u.se", "v.se", "isd")]

    ## 2-D speed along track, calculated separately for fitted and predicted
    ## states. Standard errors are not propagated here: the delta method for a
    ## joint fit across all individuals is expensive and rarely wanted.
    spd <- function(sub) {
      xy <- st_coordinates(sub)
      tt <- as.numeric(difftime(sub$date,
                                c(as.POSIXct(NA), sub$date[-nrow(sub)]),
                                units = "hours"))
      s <- c(NA, sqrt(diff(xy[, 1]) ^ 2 + diff(xy[, 2]) ^ 2) / tt[-1])
      s
    }

    fv <- subset(rdm, isd)[, -9]
    fv$s <- spd(subset(rdm, isd))
    fv$s.se <- NA
    fv$gap_flag <- p$gap_flag[d.all$isd]

    if (all(!is.na(time.step))) {
      pv <- subset(rdm, !isd)[, -9]
      pv$s <- spd(subset(rdm, !isd))
      pv$s.se <- NA
      pv$gap_flag <- p$gap_flag[!d.all$isd]
    } else {
      pv <- NULL
    }

    ## parameter table: the shared parameters, plus this individual's own
    ## diffusion coefficient. Parameters switched off by the map are dropped
    ## rather than reported as an estimate with a zero standard error.
    keep <- !rownames(srep) %in% c("D1", "D2", drop_rn)
    fxd <- srep[keep, , drop = FALSE]
    Di <- srep[rownames(srep) %in% c("D1", "D2"), , drop = FALSE]
    if (nrow(Di) == 2 * A) {
      di <- rbind(Di[i, , drop = FALSE], Di[A + i, , drop = FALSE])
      rownames(di) <- c("D_x", "D_y")
      fxd <- rbind(fxd, di)
    }
    rn <- rownames(fxd)
    rn[rn == "D_pop"] <- c("D_pop_x", "D_pop_y")[seq_len(sum(rn == "D_pop"))]
    rownames(fxd) <- make.unique(rn)

    o <- list(
      call = call,
      predicted = pv,
      fitted = fv,
      par = fxd,
      data = x[[i]],
      isd = d.all$isd,
      inits = parameters,
      pm = "jcrw",
      ts = time.step,
      opt = opt,
      tmb = obj,
      rep = rep,
      AICc = AICc,
      optimiser = control$optim,
      time = proc.time() - st
    )
    attr(o, "jdata") <- dat
    class(o) <- append("ssm", class(o))
    out[[i]] <- o
  }

  names(out) <- ids

  if (!rep$pdHess)
    warning("the joint Hessian was not positive-definite, so some standard ",
            "errors could not be calculated.\n",
            "  With a hierarchical model this most often means the ",
            "among-individual variance of D is\n  not identifiable from these ",
            "data - too few individuals, or tracks too short. Try\n",
            "  share_control(D = \"pooled\"), or map = list(psi = factor(NA)).",
            call. = FALSE)

  out
}
