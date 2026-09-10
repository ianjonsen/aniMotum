##' @title Simulate tracks from the joint correlated random walk
##'
##' @description Simulates multiple individual tracks from the `jcrw` process
##' and observation models with known parameter values, for testing whether
##' [aniMotum::fit_ssm] recovers them.
##'
##' @details Tracks are simulated directly in a projected coordinate frame and
##' returned as an `sf` object carrying that projection. `fit_ssm` respects an
##' `sf` input, so nothing is re-projected between simulating and fitting and
##' no map distortion enters the comparison. That matters here: on a Mercator
##' grid the apparent diffusion magnitude is inflated by `sec(latitude)^2`, so
##' simulating in longitude and latitude would confound the recovery test with
##' the projection.
##'
##' The process is the one in [aniMotum::jcrw_nll]:
##'
##' ```
##'   v[t]  = v[t-1] + e[t]        e[t] ~ N(0, dt[t] * V)
##'   mu[t] = mu[t-1] + v[t]*dt[t]
##' ```
##'
##' with each individual's velocity innovation covariance built from its
##' magnitude, anisotropy and orientation:
##'
##' ```
##'   V = exp(m) * expm(A)         A = [ u1   u2 ]
##'                                    [ u2  -u1 ]
##' ```
##'
##' where `m = log(2 * D)`, `u1 = rho * cos(2 * theta)`,
##' `u2 = rho * sin(2 * theta)` and `rho = log(aniso) / 2`. `V` then has
##' eigenvalues `exp(m +/- rho)`, so the ratio of its axes is `aniso` and its
##' major axis lies at `theta`.
##'
##' Individual magnitudes are drawn as `m[i] ~ N(log(2 * D), sd_m^2)`. Set
##' `sd_m` well away from zero to test whether the hierarchical model can
##' recover among-individual variation - a fit to data with no such variation
##' can only ever show `sd_m` collapsing to its lower bound, which tests
##' nothing.
##'
##' \strong{A note on choosing D.} Velocity in a correlated random walk is an
##' integrated random walk, so it has no stationary distribution: its standard
##' deviation grows as `sqrt(2 * D * T)` over a track of total duration `T`.
##' Simulated speeds therefore increase with track length, and a `D` taken from
##' a real fit will not generally produce plausible speeds when run forward -
##' a real fit is held in check by the observations, forward simulation is not.
##' At six-hourly intervals over 300 steps, `D = 0.005` gives a median speed of
##' about 2.4 km/h, in the right range for a southern elephant seal, while
##' `D = 0.05` gives about 7 km/h and `D = 0.5` around 25 km/h. Pick `D` for
##' the speeds you want at the track length you are simulating, and check with
##' `sqrt(colSums(attr(d, "truth")$states[[1]]$v^2))`. This is a property of
##' the process model, not of the simulator.
##'
##' Argos error ellipse parameters are drawn from the same log-normal and von
##' Mises distributions, fitted to real Argos data, that [aniMotum::sim] uses.
##' Note that the errors here are generated with `eor` converted to radians;
##' the internal `ellp.par` applies `sin()` and `cos()` to `eor` in degrees,
##' so its errors do not match the ellipse parameters recorded beside them.
##'
##' @param A number of individuals
##' @param n number of locations per individual; a scalar, or a vector of
##' length `A`
##' @param dt mean time interval between locations, in hours
##' @param irregular logical; draw intervals from a gamma distribution with
##' mean `dt` (TRUE, the default) rather than using a regular interval
##' @param D population mean diffusion magnitude, on the same scale as `crw`'s
##' `D_x` and `D_y`. The default gives plausible elephant seal speeds at the
##' default track length; see Details
##' @param sd_m standard deviation of `log(2 * D)` among individuals. Zero
##' gives identical individuals
##' @param aniso ratio of the axes of the velocity innovation covariance; a
##' scalar shared by all individuals, or a vector of length `A`. 1 is isotropic
##' @param orient orientation of the major axis in degrees, a vector of length
##' `A`. NULL (default) draws orientations uniformly
##' @param error observation error model: `"LS"` (default) for Argos
##' least-squares classes, `"KF"` for error ellipses
##' @param tau scaling on the error multiplication factors, length 2
##' @param rho_o correlation between the x and y observation errors, `"LS"`
##' only
##' @param psi scaling on the ellipse semi-minor axis, `"KF"` only
##' @param lc.prob named vector of Argos location class probabilities
##' @param crs projection to simulate and return in
##' @param start two-element vector giving the starting coordinate, in the
##' units of `crs`
##' @param start.date start date-time, as a character string or POSIXt
##'
##' @return an `sf` object in the requested projection with columns `id`,
##' `date`, `lc` and, for `error = "KF"`, `smaj`, `smin` and `eor`. The
##' generating parameters and the latent states are attached as the attribute
##' `"truth"`, for comparison against a fit
##'
##' @examples
##' ## 20 individuals differing markedly in movement magnitude
##' d <- sim_jcrw(A = 20, n = 200, D = 0.005, sd_m = 0.6, aniso = 2)
##' truth <- attr(d, "truth")
##' round(truth$D, 3)
##'
##' @importFrom stats rnorm rgamma rlnorm runif
##' @importFrom sf st_as_sf st_set_crs
##' @importFrom CircStats rvm
##'
##' @export

sim_jcrw <- function(A = 20,
                     n = 200,
                     dt = 6,
                     irregular = TRUE,
                     D = 0.005,
                     sd_m = 0.5,
                     aniso = 2,
                     orient = NULL,
                     error = c("LS", "KF"),
                     tau = c(1, 1),
                     rho_o = 0,
                     psi = 1,
                     lc.prob = c("3" = .05, "2" = .10, "1" = .15,
                                 "0" = .20, "A" = .20, "B" = .30),
                     crs = paste("+proj=laea +lat_0=-57 +lon_0=70",
                                 "+datum=WGS84 +units=km +no_defs"),
                     start = c(0, 0),
                     start.date = "2020-01-01 00:00:00") {

  error <- match.arg(error)

  if (A < 1) stop("A must be at least 1", call. = FALSE)
  if (length(n) == 1) n <- rep(n, A)
  if (length(n) != A) stop("n must be a scalar or of length A", call. = FALSE)
  if (any(n < 3)) stop("each track needs at least 3 locations", call. = FALSE)
  if (D <= 0) stop("D must be positive", call. = FALSE)
  if (sd_m < 0) stop("sd_m cannot be negative", call. = FALSE)
  if (length(aniso) == 1) aniso <- rep(aniso, A)
  if (length(aniso) != A)
    stop("aniso must be a scalar or of length A", call. = FALSE)
  if (any(aniso < 1))
    stop("aniso is a ratio of ellipse axes and cannot be less than 1",
         call. = FALSE)
  if (is.null(orient)) orient <- runif(A, -90, 90)
  if (length(orient) != A)
    stop("orient must be NULL or of length A", call. = FALSE)
  if (length(tau) == 1) tau <- rep(tau, 2)

  m_pop <- log(2 * D)
  m <- m_pop + rnorm(A, 0, sd_m)
  rho <- log(aniso) / 2
  th <- orient * pi / 180
  u1 <- rho * cos(2 * th)
  u2 <- rho * sin(2 * th)

  start.date <- as.POSIXct(start.date, tz = "UTC")

  out <- vector("list", A)
  states <- vector("list", A)
  Vs <- vector("list", A)

  for (i in seq_len(A)) {

    ni <- n[i]

    ## velocity innovation covariance, from the closed form for a symmetric
    ## traceless A: expm(A) = cosh(rho) I + sinh(rho)/rho A
    r <- rho[i]
    sh <- if (r < 1e-8) 1 else sinh(r) / r
    ch <- cosh(r)
    V <- exp(m[i]) * matrix(c(ch + sh * u1[i], sh * u2[i],
                              sh * u2[i], ch - sh * u1[i]), 2, 2)
    Vs[[i]] <- V
    L <- chol(V)          # V = t(L) %*% L

    dti <- if (irregular) rgamma(ni, shape = 2, scale = dt / 2) else
      rep(dt, ni)
    dti[1] <- 0
    dti <- pmax(dti, 1e-3)

    v <- matrix(0, 2, ni)
    mu <- matrix(0, 2, ni)
    mu[, 1] <- start

    for (t in 2:ni) {
      ## innovation covariance is dt * V
      v[, t] <- v[, t - 1] + sqrt(dti[t]) * as.vector(t(L) %*% rnorm(2))
      mu[, t] <- mu[, t - 1] + v[, t] * dti[t]
    }

    dte <- start.date + cumsum(dti) * 3600
    lc <- sample(names(lc.prob), ni, replace = TRUE, prob = lc.prob)

    if (error == "LS") {
      E <- emf()
      ex <- E$emf.x[match(lc, E$lc)]
      ey <- E$emf.y[match(lc, E$lc)]
      z1 <- rnorm(ni)
      z2 <- rho_o * z1 + sqrt(1 - rho_o ^ 2) * rnorm(ni)
      x.err <- tau[1] * ex * z1
      y.err <- tau[2] * ey * z2
      smaj <- smin <- eor <- NA_real_

    } else {
      ## ellipse parameters from the distributions fitted to real Argos data
      load(system.file("extdata", "ellps_tab.rda", package = "aniMotum"))
      k <- match(lc, ellps.tab$lc)
      smaj <- rlnorm(ni, ellps.tab$smaj.mn[k], ellps.tab$smaj.sd[k])
      smin <- rlnorm(ni, ellps.tab$smin.mn[k], ellps.tab$smin.sd[k])
      smin <- pmin(smin, smaj)
      eor <- sapply(k, function(j)
        rvm(1, ellps.tab$eor.mn[j], ellps.tab$eor.conc[j]) * 180 / pi)

      ## errors in km, with eor converted to radians as the model expects
      e <- eor * pi / 180
      M2 <- (smaj / sqrt(2) / 1000) ^ 2
      m2 <- (smin * psi / sqrt(2) / 1000) ^ 2
      s11 <- M2 * sin(e) ^ 2 + m2 * cos(e) ^ 2
      s22 <- M2 * cos(e) ^ 2 + m2 * sin(e) ^ 2
      s12 <- (M2 - m2) * cos(e) * sin(e)

      z1 <- rnorm(ni)
      z2 <- rnorm(ni)
      ## Cholesky of a 2x2, written out
      l11 <- sqrt(s11)
      l21 <- s12 / l11
      l22 <- sqrt(pmax(1e-12, s22 - l21 ^ 2))
      x.err <- l11 * z1
      y.err <- l21 * z1 + l22 * z2
    }

    out[[i]] <- data.frame(
      id = paste0("sim", formatC(i, width = 2, flag = "0")),
      date = dte,
      lc = lc,
      x = mu[1, ] + x.err,
      y = mu[2, ] + y.err,
      smaj = smaj,
      smin = smin,
      eor = eor,
      stringsAsFactors = FALSE
    )

    states[[i]] <- list(mu = mu, v = v, dt = dti)
  }

  d <- do.call(rbind, out)
  if (error == "LS") d <- d[, c("id", "date", "lc", "x", "y")]

  d <- st_as_sf(d, coords = c("x", "y"))
  d <- st_set_crs(d, crs)

  attr(d, "truth") <- list(
    D_pop = D, m_pop = m_pop, sd_m = sd_m,
    m = m, D = exp(m) / 2,
    aniso = aniso, orient = orient,
    u1 = u1, u2 = u2, V = Vs,
    tau = tau, rho_o = rho_o, psi = psi,
    error = error, crs = crs, states = states
  )

  d
}
