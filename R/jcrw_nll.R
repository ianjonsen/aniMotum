##' @title Joint (hierarchical) continuous-time correlated random walk
##' negative log-likelihood
##'
##' @description Returns the RTMB objective function for a correlated random
##' walk fitted jointly to multiple individual tracks, with parameters shared
##' among individuals according to a [aniMotum::share_control] specification.
##'
##' @details This is the R-language counterpart of `src/TMB/sub/crw.hpp`,
##' extended across individuals. The process and observation models are
##' identical to the single-individual template, including the `gap_flag` and
##' `ho_flag` handling, so a joint fit with all parameters specified as
##' `individual` reduces to a set of independent `crw` fits.
##'
##' Two departures from the C++ template are deliberate.
##'
##' First, the velocity innovation covariance is `dt` times a matrix that is
##' constant within an individual. `crw.hpp` calls `setSigma()` on a 4 x 4
##' covariance at every time step, which factorises afresh each time and records
##' that factorisation on the AD tape. Here the 2 x 2 velocity block is
##' inverted analytically once per individual and the `dt` scaling is applied to
##' the quadratic form and the log-determinant, which is exact: for a scalar
##' `s`, `det(sV) = s^2 det(V)` and `(sV)^-1 = V^-1 / s`. The position block is
##' diagonal with variance `tiny`, so it separates into two univariate normal
##' densities. The whole process likelihood therefore reduces to vector
##' arithmetic, which matters more in RTMB than in C++ because tape size drives
##' taping cost.
##'
##' Second, the observation likelihood is evaluated in three vectorised blocks
##' (LS/GPS, Argos error ellipse, generic location) rather than one row at a
##' time. The error ellipse determinant is `M2 * m2` exactly, because the
##' ellipse covariance is a rotation of `diag(M2, m2)` and rotation preserves
##' the determinant. Using that identity avoids both a matrix factorisation and
##' the cancellation that arises from forming `c11 * c22 - c12^2` when the
##' ellipse is highly eccentric.
##'
##' @param dat a named list of model data assembled by [aniMotum::jsfilter]
##'
##' @return a function of the parameter list, suitable for `RTMB::MakeADFun`
##'
##' @keywords internal

jcrw_nll <- function(dat) {

  function(pars) {

    ## RTMB's distribution and reporting functions are used explicitly rather
    ## than by attaching the package, so that aniMotum does not depend on RTMB
    ## being on the search path.
    dnorm <- RTMB::dnorm
    RTMB::getAll(pars, dat, warn = FALSE)

    ## Note: RTMB::OBS() is deliberately NOT applied to Y. OBS() exists to hook
    ## the one-step-ahead residual and simulation machinery onto a density
    ## call, and the observation densities below are written out by hand rather
    ## than as RTMB density functions, so there is nothing for it to attach to.
    ## Marking Y without a matching density call gains nothing and wraps the
    ## observations in an object whose derivatives do not propagate cleanly.

    jnll <- 0
    tiny <- 1e-5
    sd_tiny <- sqrt(tiny)

    ## ------------------------------------------------------------------
    ## Individual diffusion coefficients
    ##
    ## D_mode: 0 = pooled (one value for all individuals)
    ##         1 = hierarchical, a single scalar deviation per individual
    ##             applied to both coordinates, so individuals differ in
    ##             overall diffusion magnitude but share the population x,y
    ##             anisotropy
    ##         2 = hierarchical, independent deviations per coordinate
    ##         3 = individual, deviations estimated as fixed effects with no
    ##             population distribution
    ## ------------------------------------------------------------------
    if (D_mode == 0L) {
      lD1 <- lD_pop[1] + zeroA
      lD2 <- lD_pop[2] + zeroA

    } else if (D_mode == 1L) {
      sd_lD <- exp(l_sd_lD[1])
      jnll <- jnll - sum(dnorm(lD_re, 0, sd_lD, log = TRUE))
      lD1 <- lD_pop[1] + lD_re
      lD2 <- lD_pop[2] + lD_re

    } else if (D_mode == 2L) {
      sd_lD <- exp(l_sd_lD)
      jnll <- jnll - sum(dnorm(lD_re[1:A], 0, sd_lD[1], log = TRUE))
      jnll <- jnll - sum(dnorm(lD_re[(A + 1):(2 * A)], 0, sd_lD[2], log = TRUE))
      lD1 <- lD_pop[1] + lD_re[1:A]
      lD2 <- lD_pop[2] + lD_re[(A + 1):(2 * A)]

    } else {
      lD1 <- lD_pop[1] + lD_re[1:A]
      lD2 <- lD_pop[2] + lD_re[(A + 1):(2 * A)]
    }

    D1 <- exp(lD1)
    D2 <- exp(lD2)

    ## process error correlation, indexed to individuals
    rho_p <- 2 / (1 + exp(-l_rho_p)) - 1
    rp <- rho_p[g_rho_p]

    ## haulout process variance scale factor: either the constant supplied by
    ## ssm_control(), or a single parameter pooled across all individuals.
    ## Pooling is what makes it estimable at all - it is not identifiable from
    ## a single track, which is why the individual filters assert it.
    if (est_ho == 1L) {
      hos <- 1 / (1 + exp(-l_ho_scale[1]))
    } else {
      hos <- ho_scale_fixed
    }

    ## ------------------------------------------------------------------
    ## Process model
    ## ------------------------------------------------------------------
    for (i in 1:A) {

      k1 <- i1[i]
      k2 <- i2[i]

      ## initial location and velocity pinned to state0 with tiny variance
      jnll <- jnll - dnorm(mu[1, k1], state0[i, 1], sd_tiny, log = TRUE)
      jnll <- jnll - dnorm(mu[2, k1], state0[i, 2], sd_tiny, log = TRUE)
      jnll <- jnll - dnorm(v[1, k1], state0[i, 3], sd_tiny, log = TRUE)
      jnll <- jnll - dnorm(v[2, k1], state0[i, 4], sd_tiny, log = TRUE)

      if (k2 > k1) {

        kk <- (k1 + 1):k2
        kp <- k1:(k2 - 1)

        ## velocity innovation covariance = cs * V, V constant within individual
        V11 <- 2 * D1[i]
        V22 <- 2 * D2[i]
        V12 <- 2 * sqrt(D1[i] * D2[i]) * rp[i]
        detV <- V11 * V22 - V12 * V12

        dti <- dt[kk]
        gf <- gap_flag[kk]
        hf <- ho_flag[kk]

        ## position innovations, variance tiny, independent across coordinates
        ex <- mu[1, kk] - (mu[1, kp] + v[1, kk] * dti)
        ey <- mu[2, kk] - (mu[2, kp] + v[2, kk] * dti)
        jnll <- jnll - sum(dnorm(ex, 0, sd_tiny, log = TRUE))
        jnll <- jnll - sum(dnorm(ey, 0, sd_tiny, log = TRUE))

        ## velocity innovations. Directional persistence is broken by a data
        ## gap or by a haulout, in which case the marginal rather than the
        ## conditional distribution is used. gap_flag takes precedence and
        ## ho_flag has already been cleared wherever gap_flag is set.
        brk <- pmax(gf, hf)
        a <- v[1, kk] - (1 - brk) * v[1, kp]
        b <- v[2, kk] - (1 - brk) * v[2, kp]

        ## dt scaling, tightened by ho_scale during haulout
        cs <- dti * (1 - hf + hf * hos)

        qf <- (a * a * V22 - 2 * a * b * V12 + b * b * V11) / detV
        jnll <- jnll + sum(log(2 * pi) + log(cs) +
                             0.5 * log(detV) + 0.5 * qf / cs)
      }
    }

    ## ------------------------------------------------------------------
    ## Observation model
    ##
    ## Measurement parameters are indexed by group, so a single pooled value
    ## and a separate value per individual are the same code path with a
    ## different index vector.
    ## ------------------------------------------------------------------
    tau_x <- exp(l_tau[1:G_tau])
    tau_y <- exp(l_tau[(G_tau + 1):(2 * G_tau)])
    psi <- exp(l_psi)
    rho_o <- 2 / (1 + exp(-l_rho_o)) - 1

    ## Argos Least Squares and GPS observations
    if (n_ls > 0L) {
      s <- tau_x[r_tau[i_ls]] * K[i_ls, 1]
      q <- tau_y[r_tau[i_ls]] * K[i_ls, 2]
      r <- rho_o[r_rho_o[i_ls]]
      om <- 1 - r * r
      zx <- (Y[1, i_ls] - mu[1, i_ls]) / s
      zy <- (Y[2, i_ls] - mu[2, i_ls]) / q
      jnll <- jnll + sum(log(2 * pi) + log(s) + log(q) + 0.5 * log(om) +
                           0.5 * (zx * zx - 2 * r * zx * zy + zy * zy) / om)
    }

    ## Argos Kalman Filter / Smoother observations (error ellipses)
    if (n_kf > 0L) {
      psi_i <- psi[r_psi[i_kf]]
      cc <- cos(c_eor[i_kf])
      sc <- sin(c_eor[i_kf])
      M2 <- (M[i_kf] / sqrt(2)) ^ 2
      m2 <- (m[i_kf] * psi_i / sqrt(2)) ^ 2
      c11 <- M2 * sc * sc + m2 * cc * cc
      c22 <- M2 * cc * cc + m2 * sc * sc
      c12 <- (M2 - m2) * cc * sc
      ## the ellipse covariance is a rotation of diag(M2, m2), so its
      ## determinant is exactly M2 * m2
      detO <- M2 * m2
      ex <- Y[1, i_kf] - mu[1, i_kf]
      ey <- Y[2, i_kf] - mu[2, i_kf]
      qf <- (c22 * ex * ex - 2 * c12 * ex * ey + c11 * ey * ey) / detO
      jnll <- jnll + sum(log(2 * pi) + 0.5 * log(detO) + 0.5 * qf)
    }

    ## Generic Location observations (supplied standard errors)
    if (n_gl > 0L) {
      s <- GLerr[i_gl, 1]
      q <- GLerr[i_gl, 2]
      r <- rho_o[r_rho_o[i_gl]]
      om <- 1 - r * r
      zx <- (Y[1, i_gl] - mu[1, i_gl]) / s
      zy <- (Y[2, i_gl] - mu[2, i_gl]) / q
      jnll <- jnll + sum(log(2 * pi) + log(s) + log(q) + 0.5 * log(om) +
                           0.5 * (zx * zx - 2 * r * zx * zy + zy * zy) / om)
    }

    ## ------------------------------------------------------------------
    ## Reporting
    ## ------------------------------------------------------------------
    D_pop <- exp(lD_pop)
    RTMB::ADREPORT(D_pop)
    RTMB::ADREPORT(D1)
    RTMB::ADREPORT(D2)
    RTMB::ADREPORT(tau_x)
    RTMB::ADREPORT(tau_y)
    RTMB::ADREPORT(psi)
    RTMB::ADREPORT(rho_p)
    RTMB::ADREPORT(rho_o)

    if (D_mode == 1L || D_mode == 2L) RTMB::ADREPORT(sd_lD)
    if (est_ho == 1L) RTMB::ADREPORT(hos)

    ## individual deviations are reported so they can be checked for structure
    ## (clustering by deployment, site or year) before the among-individual
    ## variance is interpreted as exchangeable biological variation
    RTMB::REPORT(lD_re)
    RTMB::REPORT(D1)
    RTMB::REPORT(D2)

    jnll
  }
}
