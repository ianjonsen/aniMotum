##' @title Joint (hierarchical) move persistence state-space model
##' negative log-likelihood
##'
##' @description Returns the RTMB objective function for a time-varying move
##' persistence model fitted jointly to multiple individual tracks, with
##' parameters shared among individuals according to a
##' [aniMotum::share_control] specification.
##'
##' @details This is the R-language counterpart of `src/TMB/sub/mp.hpp`,
##' extended across individuals. The process and observation models are
##' identical to the single-individual template, including the `gap_flag` and
##' `ho_flag` handling.
##'
##' The parameter that matters most here is `sigma_g`, the scale of the random
##' walk on `logit(gamma)`. It is pooled across individuals by default, for the
##' reason set out in Jonsen et al. (2019) and already embodied in
##' `joint_mpm.hpp`: with an individual-specific `sigma_g`, each animal's
##' `g_t` series is smoothed on its own scale and the series are not comparable
##' between animals. Comparability is usually the entire reason for fitting a
##' move persistence model to a deployment.
##'
##' As in [aniMotum::jcrw_nll] (now superseded), the process covariance is a
##' scalar multiple of a matrix that is constant within an individual, so it is
##' inverted analytically once per individual rather than factorised at every
##' time step: for a scalar `s`, `det(sC) = s^2 det(C)` and `(sC)^-1 = C^-1 / s`.
##' The observation likelihood is evaluated in three vectorised blocks.
##'
##' @param dat a named list of model data assembled by [aniMotum::jsfilter]
##'
##' @return a function of the parameter list, suitable for `RTMB::MakeADFun`
##'
##' @references Jonsen ID, McMahon CR, Patterson TA, et al. (2019) Movement
##' responses to environment: fast inference of variation among southern
##' elephant seals with a mixed effects model. Ecology 100(1):e02566
##'
##' @keywords internal

jmp_nll <- function(dat) {

  function(pars) {

    dnorm <- RTMB::dnorm
    RTMB::getAll(pars, dat, warn = FALSE)

    jnll <- 0

    ## ------------------------------------------------------------------
    ## Individual process innovation scales.
    ##
    ## sig_mode: 0 = pooled
    ##           1 = hierarchical, one scalar deviation per individual applied
    ##               to both coordinates. This is the rotation-invariant part
    ##               of the process covariance: scaling both coordinates
    ##               equally does not depend on the animal's direction of
    ##               travel, so the estimated among-individual variance is a
    ##               statement about movement magnitude rather than heading.
    ##               The sigma_x : sigma_y ratio is held common, which is the
    ##               residual frame-dependent assumption in this model.
    ##           2 = hierarchical, independent deviations per coordinate
    ##           3 = individual, fixed effects with no population distribution
    ## ------------------------------------------------------------------
    if (sig_mode == 0L) {
      lsx <- lsig_pop[1] + zeroA
      lsy <- lsig_pop[2] + zeroA

    } else if (sig_mode == 1L) {
      sd_lsig <- exp(l_sd_lsig[1])
      jnll <- jnll - sum(dnorm(lsig_re, 0, sd_lsig, log = TRUE))
      lsx <- lsig_pop[1] + lsig_re
      lsy <- lsig_pop[2] + lsig_re

    } else if (sig_mode == 2L) {
      sd_lsig <- exp(l_sd_lsig)
      jnll <- jnll - sum(dnorm(lsig_re[1:A], 0, sd_lsig[1], log = TRUE))
      jnll <- jnll - sum(dnorm(lsig_re[(A + 1):(2 * A)], 0, sd_lsig[2],
                               log = TRUE))
      lsx <- lsig_pop[1] + lsig_re[1:A]
      lsy <- lsig_pop[2] + lsig_re[(A + 1):(2 * A)]

    } else {
      lsx <- lsig_pop[1] + lsig_re[1:A]
      lsy <- lsig_pop[2] + lsig_re[(A + 1):(2 * A)]
    }

    sigma_x <- exp(lsx)
    sigma_y <- exp(lsy)

    ## process error correlation, indexed to individuals. Individual by
    ## default: rho_p sets the tilt of the process covariance ellipse relative
    ## to the projected axes, so it records the animal's direction of travel
    ## rather than a property of its movement, and animals on different
    ## bearings are not exchangeable in it.
    rho_p <- 2 / (1 + exp(-l_rho_p)) - 1
    rp <- rho_p[g_rho_p]

    ## move persistence random walk scale, pooled by default
    sigma_g <- exp(l_sigma_g)
    sg <- sigma_g[g_sg]

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

      sx <- sigma_x[i]
      sy <- sigma_y[i]
      r <- rp[i]
      om <- 1 - r * r
      lsxy <- log(sx) + log(sy) + 0.5 * log(om)

      if (k2 > k1) {

        ## random walk on logit(gamma). The innovation standard deviation is
        ## dt * sigma_g, matching mp.hpp, so gamma is free to drift across long
        ## gaps and haulout periods without further intervention.
        kk <- (k1 + 1):k2
        kp <- k1:(k2 - 1)
        jnll <- jnll - sum(dnorm(lg[kk], lg[kp], dt[kk] * sg[i], log = TRUE))

        ## first location step is a pure random walk: there is no previous
        ## displacement to condition on, and the covariance is not dt-scaled
        ax <- (X[1, k1 + 1] - X[1, k1]) / sx
        ay <- (X[2, k1 + 1] - X[2, k1]) / sy
        jnll <- jnll + log(2 * pi) + lsxy +
          0.5 * (ax * ax - 2 * r * ax * ay + ay * ay) / om
      }

      if (k2 >= k1 + 2) {

        kk <- (k1 + 2):k2
        km1 <- kk - 1L
        km2 <- kk - 2L

        dti <- dt[kk]
        dtp <- dt[km1]
        gf <- gap_flag[kk]
        hf <- ho_flag[kk]

        ## A data gap or a haulout breaks directional persistence, zeroing the
        ## correlated first-difference term. gap_flag takes precedence and
        ## ho_flag has already been cleared wherever gap_flag is set.
        brk <- pmax(gf, hf)
        gi <- 1 / (1 + exp(-lg[kk]))
        fac <- (1 - brk) * gi * (dti / dtp)

        ex <- X[1, kk] - X[1, km1] - fac * (X[1, km1] - X[1, km2])
        ey <- X[2, kk] - X[2, km1] - fac * (X[2, km1] - X[2, km2])

        ## covariance is s * C with s = dt^2, tightened by ho_scale during
        ## haulout. C is constant within an individual, so it is inverted once
        ## above rather than factorised at every step.
        s <- dti * dti * (1 - hf + hf * hos)

        zx <- ex / sx
        zy <- ey / sy
        q <- (zx * zx - 2 * r * zx * zy + zy * zy) / om

        jnll <- jnll + sum(log(2 * pi) + log(s) + lsxy + 0.5 * q / s)
      }
    }

    ## ------------------------------------------------------------------
    ## Observation model
    ## ------------------------------------------------------------------
    tau_x <- exp(l_tau[1:G_tau])
    tau_y <- exp(l_tau[(G_tau + 1):(2 * G_tau)])
    psi <- exp(l_psi)
    rho_o <- 2 / (1 + exp(-l_rho_o)) - 1

    if (n_ls > 0L) {
      s <- tau_x[r_tau[i_ls]] * K[i_ls, 1]
      q <- tau_y[r_tau[i_ls]] * K[i_ls, 2]
      r <- rho_o[r_rho_o[i_ls]]
      om <- 1 - r * r
      zx <- (Y[1, i_ls] - X[1, i_ls]) / s
      zy <- (Y[2, i_ls] - X[2, i_ls]) / q
      jnll <- jnll + sum(log(2 * pi) + log(s) + log(q) + 0.5 * log(om) +
                           0.5 * (zx * zx - 2 * r * zx * zy + zy * zy) / om)
    }

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
      ex <- Y[1, i_kf] - X[1, i_kf]
      ey <- Y[2, i_kf] - X[2, i_kf]
      qf <- (c22 * ex * ex - 2 * c12 * ex * ey + c11 * ey * ey) / detO
      jnll <- jnll + sum(log(2 * pi) + 0.5 * log(detO) + 0.5 * qf)
    }

    if (n_gl > 0L) {
      s <- GLerr[i_gl, 1]
      q <- GLerr[i_gl, 2]
      r <- rho_o[r_rho_o[i_gl]]
      om <- 1 - r * r
      zx <- (Y[1, i_gl] - X[1, i_gl]) / s
      zy <- (Y[2, i_gl] - X[2, i_gl]) / q
      jnll <- jnll + sum(log(2 * pi) + log(s) + log(q) + 0.5 * log(om) +
                           0.5 * (zx * zx - 2 * r * zx * zy + zy * zy) / om)
    }

    ## ------------------------------------------------------------------
    ## Reporting
    ## ------------------------------------------------------------------
    sigma_pop <- exp(lsig_pop)
    RTMB::ADREPORT(sigma_pop)
    RTMB::ADREPORT(sigma_x)
    RTMB::ADREPORT(sigma_y)
    RTMB::ADREPORT(sigma_g)
    RTMB::ADREPORT(tau_x)
    RTMB::ADREPORT(tau_y)
    RTMB::ADREPORT(psi)
    RTMB::ADREPORT(rho_p)
    RTMB::ADREPORT(rho_o)

    if (sig_mode == 1L || sig_mode == 2L) RTMB::ADREPORT(sd_lsig)
    if (est_ho == 1L) RTMB::ADREPORT(hos)

    RTMB::REPORT(lsig_re)
    RTMB::REPORT(sigma_x)
    RTMB::REPORT(sigma_y)

    jnll
  }
}
