##' @title Joint (hierarchical) correlated random walk with a rotation-invariant
##' process covariance
##'
##' @description Returns the RTMB objective function for a continuous-time
##' correlated random walk fitted jointly to multiple individual tracks, with
##' parameters shared among individuals according to a
##' [aniMotum::share_control] specification.
##'
##' @details This is the R-language counterpart of `src/TMB/sub/crw.hpp`,
##' extended across individuals and reparameterised so that the quantities
##' shared among individuals do not depend on which way each animal travelled.
##'
##' \strong{Why the reparameterisation.} `crw.hpp` describes the velocity
##' innovation covariance with `D_x`, `D_y` and `rho_p`, which are the axes and
##' tilt of an ellipse measured against the projected coordinate axes. All
##' three change when an animal travels a different bearing, even if its
##' movement is otherwise identical. Pooling them across a deployment therefore
##' averages quantities that mean different things for different animals, and
##' an among-individual variance estimated from them is partly a statement
##' about heading.
##'
##' \strong{The parameterisation.} The same covariance is written
##'
##' ```
##'   V = exp(m) * expm(A)        A = [ u1   u2 ]
##'                                   [ u2  -u1 ]
##' ```
##'
##' `A` is symmetric with zero trace, so `exp(A)` has determinant one and
##' `det(V) = exp(2m)` exactly. `A` has eigenvalues `+/- r`, where `r` is
##' \code{sqrt(u1^2 + u2^2)}, so `V` has eigenvalues `exp(m +/- r)` and its
##' major axis lies at `theta = atan2(u2, u1) / 2`. That gives
##'
##' \describe{
##'   \item{`m`}{overall movement magnitude, half the log determinant. Rotation
##'   invariant: rotate the coordinate frame and it does not move.}
##'   \item{`r`}{log anisotropy, half the log ratio of the ellipse axes. Also
##'   rotation invariant.}
##'   \item{`theta`}{orientation. This is the only frame-dependent piece, and
##'   it is never pooled.}
##' }
##'
##' The `2` in `atan2(u2, u1) / 2` is what makes `u` well behaved: an ellipse is
##' unchanged by a half turn, so orientation lives on a half circle, and using
##' `2 theta` maps that onto a full circle. `u` is then an ordinary unconstrained
##' vector with no wrapping to handle.
##'
##' Because `A` is traceless and symmetric, `exp(A) = cosh(r) I + sinh(r)/r A`,
##' so neither an eigendecomposition nor a matrix inverse is needed - the log
##' determinant is `2m` and the quadratic form is written out directly. That
##' keeps the AD tape small, which matters more in RTMB than in C++.
##'
##' @param dat a named list of model data assembled by [aniMotum::jsfilter]
##'
##' @return a function of the parameter list, suitable for `RTMB::MakeADFun`
##'
##' @keywords internal

jcrw_nll <- function(dat) {

  function(pars) {

    dnorm <- RTMB::dnorm
    RTMB::getAll(pars, dat, warn = FALSE)

    jnll <- 0
    tiny <- 1e-5
    sd_tiny <- sqrt(tiny)

    ## ------------------------------------------------------------------
    ## Movement magnitude, one value per individual.
    ##
    ## m_mode: 0 = pooled
    ##         1 = hierarchical
    ##         2 = individual, fixed effects with no population distribution
    ##
    ## m is half the log determinant of the velocity innovation covariance, so
    ## it does not depend on the animal's direction of travel. That is what
    ## makes an among-individual variance estimated from it a statement about
    ## how much the animals move rather than about where they went.
    ## ------------------------------------------------------------------
    if (m_mode == 0L) {
      m <- m_pop[1] + zeroA

    } else if (m_mode == 1L) {
      sd_m <- exp(l_sd_m[1])
      jnll <- jnll - sum(dnorm(m_re, 0, sd_m, log = TRUE))
      m <- m_pop[1] + m_re

    } else {
      m <- m_pop[1] + m_re
    }

    ## ------------------------------------------------------------------
    ## Anisotropy vector, one per individual.
    ##
    ## an_mode: 0 = isotropic, u fixed at zero. The ellipse is a circle, so
    ##              there is no orientation to estimate and the process is
    ##              described by m alone
    ##          1 = common ratio: the amount of anisotropy is shared while each
    ##              individual keeps its own orientation. This is the pooling
    ##              the reparameterisation exists to make possible - the shared
    ##              quantity is rotation invariant, the free one is not
    ##          2 = individual: amount and orientation both free per individual
    ## ------------------------------------------------------------------
    if (an_mode == 0L) {
      u1 <- zeroA
      u2 <- zeroA

    } else if (an_mode == 1L) {
      a <- exp(l_aniso[1])
      u1 <- a * cos(2 * theta)
      u2 <- a * sin(2 * theta)

    } else {
      u1 <- u_re[1:A]
      u2 <- u_re[(A + 1):(2 * A)]
    }

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

      ## the small constant keeps sqrt() and sinh(r)/r differentiable at u = 0;
      ## at 1e-10 it perturbs sinh(r)/r by about 1e-11
      r <- sqrt(u1[i] * u1[i] + u2[i] * u2[i] + 1e-10)
      ch <- cosh(r)
      sh <- sinh(r) / r
      emi <- exp(-m[i])

      ## initial location and velocity pinned to state0 with tiny variance
      jnll <- jnll - dnorm(mu[1, k1], state0[i, 1], sd_tiny, log = TRUE)
      jnll <- jnll - dnorm(mu[2, k1], state0[i, 2], sd_tiny, log = TRUE)
      jnll <- jnll - dnorm(v[1, k1], state0[i, 3], sd_tiny, log = TRUE)
      jnll <- jnll - dnorm(v[2, k1], state0[i, 4], sd_tiny, log = TRUE)

      if (k2 > k1) {

        kk <- (k1 + 1):k2
        kp <- k1:(k2 - 1)

        dti <- dt[kk]
        gf <- gap_flag[kk]
        hf <- ho_flag[kk]

        ## position innovations: variance tiny, independent across coordinates
        ex <- mu[1, kk] - (mu[1, kp] + v[1, kk] * dti)
        ey <- mu[2, kk] - (mu[2, kp] + v[2, kk] * dti)
        jnll <- jnll - sum(dnorm(ex, 0, sd_tiny, log = TRUE))
        jnll <- jnll - sum(dnorm(ey, 0, sd_tiny, log = TRUE))

        ## velocity innovations. A data gap or a haulout breaks directional
        ## persistence, so the marginal rather than the conditional
        ## distribution is used. gap_flag takes precedence and ho_flag has
        ## already been cleared wherever gap_flag is set.
        brk <- pmax(gf, hf)
        z1 <- v[1, kk] - (1 - brk) * v[1, kp]
        z2 <- v[2, kk] - (1 - brk) * v[2, kp]

        ## covariance is cs * V, tightened by ho_scale during haulout
        cs <- dti * (1 - hf + hf * hos)

        ## log det(cs * V) = 2 log(cs) + 2 m, and the quadratic form follows
        ## from exp(-A) = cosh(r) I - sinh(r)/r A, so nothing is inverted
        q <- emi * (ch * (z1 * z1 + z2 * z2) -
                      sh * (u1[i] * (z1 * z1 - z2 * z2) + 2 * u2[i] * z1 * z2))

        jnll <- jnll + sum(log(2 * pi) + log(cs) + m[i] + 0.5 * q / cs)
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
      qq <- tau_y[r_tau[i_ls]] * K[i_ls, 2]
      rr <- rho_o[r_rho_o[i_ls]]
      om <- 1 - rr * rr
      zx <- (Y[1, i_ls] - mu[1, i_ls]) / s
      zy <- (Y[2, i_ls] - mu[2, i_ls]) / qq
      jnll <- jnll + sum(log(2 * pi) + log(s) + log(qq) + 0.5 * log(om) +
                           0.5 * (zx * zx - 2 * rr * zx * zy + zy * zy) / om)
    }

    if (n_kf > 0L) {
      psi_i <- psi[r_psi[i_kf]]
      cc <- cos(c_eor[i_kf])
      sc <- sin(c_eor[i_kf])
      M2 <- (M[i_kf] / sqrt(2)) ^ 2
      m2 <- (m_ax[i_kf] * psi_i / sqrt(2)) ^ 2
      c11 <- M2 * sc * sc + m2 * cc * cc
      c22 <- M2 * cc * cc + m2 * sc * sc
      c12 <- (M2 - m2) * cc * sc
      detO <- M2 * m2
      ex <- Y[1, i_kf] - mu[1, i_kf]
      ey <- Y[2, i_kf] - mu[2, i_kf]
      qf <- (c22 * ex * ex - 2 * c12 * ex * ey + c11 * ey * ey) / detO
      jnll <- jnll + sum(log(2 * pi) + 0.5 * log(detO) + 0.5 * qf)
    }

    if (n_gl > 0L) {
      s <- GLerr[i_gl, 1]
      qq <- GLerr[i_gl, 2]
      rr <- rho_o[r_rho_o[i_gl]]
      om <- 1 - rr * rr
      zx <- (Y[1, i_gl] - mu[1, i_gl]) / s
      zy <- (Y[2, i_gl] - mu[2, i_gl]) / qq
      jnll <- jnll + sum(log(2 * pi) + log(s) + log(qq) + 0.5 * log(om) +
                           0.5 * (zx * zx - 2 * rr * zx * zy + zy * zy) / om)
    }

    ## ------------------------------------------------------------------
    ## Reporting
    ##
    ## The reported quantities are the interpretable ones. D is on the same
    ## scale as crw's D_x and D_y: for an isotropic process V = 2 D I, so
    ## D = exp(m)/2 is the geometric mean diffusion coefficient. aniso is the
    ## ratio of the ellipse axes, and orient is in degrees anticlockwise from
    ## the projection's x axis - the only reported quantity that depends on the
    ## coordinate frame, and so the only one not comparable between fits made
    ## in different projections.
    ## ------------------------------------------------------------------
    rr_i <- sqrt(u1 * u1 + u2 * u2 + 1e-10)
    D <- exp(m) / 2
    D_pop <- exp(m_pop[1]) / 2
    aniso <- exp(2 * rr_i)

    RTMB::ADREPORT(D_pop)
    RTMB::ADREPORT(D)
    RTMB::ADREPORT(tau_x)
    RTMB::ADREPORT(tau_y)
    RTMB::ADREPORT(psi)
    RTMB::ADREPORT(rho_o)

    if (m_mode == 1L) RTMB::ADREPORT(sd_m)
    if (an_mode > 0L) {
      RTMB::ADREPORT(aniso)
      ## RTMB has no atan2() method for advectors, so the orientation is only
      ## ADREPORTed where it is already a parameter in its own right. Under
      ## aniso = "common.ratio" that is exactly what theta is, and its standard
      ## error comes for free. With a free anisotropy vector the orientation is
      ## recovered from u1 and u2 after the fit, in jsfilter(), without one.
      if (an_mode == 1L) {
        orient <- theta * 180 / pi
        RTMB::ADREPORT(orient)
      }
    }
    if (est_ho == 1L) RTMB::ADREPORT(hos)

    RTMB::REPORT(m)
    RTMB::REPORT(u1)
    RTMB::REPORT(u2)
    RTMB::REPORT(D)
    RTMB::REPORT(aniso)

    jnll
  }
}
