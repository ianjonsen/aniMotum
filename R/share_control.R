##' @title Specify which parameters are shared among individuals
##'
##' @description `share_control` selects how each parameter of a joint
##' (hierarchical) state-space model is treated across individual tracks, and
##' guards against parameter-sharing combinations that are not identifiable.
##'
##' Each parameter may be:
##' \describe{
##'   \item{`"pooled"`}{a single value estimated from all individuals at once.}
##'   \item{`"hierarchical"`}{an individual-level value drawn from an estimated
##'   population distribution, so individual estimates are partially pooled
##'   toward the population mean.}
##'   \item{`"individual"`}{a separate, independent value per individual, as in
##'   [aniMotum::fit_ssm].}
##' }
##'
##' @details The defaults implement the recommended structure for the joint
##' move persistence model: a pooled measurement model, a pooled `sigma_g`, a
##' hierarchical process innovation scale, and an individual `rho_p`.
##'
##' \strong{Measurement model.} `tau`, `psi` and `rho_o` describe the
##' observation system rather than the animal. Argos error is a property of
##' satellite geometry, transmitter power, antenna and surfacing behaviour, and
##' is shared by animals carrying comparable tags, so estimating these
##' separately per individual estimates one quantity many times using a
##' fraction of the data each time. `psi` is the clearest case: it scales only
##' the semi-minor axis of the Argos error ellipse, so it is informed solely by
##' the anisotropy of the observation residuals and is weakly identified from a
##' single track. Note that `tau` scales fixed per-`lc` variances taken from
##' independent data (see [aniMotum::emf]) rather than estimating the error
##' from scratch, so a pooled `tau` far from 1 is a statement about how well
##' those independent values are calibrated for the deployment at hand.
##'
##' \strong{`sigma_g` is pooled, and this is the point of the model.} With an
##' individual-specific `sigma_g`, each animal's `g_t` series is smoothed on
##' its own scale and the series cannot be compared between animals.
##' Comparability is usually the entire reason for fitting a move persistence
##' model to a deployment. This follows Jonsen et al. (2019) and the existing
##' `jmpm` model in [aniMotum::fit_mpm]. `sigma_g` is also rotation-invariant:
##' it scales a random walk on a scalar, so it does not depend on which way the
##' animal travelled.
##'
##' \strong{`rho_p` is individual by default, and this is deliberate.} `rho_p`
##' is the correlation between the x and y process innovations, which sets the
##' tilt of the process covariance ellipse relative to the projected
##' coordinate axes. It therefore records the animal's direction of travel
##' rather than a property of its movement. Animals heading in different
##' directions are not exchangeable in `rho_p`, and pooling it averages
##' correlations that may differ in sign, producing an estimate near zero that
##' reflects nothing but the mixture. Pool it only when all individuals travel
##' broadly the same way.
##'
##' \strong{The residual frame-dependence.} `hier.sigma = "shared"` gives each
##' individual one scalar deviation applied to both coordinates, so an
##' individual's process covariance is a scalar multiple of the population one.
##' A scalar multiple is rotation-invariant, so the estimated among-individual
##' variance is a statement about movement magnitude rather than heading. What
##' it does hold common is the `sigma_x` : `sigma_y` ratio, which is
##' frame-dependent. That is a known approximation, and a fully
##' rotation-invariant parameterisation - overall magnitude, anisotropy ratio,
##' and orientation, with orientation always individual - is the intended
##' replacement.
##'
##' \strong{Identifiability.} The central tension in a state-space model is the
##' partitioning of total variance between the process and the measurement
##' model. Random effects on both sides let the individual-level deviations
##' trade off against each other at almost no cost in likelihood, leaving the
##' variance hyperparameters weakly identified and, with short tracks,
##' effectively unidentified. `share_control` therefore refuses to make `sigma`
##' hierarchical at the same time as `tau` or `psi`. Set `strict = FALSE` to
##' downgrade this to a warning.
##'
##' \strong{Grouping.} Pooling the measurement parameters assumes a homogeneous
##' observation process. Mixed tag models, different duty cycles, or different
##' species all violate that. Use `group` to name a variable in the input data
##' defining strata within which the measurement parameters are pooled.
##'
##' @param sigma treatment of the process innovation scale. One of
##' `"hierarchical"` (default), `"pooled"` or `"individual"`
##' @param sigma_g treatment of the move persistence random walk scale. One of
##' `"pooled"` (default) or `"individual"`. Pooling is what makes `g_t`
##' comparable among individuals
##' @param rho_p treatment of the process error correlation between x and y.
##' One of `"individual"` (default) or `"pooled"`
##' @param tau treatment of the LS/GPS measurement error dispersion. One of
##' `"pooled"` (default), `"individual"` or `"hierarchical"`
##' @param psi treatment of the Argos error ellipse semi-minor axis scaling.
##' One of `"pooled"` (default), `"individual"` or `"hierarchical"`
##' @param rho_o treatment of the measurement error correlation between x and
##' y. One of `"pooled"` (default) or `"individual"`
##' @param hier.sigma structure of the individual random effect on `sigma` when
##' `sigma = "hierarchical"`. `"shared"` (default) gives one scalar deviation
##' per individual applied to both coordinates; `"xy"` gives independent
##' deviations per coordinate. `"shared"` is the default because it is the
##' rotation-invariant choice
##' @param ho_scale treatment of the haulout process variance scale factor.
##' `"fixed"` (default) uses the constant from [aniMotum::ssm_control];
##' `"pooled"` estimates it. Estimating it is only possible in a joint fit: it
##' is not identifiable from one track
##' @param group name of a variable in the input data defining strata within
##' which the measurement parameters are pooled, or NULL (default)
##' @param strict logical; if TRUE (default), reject parameter combinations
##' that are not identifiable. If FALSE, warn instead
##'
##' @return a named list of parameter sharing settings
##'
##' @references Jonsen ID, McMahon CR, Patterson TA, et al. (2019) Movement
##' responses to environment: fast inference of variation among southern
##' elephant seals with a mixed effects model. Ecology 100(1):e02566
##'
##' @examples
##' ## the recommended defaults
##' share_control()
##'
##' ## pool the measurement model within tag type
##' share_control(group = "tag_type")
##'
##' ## fully pooled process scale, for too few individuals to estimate
##' ## among-individual variance
##' share_control(sigma = "pooled")
##'
##' @export

share_control <- function(sigma = c("hierarchical", "pooled", "individual"),
                          sigma_g = c("pooled", "individual"),
                          rho_p = c("individual", "pooled"),
                          tau = c("pooled", "individual", "hierarchical"),
                          psi = c("pooled", "individual", "hierarchical"),
                          rho_o = c("pooled", "individual"),
                          hier.sigma = c("shared", "xy"),
                          ho_scale = c("fixed", "pooled"),
                          group = NULL,
                          strict = TRUE) {

  sigma <- match.arg(sigma)
  sigma_g <- match.arg(sigma_g)
  rho_p <- match.arg(rho_p)
  tau <- match.arg(tau)
  psi <- match.arg(psi)
  rho_o <- match.arg(rho_o)
  hier.sigma <- match.arg(hier.sigma)
  ho_scale <- match.arg(ho_scale)

  if (!is.logical(strict) || length(strict) != 1)
    stop("strict must be a single logical value", call. = FALSE)
  if (!is.null(group) && !(is.character(group) && length(group) == 1))
    stop("group must be NULL or the name of a single variable in the input data",
         call. = FALSE)

  ## Identifiability guard: random effects on both sides of the process /
  ## measurement variance partition.
  if (sigma == "hierarchical" &&
      (tau == "hierarchical" || psi == "hierarchical")) {
    msg <- paste0(
      "`sigma` and the measurement parameters cannot both be hierarchical.\n",
      "  A state-space model partitions total variance between the process\n",
      "  and the measurement model. Individual random effects on both sides\n",
      "  trade off against each other at almost no cost in likelihood, so the\n",
      "  variance hyperparameters are not identifiable - with short tracks the\n",
      "  likelihood surface is close to flat in that direction.\n",
      "  Keep `sigma` hierarchical and pool the measurement model (the\n",
      "  default), or pool `sigma` and let the measurement model vary."
    )
    if (strict) stop(msg, call. = FALSE)
    else warning(msg, call. = FALSE, immediate. = TRUE)
  }

  ## Pooling sigma_g is the reason for fitting a joint move persistence model
  if (sigma_g == "individual")
    warning("sigma_g = \"individual\" smooths each animal's g_t on its own\n",
            "  scale, so the resulting g_t series are not comparable between\n",
            "  individuals. That comparability is usually the reason for\n",
            "  fitting a joint move persistence model at all.",
            call. = FALSE, immediate. = TRUE)

  ## A fully individual specification is just independent per-individual fits
  if (all(c(sigma, sigma_g, rho_p, tau, psi, rho_o) == "individual"))
    warning("all parameters are specified as `individual`, which is equivalent\n",
            "  to fitting each track separately. Use `fit_ssm(model = \"mp\")`\n",
            "  instead, which is faster and parallelises across individuals.",
            call. = FALSE, immediate. = TRUE)

  list(sigma = sigma,
       sigma_g = sigma_g,
       rho_p = rho_p,
       tau = tau,
       psi = psi,
       rho_o = rho_o,
       hier.sigma = hier.sigma,
       ho_scale = ho_scale,
       group = group,
       strict = strict)
}
