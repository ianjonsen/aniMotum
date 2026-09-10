##' @title Specify which parameters are shared among individuals
##'
##' @description `share_control` selects how each parameter of a joint
##' (hierarchical) state-space model is treated across individual tracks, and
##' guards against the parameter-sharing combinations that are not identifiable.
##'
##' Each parameter may be:
##' \describe{
##'   \item{`"pooled"`}{a single value estimated from all individuals at once.}
##'   \item{`"hierarchical"`}{an individual-level value drawn from an estimated
##'   population distribution, so that individual estimates are partially pooled
##'   (shrunk) toward the population mean.}
##'   \item{`"individual"`}{a separate, independent value per individual, as in
##'   [aniMotum::fit_ssm].}
##' }
##'
##' @details The defaults implement the recommended structure: the measurement
##' model is pooled and the process model is hierarchical.
##'
##' The measurement parameters `tau`, `psi` and `rho_o` describe the observation
##' system rather than the animal. Argos error is a property of satellite
##' geometry, transmitter power, antenna and surfacing behaviour, and is shared
##' by animals carrying comparable tags. Estimating these separately for each
##' individual estimates one quantity many times using a fraction of the data
##' each time. `psi` is the clearest case: it scales only the semi-minor axis of
##' the Argos error ellipse, so it is informed solely by the anisotropy of the
##' observation residuals and is weakly identified from a single track.
##'
##' The diffusion coefficient `D` is the parameter with a real biological reason
##' to differ among individuals, since animals differ in how fast and how
##' tortuously they move, and so is hierarchical by default.
##'
##' \strong{Identifiability.} The central tension in a state-space model is the
##' partitioning of total variance between the process and the measurement
##' model. Placing random effects on both sides of that partition allows the
##' individual-level process and measurement deviations to trade off against
##' each other at almost no cost in likelihood, leaving the corresponding
##' variance hyperparameters weakly identified, and with short tracks
##' effectively unidentified. `share_control` therefore refuses to specify `D`
##' as hierarchical at the same time as `tau` or `psi`. Set `strict = FALSE` to
##' downgrade this to a warning if you want to explore the behaviour
##' deliberately, but expect the optimiser to struggle and the hyperparameter
##' standard errors to be uninformative.
##'
##' \strong{Grouping.} Pooling the measurement parameters assumes a homogeneous
##' observation process. Mixed tag models, different duty cycles, or different
##' species (a deep-diving seal and a surface-basking turtle produce different
##' effective Argos error) all violate that. Use `group` to name a variable in
##' the input data that defines strata within which the measurement parameters
##' are pooled; the default pools across all individuals.
##'
##' @param D treatment of the diffusion coefficient. One of `"hierarchical"`
##' (default), `"pooled"` or `"individual"`
##' @param tau treatment of the LS/GPS measurement error dispersion. One of
##' `"pooled"` (default), `"individual"` or `"hierarchical"`
##' @param psi treatment of the Argos error ellipse semi-minor axis scaling.
##' One of `"pooled"` (default), `"individual"` or `"hierarchical"`
##' @param rho_p treatment of the process error correlation between x and y.
##' One of `"pooled"` (default) or `"individual"`
##' @param rho_o treatment of the measurement error correlation between x and y.
##' One of `"pooled"` (default) or `"individual"`
##' @param hier.D structure of the individual random effect on `D` when
##' `D = "hierarchical"`. `"shared"` (default) gives each individual a single
##' scalar deviation applied to both coordinates, so individuals differ in
##' overall diffusion magnitude while sharing the population-level x,y
##' anisotropy. `"xy"` gives independent deviations for each coordinate. A
##' correlated bivariate deviation is the natural extension but is not yet
##' implemented; `"shared"` is the parsimonious default because a fast animal
##' is generally fast in both coordinates, and independent deviations would
##' misrepresent that dependence
##' @param ho_scale treatment of the haulout process variance scale factor.
##' `"fixed"` (default) uses the constant supplied by
##' [aniMotum::ssm_control]; `"pooled"` estimates it as a single parameter
##' across all individuals. Estimating `ho_scale` is only sensible in a joint
##' fit: it is not identifiable from one track, which is why it is asserted
##' rather than estimated in the individual filters
##' @param group name of a variable in the input data defining strata within
##' which the measurement parameters are pooled, or NULL (default) to pool
##' across all individuals
##' @param strict logical; if TRUE (default), reject parameter combinations
##' that are not identifiable. If FALSE, warn instead
##'
##' @return a named list of parameter sharing settings
##'
##' @examples
##' ## the recommended default: pooled measurement model, hierarchical process
##' share_control()
##'
##' ## pool the measurement model within tag type
##' share_control(group = "tag_type")
##'
##' ## fully pooled diffusion coefficient - appropriate when there are too few
##' ## individuals to estimate among-individual variance
##' share_control(D = "pooled")
##'
##' @export

share_control <- function(D = c("hierarchical", "pooled", "individual"),
                          tau = c("pooled", "individual", "hierarchical"),
                          psi = c("pooled", "individual", "hierarchical"),
                          rho_p = c("pooled", "individual"),
                          rho_o = c("pooled", "individual"),
                          hier.D = c("shared", "xy"),
                          ho_scale = c("fixed", "pooled"),
                          group = NULL,
                          strict = TRUE) {

  D <- match.arg(D)
  tau <- match.arg(tau)
  psi <- match.arg(psi)
  rho_p <- match.arg(rho_p)
  rho_o <- match.arg(rho_o)
  hier.D <- match.arg(hier.D)
  ho_scale <- match.arg(ho_scale)

  if (!is.logical(strict) || length(strict) != 1)
    stop("strict must be a single logical value", call. = FALSE)
  if (!is.null(group) && !(is.character(group) && length(group) == 1))
    stop("group must be NULL or the name of a single variable in the input data",
         call. = FALSE)

  ## Identifiability guard: random effects on both sides of the process /
  ## measurement variance partition.
  both.sides <- D == "hierarchical" &&
    (tau == "hierarchical" || psi == "hierarchical")

  if (both.sides) {
    msg <- paste0(
      "`D` and the measurement parameters cannot both be hierarchical.\n",
      "  A state-space model partitions total variance between the process\n",
      "  and the measurement model. Individual random effects on both sides\n",
      "  trade off against each other at almost no cost in likelihood, so the\n",
      "  variance hyperparameters are not identifiable - with short tracks the\n",
      "  likelihood surface is close to flat in that direction.\n",
      "  Keep `D` hierarchical and pool the measurement model (the default),\n",
      "  or pool `D` and let the measurement model vary."
    )
    if (strict) stop(msg, call. = FALSE)
    else warning(msg, call. = FALSE, immediate. = TRUE)
  }

  ## A fully individual specification is just independent per-individual fits
  if (all(c(D, tau, psi, rho_p, rho_o) == "individual")) {
    warning("all parameters are specified as `individual`, which is equivalent to\n",
            "  fitting each track separately. Use `fit_ssm(model = \"crw\")` instead,\n",
            "  which is faster and parallelises across individuals.",
            call. = FALSE, immediate. = TRUE)
  }

  list(D = D,
       tau = tau,
       psi = psi,
       rho_p = rho_p,
       rho_o = rho_o,
       hier.D = hier.D,
       ho_scale = ho_scale,
       group = group,
       strict = strict)
}
