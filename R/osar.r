##' @title calculate one-step-ahead (prediction) residuals from a \code{foieGras} fit
##'
##' @param x a compound \code{fG} tbl fit object
##' @param method method to calculate prediction residuals (default is "oneStepGaussianOffMode"; see `?TMB::oneStepPrediction` for details)
##' @param ... other arguments to TMB::oneStepPrediction
##'
##' @details One-step-ahead residuals are useful for assessing goodness-of-fit in latent variable models. This is a wrapper function for TMB::oneStepPredict (beta version)
##'
##' @references Thygesen, U. H., C. M. Albertsen, C. W. Berg, K. Kristensen, and A. Neilsen. 2017. Validation of ecological state space models using the Laplace approximation. Environmental and Ecological Statistics 24:317–339.
##'
##' @examples
##' ## see summary fit output
##' ## load example foieGras fit object (to save time)
##' data(fit)
##' fit_res <- osar(fit)
##' plot(fit_res)
##'
##' @importFrom dplyr "%>%" select slice mutate rename bind_rows everything filter
##' @importFrom tibble as_tibble
##' @importFrom TMB oneStepPredict
##' @importFrom purrr map
##' @export

osar <- function(x, method = "oneStepGaussianOffMode", ...)
{

  fmap_fn <- function(f) {
    sub <- which(rep(f$isd, each = 2))
    oneStepPredict(obj = f$tmb,
                   observation.name = "Y",
                   data.term.indicator = "keep",
                   method = method,
                   subset = sub,
                   discrete = FALSE,
                   parallel = TRUE,
                   trace = FALSE,
                   ...)
  }

  ## FIXME: find a way to better parallelise this. parallel = TRUE only uses 2 cores & is too slow
  if(inherits(x, "fG")) {
    r <- x$ssm %>%
      map(~ try(fmap_fn(.x)))

  } else if(inherits(x, "foieGras")) {
    stop("provide an fG compound tbl: `osar(fit)`")
  }


    out <- lapply(1:length(r), function(i) {
      z <- r[[i]] %>%
        mutate(id = x$id[i]) %>%
        select(id, everything())
      x.z <- z %>% slice(seq(1, nrow(z), by = 2))
      y.z <- z %>% slice(seq(2, nrow(z), by = 2))

      bind_rows(x.z, y.z) %>%
        mutate(coord = rep(c("x","y"), each=nrow(z)/2))
      }) %>%
      do.call(rbind, .) %>%
      as_tibble() %>%
      rename(obs = "observation", resid = "residual")


  class(out) <- append("osar", class(out))
  return(out)
}
