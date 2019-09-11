##' @title calculate one-step-ahead (prediction) residuals from a \code{foieGras} fit
##'
##' @param x a compound \code{fG} tbl or a \code{foieGras} individual fit object
##' @param plot should results be plotted and returned? (default = TRUE)
##' @param method method to calculate prediction residuals (default is "oneStepGaussianOffMode"; see `?TMB::oneStepPrediction` for details)
##'
##' @details One-step-ahead residuals are useful for assessing goodness-of-fit in latent variable models. This is a wrapper function for TMB::oneStepPredict (beta version)
##'
##' @references Thygesen, U. H., C. M. Albertsen, C. W. Berg, K. Kristensen, and A. Neilsen. 2017. Validation of ecological state space models using the Laplace approximation. Environmental and Ecological Statistics 24:317–339.
##'
##' @examples
##' ## see summary fit output
##' ## load example foieGras fit object (to save time)
##' data(fit)
##' osar(fit$ssm[[1]])
##'
##' @importFrom dplyr "%>%" select slice mutate rename bind_rows everything filter
##' @importFrom tibble as_tibble
##' @importFrom TMB oneStepPredict
##' @importFrom furrr future_map
##' @importFrom future plan
##' @export

osar <- function(x, plot = TRUE, method = "oneStepGaussianOffMode", ...)
{

  fmap_fn <- function(f) {
    sub <- which(rep(f$isd, each = 2))
    oneStepPredict(obj = f$tmb,
                   observation.name = "Y",
                   data.term.indicator = "keep",
                   method = method,
                   subset = sub,
                   discrete = FALSE,
                   parallel = FALSE,
                   trace = FALSE,
                   ...)
  }

  if(inherits(x, "fG")) {
    cat("parallel processing...\n")
    plan("multisession")
    r <- x$ssm %>%
      future_map(~ try(fmap_fn(.x)))

  } else if(inherits(x, "foieGras")) {
    sub <- which(rep(x$isd, each = 2))
    r <- oneStepPredict(
      x$tmb,
      "Y",
      "keep",
      method = method,
      subset = sub,
      discrete = FALSE,
      parallel = TRUE,
      ...
    )
  }

  if(inherits(r, "list")) {
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

  } else {
    z <- r %>%
      mutate(id = x$id) %>%
      select(id, everything())
    x.z <- z %>% slice(seq(1, nrow(z), by = 2))
    y.z <- z %>% slice(seq(2, nrow(z), by = 2))

    out <- bind_rows(x.z, y.z) %>%
      mutate(coord = rep(c("x","y"), each=nrow(z)/2)) %>%
      as_tibble() %>%
      rename(obs = "observation", resid = "residual")

  }

  if(plot) {
    p <- ggplot(out %>% filter(!is.na(resid)), aes(sample = resid)) +
      geom_qq() +
      geom_qq_line(col = "firebrick") +
      facet_grid(id ~ coord)
    print(p)
    }

  out
}
