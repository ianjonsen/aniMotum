##' @title calculate one-step-ahead (prediction) residuals from a `aniMotum` 
##' `ssm` fit
##'
##' @param x a `aniMotum` `ssm` fit object with class `ssm_df`
##' @param method method to calculate prediction residuals 
##' (default is `oneStepGaussianOffMode`; see [TMB::oneStepPredict] for details)
##' @param ... other arguments to [TMB::oneStepPredict]
##'
##' @details One-step-ahead residuals are useful for assessing goodness-of-fit 
##' in latent variable models. This is a wrapper function for [TMB::oneStepPredict]
##' (beta version). `osar` tries the `fullGaussian` (fastest) method first and 
##' falls back to the `oneStepGaussianOffMode` (slower) method for any failures. 
##' Subsequent failures are dropped from the output and a warning message is given. 
##' Note, OSA residuals can take a considerable time to calculate if there are 
##' many individual fits and/or deployments are long. The method is automatically
##' parallelised across 2 x the number of individual fits, up to the number of 
##' processor cores available.
##'
##' @references Thygesen, U. H., C. M. Albertsen, C. W. Berg, K. Kristensen, and 
##' A. Neilsen. 2017. Validation of ecological state space models using the Laplace 
##' approximation. Environmental and Ecological Statistics 24:317–339.
##'
##' @examples
##' # generate a ssm fit object (call is for speed only)
##' xs <- fit_ssm(ellie, spdf=FALSE, model = "rw", time.step=24, control = ssm_control(verbose = 0))
##' 
##' res <- osar(xs)
##'
##' @importFrom dplyr select slice mutate bind_rows everything
##' @importFrom tibble as_tibble
##' @importFrom TMB oneStepPredict
##' @export
##' @md

osar <- function(x, method = "fullGaussian", ...)
{

  map_fn <- function(f, method) {
    oneStepPredict(obj = f$tmb,
                   observation.name = "Y",
                   data.term.indicator = "keep",
                   method = method,
                   subset = which(rep(f$isd, each = 2)),
                   discrete = FALSE,
                   trace = FALSE,
                   ...)
  }

  ## A joint fit stores the same TMB object on every row, holding all the
  ## animals' observations at once, while $isd is one individual's. The subset
  ## index below would then point at the wrong columns of Y and be too short,
  ## so the residuals returned would be silently wrong rather than absent.
  ## Beyond the indexing, TMB::oneStepPredict needs the objective function to
  ## carry a per-observation data-term indicator; the RTMB templates behind the
  ## joint models compute the observation likelihood as a single summed
  ## quadratic form and do not yet declare one.
  if(inherits(x, "jssm_df")) {
    stop("one-step-ahead residuals are not available for joint fits (`model = ",
         "\"jmp\"` or `\"jcrw\"`) yet.\n  To check goodness-of-fit for these ",
         "data, fit the corresponding single-animal model\n  (`\"mp\"` or ",
         "`\"crw\"`) and call osar() on that.", call. = FALSE)
  }

  if(inherits(x, "ssm_df")) {
    if(nrow(x) > 3 & 
       requireNamespace("future", quietly = TRUE) &
       requireNamespace("furrr", quietly = TRUE) &
       requireNamespace("parallelly", quietly = TRUE)
       ) {
    cat("running in parallel, this could take a while...\n")
    cl <- future::makeClusterPSOCK(parallelly::availableCores())
    future::plan("future::cluster", workers = cl)
    
    r <- x$ssm |>
      furrr::future_map(~ try(map_fn(.x, method), silent = TRUE), 
                 .options = furrr::furrr_options(seed = TRUE))
    
    suppressWarnings(parallelly::autoStopCluster(cl))
    
    } else {
      if(nrow(x) > 3) cat("future and furrr packages not installed for parallel processing, 
                           running sequentially. This could take a while...\n")
      r <- lapply(1:nrow(x), function(i) {
        try(map_fn(x$ssm[[i]], method), silent = TRUE)
    })
    }
  } else {
    stop("a aniMotum ssm fit object with class `ssm_df` is required")
  }
  
  ## r is a list with one element per row of x, and it stays that way: every
  ## result is stored at the row of the individual it belongs to, and
  ## individuals whose residuals cannot be calculated are recorded by row
  ## number in `failed` instead of being removed from r. Compacting r would
  ## shift every later result onto the wrong individual's id and dates.
  cr <- sapply(r, function(.) inherits(., "try-error"))
  failed <- integer(0)

  ## if any try-errors then retry using oneStepGaussianOffMode but preserve successful results
  if (any(cr)) {
    ## re-try on failures
    redo <- x[which(cr), ]
    if(nrow(x) > 3 & 
       requireNamespace("future", quietly = TRUE) &
       requireNamespace("furrr", quietly = TRUE) &
       requireNamespace("parallelly", quietly = TRUE)
    ) {
      cat("running in parallel, this could take a while...\n")
      cl <- future::makeClusterPSOCK(parallelly::availableCores())
      future::plan("future::cluster", workers = cl)
      
      r.redo <- redo$ssm |>
        furrr::future_map(~ try(map_fn(.x, method = "oneStepGaussianOffMode")), 
                   .options = furrr::furrr_options(seed = TRUE))
      
      suppressWarnings(parallelly::autoStopCluster(cl))
      
    } else {
      if(nrow(x) > 3) cat("future and furrr packages not installed for parallel processing, 
                           running sequentially. This could take a while...\n")
      r.redo <- lapply(1:nrow(redo), function(i) {
        try(map_fn(redo$ssm[[i]], method = "oneStepGaussianOffMode"), silent = TRUE)
      })
    }
    ## r.redo[[j]] belongs to row which(cr)[j] of x
    cr.redo <- sapply(r.redo, function(.) inherits(., "try-error"))
    redo.rows <- which(cr)

    ## store the retry successes at their own rows
    r[redo.rows[!cr.redo]] <- r.redo[!cr.redo]

    ## individuals that failed both methods
    failed <- redo.rows[cr.redo]
    if (length(failed) > 0) {
      warning(
        sprintf(
          "\n failed to calculate OSA residuals for the following individuals: %s \n",
          paste(x$id[failed], collapse = ", ")
        ), immediate. = TRUE, call. = FALSE
      )
    }
  }

  keep <- setdiff(seq_len(nrow(x)), failed)

  ## return error if no successful results, otherwise return OSA resid object
  if (length(keep) == 0) {
    stop("no residuals calculated", call. = FALSE)
  } else {
    out <- lapply(keep, function(i) {
      z <- r[[i]] |>
        mutate(id = x$id[i]) |>
        select(id, everything())
      x.z <- z |> slice(seq(1, nrow(z), by = 2))
      y.z <- z |> slice(seq(2, nrow(z), by = 2))
      date <- x$ssm[[i]]$fitted$date
      bind_rows(data.frame(date, x.z), data.frame(date, y.z)) |>
        mutate(coord = rep(c("x", "y"), each = nrow(z) / 2))
    }) 
    
    out <- lapply(out, function(x) {
        x[, c("id", "date", "residual", "coord")]
      }) 
    out <- do.call(rbind, out) |> 
      as_tibble() 

    class(out) <- append("osar", class(out))
    return(out)
  }
}
