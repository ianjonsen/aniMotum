##' @title object summaries
##' @description return a summary of an `ssm_df` fit object
##' @param object an `ssm_df` fit object
##' @param ... additional arguments to be ignored
##' @importFrom stats pnorm AIC BIC
##' @method summary ssm_df
##' @export
summary.ssm_df <- function(object, ...) {
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }

  mkStattab <- function(object) {
    ids <- object$id
    pm <- sapply(object$ssm, function(x) x$pm)
    p.int <- sapply(object$ssm, function(x) ifelse(length(x$ts) == 1,
                                                   x$ts,
                                                   "variable"))
    nobs.tot <- sapply(object$ssm, function(x) nrow(x$data))
    nfit <- sapply(object$ssm, function(x) nrow(x$fitted))
    nfilt <- nobs.tot - nfit
    npred <- sapply(object$ssm, function(x) nrow(x$predicted))
    nrr <- sapply(object$ssm, function(x) {
      if("rerouted" %in% names(x)) nrow(x$rerouted)
      else NA
    })
    conv <- sapply(object$ssm, function(x) ifelse(x$opt$conv == 0, TRUE, FALSE))
    
    ## req'd to handle old fG_ssm format
    if ("AICc" %in% names(object$ssm[[1]])) {
      AICc <- round(sapply(object$ssm, function(x) x$AICc), 1)
      Stattab <- cbind(ids, pm, p.int, nobs.tot, nfilt, nfit, npred, nrr, conv, AICc)
      colnames(Stattab) <- c("Animal id", "Model", "Time", "n.obs", "n.filt", "n.fit", "n.pred", "n.rr", "converged", "AICc")
      
    } else if ("aic" %in% names(object$ssm[[1]])) {
      AIC <- round(sapply(object$ssm, function(x) x$aic), 1)
      Stattab <- cbind(ids, pm, p.int, nobs.tot, nfilt, nfit, npred, nrr, conv, AIC)
      colnames(Stattab) <- c("Animal id", "Model", "Time", "n.obs", "n.filt", "n.fit", "n.pred", "n.rr", "converged", "AIC")
    }
    row.names(Stattab) <- rep("", dim(Stattab)[1])
    Stattab
  }

    mkPartab <- function(object) {
      pars <- lapply(object$ssm, function(x) {
        pars <- round(x$par[x$par[,2] != 0, ], 4) # remove any fixed parameters
        pars <- cbind(row.names(pars), pars[,1], pars[,2])
        colnames(pars) <- c("Parameter", "Estimate", "Std.Err")
        row.names(pars) <- rep("", dim(pars)[1])
        pars
    })
      pars
    }

  structure(
    list(
      Stattab = mkStattab(object),
      Partab = mkPartab(object)
    ),
    class = "summary.ssm_df"
  )

}

##' @importFrom stats printCoefmat
##' @method print summary.ssm_df
##' @export
print.summary.ssm_df <- function(x,
                               signif.stars = getOption("show.signif.stars"),
                               ...)
{
  print.default(x$Stattab,
                row.names = FALSE,
                quote = FALSE,
                right = TRUE,
                na.print = ".")

  fun <- function() {
    readline("print individual model parameter tables? [enter / esc to stop]")
  }
  if(interactive()) fun()

  nms <- names(sapply(x$Partab, function(.) names(.)))

  for(i in 1:length(x$Partab)) {
    cat("\n--------------\n")
    cat(nms[i], "\n")
    cat("--------------\n")
    print.default(x$Partab[[i]],
                  row.names = FALSE,
                  quote = FALSE,
                  digits = 2,
                  right = TRUE,
                  na.print = ".")

    fun <- function() {
      readline("next table... [enter / esc to stop]")
    }

    if(interactive()) fun()
  }

  invisible(x)
} ## print.summary.ssm_df
