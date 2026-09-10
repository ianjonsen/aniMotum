##' @title summarise a joint (hierarchical) ssm fit
##'
##' @description A joint fit is one model fitted to all tracks at once, not one
##' model per track. The returned `ssm_df` keeps a row per individual because
##' the estimated states really are per individual, but the objective,
##' convergence and AICc describe the whole fit. This method reports them once,
##' rather than repeating them against every animal as the `ssm_df` method
##' would, and separates the parameters shared by all individuals from those
##' that vary.
##'
##' @method summary jssm_df
##' @param object a joint ssm fit object with class `jssm_df`
##' @param ... unused. For compatibility with the generic method.
##'
##' @return a list with components `Fittab` (the single fit), `Stattab` (one
##' row per individual), `Shared` (parameters common to all individuals) and
##' `Partab` (parameters that vary, one table per individual)
##'
##' @export

summary.jssm_df <- function(object, ...) {

  ssm <- object$ssm
  n <- length(ssm)
  ok <- sapply(ssm, function(x) length(x) == 15)

  ## ---- the single fit ------------------------------------------------------
  if (any(ok)) {
    f <- ssm[[which(ok)[1]]]
    nll <- if ("objective" %in% names(f$opt)) f$opt$objective else f$opt$value
    Fittab <- cbind(
      "Model" = f$pm,
      "Tracks" = n,
      "n.par" = length(f$opt$par),
      "neg.log.lik" = round(nll, 2),
      "converged" = ifelse(f$opt$convergence == 0, "yes", "no"),
      "pdHess" = ifelse(isTRUE(f$rep$pdHess), "yes", "no"),
      "AICc" = round(f$AICc, 1)
    )
  } else {
    Fittab <- cbind("Model" = "jmp", "Tracks" = n, "n.par" = NA,
                    "neg.log.lik" = NA, "converged" = "no",
                    "pdHess" = NA, "AICc" = NA)
  }
  row.names(Fittab) <- ""

  ## ---- per-individual quantities only --------------------------------------
  ## no AICc or convergence column here: repeating one fit's score against each
  ## animal invites it to be summed or compared animal by animal, and neither
  ## is valid
  Stattab <- cbind(
    "Animal id" = object$id,
    "Time" = sapply(ssm, function(x)
      ifelse(length(x$ts) == 1, x$ts, "variable")),
    "n.obs" = sapply(ssm, function(x) nrow(x$data)),
    "n.filt" = sapply(ssm, function(x) nrow(x$data) -
                        ifelse(is.null(x$fitted), NA, nrow(x$fitted))),
    "n.fit" = sapply(ssm, function(x)
      ifelse(is.null(x$fitted), NA, nrow(x$fitted))),
    "n.pred" = sapply(ssm, function(x)
      ifelse(is.null(x$predicted), NA, nrow(x$predicted)))
  )
  row.names(Stattab) <- rep("", nrow(Stattab))

  ## ---- parameters, split by whether they are shared ------------------------
  mk <- function(p) {
    if (is.null(p) || !nrow(p)) return(NULL)
    p <- p[p[, 2] != 0, , drop = FALSE]
    if (!nrow(p)) return(NULL)
    out <- cbind(rownames(p), round(p[, 1], 4), round(p[, 2], 4))
    colnames(out) <- c("Parameter", "Estimate", "Std.Err")
    row.names(out) <- rep("", nrow(out))
    out
  }

  Shared <- NULL
  Partab <- NULL

  if (any(ok)) {
    p1 <- ssm[[which(ok)[1]]]$par
    sh <- attr(p1, "shared")
    if (is.null(sh)) sh <- rep(TRUE, nrow(p1))

    Shared <- mk(p1[sh, , drop = FALSE])

    if (any(!sh)) {
      Partab <- lapply(ssm, function(x) {
        p <- x$par
        s <- attr(p, "shared")
        if (is.null(s)) return(NULL)
        mk(p[!s, , drop = FALSE])
      })
      names(Partab) <- object$id
    }
  }

  structure(list(Fittab = Fittab, Stattab = Stattab,
                 Shared = Shared, Partab = Partab),
            class = "summary.jssm_df")
}


##' @title print a joint ssm fit summary
##'
##' @method print summary.jssm_df
##' @param x a `summary.jssm_df` object
##' @param ... unused. For compatibility with the generic method.
##'
##' @export

print.summary.jssm_df <- function(x, ...) {

  n <- as.integer(x$Fittab[1, "Tracks"])

  cat("Joint fit: one model fitted to", n, "tracks together\n")
  cat("The estimated states below are per individual. The objective,\n")
  cat("convergence and AICc describe the single fit, not each animal.\n\n")

  print.default(x$Fittab, quote = FALSE, right = TRUE, na.print = ".")

  cat("\nper individual\n")
  print.default(x$Stattab, row.names = FALSE, quote = FALSE,
                right = TRUE, na.print = ".")

  if (!is.null(x$Shared)) {
    cat("\nparameters shared by all", n, "individuals\n")
    print.default(x$Shared, row.names = FALSE, quote = FALSE,
                  digits = 2, right = TRUE, na.print = ".")
  }

  if (!is.null(x$Partab)) {
    cat("\nparameters estimated separately\n")
    for (i in seq_along(x$Partab)) {
      if (is.null(x$Partab[[i]])) next
      cat("\n--------------\n", names(x$Partab)[i], "\n--------------\n",
          sep = "")
      print.default(x$Partab[[i]], row.names = FALSE, quote = FALSE,
                    digits = 2, right = TRUE, na.print = ".")
    }
  }

  invisible(x)
}


##' @title print one individual's part of a joint ssm fit
##'
##' @description Reports what belongs to this individual as this individual's,
##' and what belongs to the joint fit as the joint fit's. The `ssm` method
##' would present the joint negative log-likelihood and convergence as though
##' they were this animal's.
##'
##' @method print jmp_ssm
##' @param x a single individual's component of a joint ssm fit
##' @param ... unused. For compatibility with the generic method.
##'
##' @export

print.jmp_ssm <- function(x, ...) {

  n <- attr(x, "n.track")
  if (is.null(n)) n <- NA

  timeStep <- ifelse(length(x$ts) == 1, x$ts, "multiple time.steps")

  cat("Process model:", x$pm, "- one joint fit to", n, "tracks\n")
  cat("Time interval:", timeStep, if (is.numeric(timeStep)) "hours", "\n")

  if (is.null(x$fitted)) {
    cat("\nthe joint fit failed; see $errmsg\n")
    return(invisible(x))
  }

  cat("\nthis individual\n")
  cat("  number of original observations:", nrow(x$data), "\n")
  cat("  number of observations fitted by ssm:", sum(x$data$keep), "\n")
  cat("  number of fitted states:", nrow(x$fitted), "\n")
  cat("  number of predicted states:",
      ifelse(is.null(x$predicted), 0, nrow(x$predicted)), "\n")

  parm <- x$par
  sh <- attr(parm, "shared")
  if (is.null(sh)) sh <- rep(TRUE, nrow(parm))

  if (any(sh)) {
    cat("\nparameter estimates shared by all", n, "individuals\n")
    cat("-------------------\n")
    print(round(parm[sh, , drop = FALSE], 5), justify = "right")
  }
  if (any(!sh)) {
    cat("\nparameter estimates for this individual\n")
    cat("-------------------\n")
    print(round(parm[!sh, , drop = FALSE], 5), justify = "right")
  }
  cat("-------------------\n")

  nll <- if ("objective" %in% names(x$opt)) x$opt$objective else x$opt$value
  cat("\njoint fit, all", n, "tracks\n")
  cat("  negative log-likelihood:", nll, "\n")
  cat("  convergence:", ifelse(x$opt$convergence == 0, "yes", "no"), "\n")
  cat("  AICc:", round(x$AICc, 1), "\n")
  cat("\nThese three describe the fit to all", n, "tracks together. They are\n")
  cat("not this individual's, and must not be compared or summed across\n")
  cat("individuals.\n\n")

  invisible(x)
}


##' @title print one individual's part of a joint crw fit
##'
##' @method print jcrw_ssm
##' @param x a single individual's component of a joint `jcrw` fit
##' @param ... unused. For compatibility with the generic method.
##'
##' @export

print.jcrw_ssm <- function(x, ...) print.jmp_ssm(x, ...)
