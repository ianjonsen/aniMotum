##' @title object summaries
##' @description return a summary of an mpmm fit object
##' @param object an mpmm fit object
##' @param ... additional arguments to be ignored
##' @importFrom stats pnorm AIC BIC
##' @importFrom lme4 nobars findbars
##' @importFrom dplyr %>%
##' @method summary mpmm
##' @export
summary.mpmm <- function(object, ...) {
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  nobs <-
    with(object$data, apply(!is.na(cbind(lon, lat)), 1, min)) %>%
    sum(.)

  mkAICtab <- function(object) {
    ll <- logLik(object)
    data.frame(
      AIC      = suppressWarnings(AIC(object)), # suppress b/c logLik will throw warning first
      BIC      = suppressWarnings(BIC(object)),
      logLik   = ll,
      deviance = ll * -2,
      df.resid = nobs - length(object$tmb$par)
    )
  }

  mkVartab <- function(object) {
    grpnm <- names(object$re)[1]
    rep <- object$tmb$env$report(object$tmb$env$last.par.best)
    stdev <- c(rep$sd[[1]],
               log(object$par["sigma_g", "Estimate"]))
    Vartab <-
      data.frame(
        Group = c(grpnm, rep("", length(stdev) - 1)),
        Name = c(names(object$re)[-1], "Residual"),
        Variance = round(stdev^2, 4),
        StdDev = round(stdev, 4)
      )

    if (length(stdev) > 2) {
      tcorr <- rep$corr[[1]]
      tcorr[upper.tri(tcorr, diag = TRUE)] <- NA
      tcorr <- round(as.vector(tcorr)[!is.na(tcorr)], 2)
      nr <- nrow(Vartab)

      excols <- nr-2
      i <- 1

      while(i <= excols) {
        if(i == 1) {
          Vartab <- data.frame(Vartab, Corr = c("", tcorr[1:(nr-2)], ""))
          tcorr <- tcorr[-c(1:(nr-2))]
        }
        else {
          if(length(tcorr) == 1) {
            Vartab <- data.frame(Vartab, c(rep("",i), tcorr, ""))
            break
          } else {
          Vartab <- data.frame(Vartab, c(rep("",i), tcorr[1:i], ""))
          tcorr <- tcorr[-c(1:i)]
          }
        }
        i <- i + 1
      }
      nc <- ncol(Vartab)
      if(nc > 5) names(Vartab)[6:nc] <- ""
    }
    Vartab
  }

  mkFixtab <- function(object) {
    terms <- nobars(object$formula) %>%
      terms() %>%
      attr(., "term.labels")
    terms <- c("Intercept", terms)
    val <- object$par[rownames(object$par) %in% terms, "Estimate"]
    stderr <- object$par[rownames(object$par) %in% terms, "Std. Error"]
    terms[1] <- "(Intercept)"

    Fixtab <- cbind(#terms,
                         val,
                         stderr,
                         val / stderr,
                         2 * pnorm(abs(val/stderr), lower.tail = FALSE))
    colnames(Fixtab) <- c("Value", "Std.Error", "z value", "Pr(>|z|)")
    Fixtab
  }

  nobs <- with(object$data, apply(!is.na(cbind(lon, lat)), 1, min)) %>%
    sum(.)
  resid.df <- nobs - length(object$opt$par)
  terms <- nobars(object$formula) %>%
    terms() %>%
    attr(., "term.labels")
  terms <- c("Intercept", terms)
  coef <- object$par[rownames(object$par) %in% terms, "Estimate"]

  ranform <- lme4::findbars(object$formula) %>%
    as.character() %>%
    paste0("(", ., ")")
  fixform <- lme4::nobars(object$formula) %>% as.character()

  structure(
    list(
      mf = object$mf,
      logLik = mkAICtab(object),
      grpnm = names(object$re)[1],
      nobs = nobs,
      coefficients = coef,
      ranform = ranform,
      fixform = fixform,
      formula = object$formula,
      Vartab = mkVartab(object),
      Fixtab = mkFixtab(object)
    ),
    class = "summary.mpmm"
  )
}

##' @importFrom stats printCoefmat
##' @method print summary.mpmm
##' @export
print.summary.mpmm <- function(x, digits = 3,
                               signif.stars = getOption("show.signif.stars"),
                               ...)
{


 # print(x$logLik); cat("\n")
    cat("Formula: ~", as.character(x$formula)[2], "\n")
    cat("Data: ", x$mf$data, "\n\n")
    print(x$logLik, row.names = FALSE); cat("\n\n")

    cat("Random effects: ~", x$ranform, "\n")
    print(x$Vartab, row.names = FALSE, digits = digits)
    cat("number of obs: ", x$nobs, ", group: ", x$grpnm, "\n\n", sep = "")

    cat("fixed effects:", x$fixform, "\n")
    printCoefmat(x$Fixtab, digits = digits, signif.stars = signif.stars)

  invisible(x)
}## print.summary.mpmm
