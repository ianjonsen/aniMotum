## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(aniMotum)

## ----visualise SSM fit1, eval=TRUE, fig.width=7, fig.height=5-----------------
fit.rw <- fit_ssm(ellie, 
                  model = "rw", 
                  time.step = 24, 
                  control = ssm_control(verbose = 0))

plot(fit.rw, what = "fitted")

plot(fit.rw, what = "predicted")

## ----visualise as track, eval=TRUE, fig.width=7, fig.height=6-----------------
plot(fit.rw, "p", type = 2, alpha = 0.1)

## ----osar plots1, eval=TRUE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
# use patchwork package to arrange plot.osar options
require(patchwork)
# calculate & plot residuals
res.rw <- osar(fit.rw)

(plot(res.rw, type = "ts") | plot(res.rw, type = "qq")) / 
  (plot(res.rw, type = "acf") | plot_spacer())

## ----fit crw and plot osar1, eval=TRUE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
fit.crw <- fit_ssm(ellie, 
                   model = "crw", 
                   time.step = 24, 
                   control = ssm_control(verbose = 0))

res.crw <- osar(fit.crw)

(plot(res.crw, type = "ts") | plot(res.crw, type = "qq")) / 
  (plot(res.crw, type = "acf") | plot_spacer())

c(fit.rw$ssm[[1]]$AICc, fit.crw$ssm[[1]]$AICc)

