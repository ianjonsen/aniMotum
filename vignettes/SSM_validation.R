## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(foieGras)

## ----visualise SSM fit, fig.width=7, fig.height=5-----------------------------

fit.rw <- fit_ssm(ellie, model = "rw", time.step = 12, control = ssm_control(verbose = 0))

plot(fit.rw, what = "fitted")

plot(fit.rw, what = "predicted")

## ----visualise as track, fig.width=7, fig.height=6----------------------------

plot(fit.rw, "p", type = 2, alpha = 0.02)

## ----osar plots, warning=FALSE, message=FALSE, fig.width=7, fig.height=5------
# use patchwork package to arrange plot.osar options
require(patchwork)
# calculate & plot residuals
res.rw <- osar(fit.rw)

(plot(res.rw, type = "ts") | plot(res.rw, type = "qq")) / 
  (plot(res.rw, type = "acf") | plot_spacer())

## ----fit crw and plot osar, warning=FALSE, message=FALSE, fig.width=7, fig.height=5----

fit.crw <- fit_ssm(ellie, model = "crw", time.step = 12, control = ssm_control(verbose = 0))

res.crw <- osar(fit.crw)

(plot(res.crw, type = "ts") | plot(res.crw, type = "qq")) / 
  (plot(res.crw, type = "acf") | plot_spacer())

fit.rw$ssm[[1]]$AICc
fit.crw$ssm[[1]]$AICc

