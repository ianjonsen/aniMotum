## ---- include = FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(foieGras)
require(ggplot2)

## ----fit ssm, warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------
d <- subset(sese, id == "ct109-186-14")
fit <- fit_ssm(d, 
               vmax = 3,
               model = "crw", 
               time.step = 24,
               control = ssm_control(verbose = 0))

## ----fit mpm, warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------
fmp <- fit_mpm(fit, 
               what = "predicted", 
               model = "mpm",
               control = mpm_control(verbose = 0))

## ----ts1, eval=FALSE, warning=FALSE, message=FALSE------------------------------------------------------------------------------------------
#  plot(fmp, ask = FALSE)

## ----map1, eval=FALSE, fig.width=7, fig.height=6--------------------------------------------------------------------------------------------
#  map(fit, fmp, what = "predicted", silent = TRUE)

## ----fit_ssm, warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------
fit <- fit_ssm(d, 
               vmax = 3, 
               model = "mp", 
               time.step = 24,
               control = ssm_control(verbose = 0))

## ----ts2, eval=FALSE, fig.width=7, fig.height=3---------------------------------------------------------------------------------------------
#  plot(fit, what = "predicted", type = 3, normalise = TRUE)

## ----map2, eval=FALSE, fig.width=7, fig.height=6--------------------------------------------------------------------------------------------
#  map(fit, what = "p", normalise = TRUE, silent = TRUE)

