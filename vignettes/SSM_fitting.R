## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(tidyverse)
require(patchwork)
require(foieGras)

## ----pre-processing, message=FALSE--------------------------------------------
fit_ssm(ellie, vmax = 3, pf = TRUE)

## ----model fit, message=FALSE, eval=FALSE-------------------------------------
#  fit <- fit_ssm(ellie, model = "rw", time.step = 12)

## ----emf, message=FALSE-------------------------------------------------------
emf()

## ----emf mod, message=FALSE, fig.width=7, fig.height=6------------------------
emf2 <- emf() %>%
  mutate(emf.x = c(0.1, 1, 2, 4, 20, 8, 15, 15))

fit1 <- fit_ssm(sese1, model = "rw", time.step=24, 
                control = ssm_control(verbose = 0))
fit2 <- fit_ssm(sese1, model = "rw", time.step=24, emf = emf2, 
                control = ssm_control(verbose = 0))

par(mfrow = c(2,1), mar = c(4,3,1,1))
hist(grab(fit1, "f")$x - grab(fit2, "f")$x, breaks = 20, 
     main = "fit1$x - fit2$x", xlim = c(-20,20), xlab = "")
hist(grab(fit1, "f")$y - grab(fit2, "f")$y, breaks = 20, 
     main = "fit1$y - fit2$y", xlim = c(-20,20), xlab = "Distance (km)")

## ----demonstrate map use, message=FALSE, warning=FALSE------------------------
fit1 <- fit_ssm(ellie, model = "crw", time.step = 12,
                control = ssm_control(verbose = 0))

fit2 <- fit_ssm(ellie, model = "crw", time.step = 12, map = list(psi = factor(NA)),
                control = ssm_control(verbose = 0))

c(fit1$ssm[[1]]$AICc, fit2$ssm[[1]]$AICc)

