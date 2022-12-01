## ---- include = FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(foieGras)
require(ggplot2)

## ----map 1, eval=FALSE, warning=FALSE, message=FALSE----------------------------------------------------------------------------------------
#  se2 <- subset(sese, id %in% unique(id)[1:2])
#  fit <- fit_ssm(se2,
#                 model = "mp",
#                 time.step = 24,
#                 control = ssm_control(verbose = 0))
#  
#  map(fit, what = "predicted")

## ----map 1a, echo=FALSE, fig.height=4, fig.width=7, warning=FALSE, message=FALSE------------------------------------------------------------
se2 <- subset(sese, id %in% unique(id)[1:2])
fit <- fit_ssm(se2, 
               model = "mp", 
               time.step = 24, 
               control = ssm_control(verbose = 0))

## ----1b, eval=FALSE, fig.height=4, fig.width=7, message=FALSE, warning=FALSE----------------------------------------------------------------
#  map(fit, what = "predicted", map_type = "cartodark", zoom = 4, progress = "none")

## ----map 2, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=4-------------------------------------------------------------
#  my.aes <- aes_lst(obs=TRUE, line=TRUE, mp=FALSE)
#  
#  map(fit, what = "p", aes = my.aes)

## ----map 3, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=4-------------------------------------------------------------
#  my.aes$df
#  
#  my.aes$df$col[4] <- "firebrick"
#  my.aes$df$shape[4] <- 18
#  
#  map(fit,
#      what = "p",
#      aes = my.aes,
#      by.id = FALSE)

## ----map 4, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=4-------------------------------------------------------------
#  map(fit,
#      what = "p",
#      aes = aes_lst(mp=FALSE, conf=FALSE),
#      by.date = TRUE,
#      by.id=FALSE)

## ----map 5, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=4-------------------------------------------------------------
#  map(fit,
#      what = "p",
#      aes = aes_lst(mp=FALSE,
#                    conf=FALSE,
#                    date_pal = hcl.colors(n=100, "Plasma")),
#      by.date = TRUE, by.id=FALSE)

## ----map 6, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=4-------------------------------------------------------------
#  map(fit,
#      what = "p",
#      aes = aes_lst(mp_pal = hcl.colors(n=100, "RdBu")))

## ----map 7, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=4-------------------------------------------------------------
#  map(fit,
#      what = "p",
#      aes = aes_lst(mp_pal = hcl.colors(n=100, "RdBu")),
#      crs = "+proj=stere +lon_0=68 +datum=WGS84 +units=km")

