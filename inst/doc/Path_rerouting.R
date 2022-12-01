## ---- include = FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(foieGras)
require(ggplot2)

## ----wese1, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=5-------------------------------------------------------------
#  # fit SSM to Weddell seal Argos data
#  fit <- fit_ssm(wese_sb,
#                 vmax = 3,
#                 model = "mp",
#                 time.step = 2,
#                 control = ssm_control(verbose = 0))
#  
#  # map the Argos locations
#  aes <- aes_lst(est=FALSE, obs=TRUE, conf=FALSE, mp=FALSE)
#  aes$df$col[4] <- "firebrick"
#  aes$df$size[4] <- 1.25
#  
#  m1 <- map(fit,
#      what = "p",
#      aes = aes,
#      silent=TRUE,
#      crs = "+proj=stere +lon_0=166 +datum=WGS84 +units=km",
#      ext.rng = c(0.3,0.1))
#  
#  # map the SSM-predicted locations
#  aes <- aes_lst(obs=FALSE, conf=FALSE, mp=FALSE, line=TRUE)
#  m2 <- map(fit,
#      what = "p",
#      aes = aes,
#      silent=TRUE,
#      crs = "+proj=stere +lon_0=166 +datum=WGS84 +units=km",
#      ext.rng = c(0.3,0.1))
#  (m1 + labs(title = "Argos locations") | m2 + labs(title = "SSM-predicted locations")) &
#      theme(panel.grid= element_line (size=0.1, colour="grey60"))

## ----wese2a, eval=FALSE, echo = FALSE, warning=FALSE, message=FALSE-------------------------------------------------------------------------
#  if(requireNamespace("pathroutr", quietly = TRUE)) {
#    fit <- route_path(fit,
#                      what = "predicted",
#                      map_scale = 10,
#                      buffer = 10000)
#  } else {
#    cat("\n the 'pathroutr' pkg is not installed, use remotes::install_github(\"jmlondon/pathroutr\")
#            or install.packages(\"pathroutr\", repos = \"https://jmlondon.r-universe.dev\")to use this function\n")
#  }

## ----wese2b, eval = FALSE, warning=FALSE, message=FALSE-------------------------------------------------------------------------------------
#  fit <- route_path(fit,
#                      what = "predicted",
#                      map_scale = 10,
#                      buffer = 10000)

## ----wese3b, eval = FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=5----------------------------------------------------------
#  my.aes <- aes_lst(line = TRUE,
#                 conf = FALSE,
#                 mp = FALSE,
#                 obs = TRUE)
#  m1 <- map(fit,
#            what = "predicted",
#            aes = my.aes,
#            crs = "+proj=stere +lon_0=166 +datum=WGS84 +units=km",
#            ext.rng = c(0.3, 0.1),
#            silent = TRUE)
#  
#  m2 <- map(fit,
#            what = "rerouted",
#            aes = my.aes,
#            crs = "+proj=stere +lon_0=166 +datum=WGS84 +units=km",
#            ext.rng = c(0.3, 0.1),
#            silent = TRUE)
#  
#  (m1 + labs(title = "SSM-predicted locations") | m2 + labs(title = "Re-routed locations")) &
#    theme(panel.grid= element_line (size=0.1, colour="grey60"))

## ----wese3a, eval=FALSE, echo = FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=5----------------------------------------------
#  my.aes <- aes_lst(line = TRUE,
#                 conf = FALSE,
#                 mp = FALSE,
#                 obs = TRUE)
#  m1 <- map(fit,
#            what = "predicted",
#            aes = my.aes,
#            crs = "+proj=stere +lon_0=166 +datum=WGS84 +units=km",
#            ext.rng = c(0.3, 0.1),
#            silent = TRUE)
#  if(requireNamespace("pathroutr", quietly = TRUE)) {
#    m2 <- map(fit,
#              what = "rerouted",
#              aes = my.aes,
#              crs = "+proj=stere +lon_0=166 +datum=WGS84 +units=km",
#              ext.rng = c(0.3, 0.1),
#              silent = TRUE)
#  
#    (m1 + labs(title = "SSM-predicted locations") | m2 + labs(title = "Re-routed locations")) &
#      theme(panel.grid= element_line (size=0.1, colour="grey60"))
#    ggsave(filename = "images/path_rerouting/map_2ab.jpg", dpi=150, bg="white")
#  } else {
#    m2 <- map(fit,
#            what = "predicted",
#            aes = my.aes,
#            crs = "+proj=stere +lon_0=166 +datum=WGS84 +units=km",
#            ext.rng = c(0.3, 0.1),
#            silent = TRUE)
#    (m1 + labs(title = "SSM-predicted locations") | m2 + labs(title = "'pathroutr' pkg needs installation")) &
#      theme(panel.grid= element_line (size=0.1, colour="grey60"))
#    cat("\n the 'pathroutr' pkg is not installed, use remotes::install_github(\"jmlondon/pathroutr\")
#            or install.packages(\"pathroutr\", repos = \"https://jmlondon.r-universe.dev\")to use this function\n")
#  }

