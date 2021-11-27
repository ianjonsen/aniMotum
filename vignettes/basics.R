## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
require(foieGras)

## ----data 1, echo = FALSE-----------------------------------------------------
data(ellie, package = "foieGras")
head(data.frame(ellie))

## ----data 2, echo = FALSE-----------------------------------------------------
data(sese, package = "foieGras")
head(data.frame(sese))

## ----data 3, echo = FALSE-----------------------------------------------------
data(ellie, package = "foieGras")
ellie[3:5, c("smaj","smin","eor")] <- NA
head(data.frame(ellie))

## ----data 4, echo = FALSE, message=FALSE--------------------------------------
data(ellie, package = "foieGras")
foo <- sf::st_as_sf(ellie, coords=c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +no_defs") 
foo <- sf::st_transform(foo, crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs")
head(data.frame(foo))

## ----data 5, echo = FALSE-----------------------------------------------------

  data.frame(
  id = rep(54632, 5),
  date = seq(Sys.time(), by = "12 hours", length.out = 5),
  lc = rep("GL", 5),
  lon = seq(100, by = 0.5, length = 5),
  lat = seq(-55, by = 1, length = 5),
  lonerr = rexp(5, 1 / 0.5),
  laterr = rexp(5, 1 / 1.5)
  )

## ----data 6, echo = FALSE-----------------------------------------------------

  data.frame(
  id = rep("F02-B-17", 5),
  date = seq(Sys.time(), by = "1 hours", length.out = 5),
  lc = rep("G", 5),
  lon = seq(70.1, by = 0.5, length = 5),
  lat = seq(-49.2, by = 1, length = 5)
  )

## ----data 7, echo = FALSE-----------------------------------------------------

  data.frame(
    id = rep("F02-B-17", 5),
    date = c("2017-09-17 05:20:00", "2017-10-04 14:35:01", "2017-10-05 04:03:25", "2017-10-05 06:28:20", "2017-10-05 10:21:18"),
    lc = c("G","2","G","A","B"),
    lon = c(70.1, 70.2, 70.1, 71.1, 70.8),
    lat = c(-49.2, -49.1, -49.3, -48.7, -48.5),
    smaj = c(NA, 1890, NA, 28532, 45546),
    smin = c(NA, 45, NA, 1723, 3303),
    eor = c(NA, 77, NA, 101, 97)
  )

## ----fit_ssm, message=FALSE---------------------------------------------------
## prefilter and fit Random Walk SSM using a 24 h time step
fit <-
  fit_ssm(
    sese1,
    model = "rw",
    time.step = 24,
    control = ssm_control(verbose = 0)
  )

## ----multi-fits, message=FALSE------------------------------------------------
## list fit outcomes for both seals
fit

## ----fit summary, message = FALSE---------------------------------------------
fit$ssm[[1]]

## ----fit plot, fig.width=6, fig.height=4--------------------------------------
# plot time-series of the predicted values
plot(fit, what = "predicted", type = 1, pages = 1)
plot(fit, what = "fitted", type = 2, pages = 1)

## ----osar plots, eval=FALSE---------------------------------------------------
#  require(patchwork)
#  # calculate & plot residuals
#  res <- osar(fit)
#  
#  (plot(res, type = "ts") | plot(res, type = "qq")) /
#    (plot(res, type = "acf") | plot_spacer())

## ---- echo=FALSE, message=FALSE, fig.width=8, fig.height=4--------------------
require(patchwork)
## load osar res data to speed vignette build
data(res)
(plot(res, type = "ts") | plot(res, type = "qq")) / 
  (plot(res, type = "acf") | plot_spacer())

## ----behaviour, eval=FALSE, message=FALSE, fig.width=8, fig.height=4----------
#  fmp <- fit_mpm(fit, what = "predicted", model = "mpm", control = mpm_control(verbose = 0))
#  
#  plot(fmp, pages = 1, pal = "Zissou1", rev = TRUE)
#  
#  plot(fmp, fit, pages = 1, pal = "Cividis")

