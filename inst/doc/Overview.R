## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(aniMotum)

## ----data 1, echo = FALSE-----------------------------------------------------
data(sese, package = "aniMotum")
head(data.frame(sese))

## ----data 2, echo = FALSE-----------------------------------------------------
data(ellie, package = "aniMotum")
head(data.frame(ellie))

## ----data 3, echo = FALSE-----------------------------------------------------
  data.frame(
  id = rep("F02-B-17", 5),
  date = seq(Sys.time(), by = "1 hours", length.out = 5),
  lc = rep("G", 5),
  lon = seq(70.1, by = 0.5, length = 5),
  lat = seq(-49.2, by = 1, length = 5)
  )

## ----data 4, echo = FALSE-----------------------------------------------------
  data.frame(
  id = rep(54632, 5),
  date = seq(Sys.time(), by = "12 hours", length.out = 5),
  lc = rep("GL", 5),
  lon = seq(100, by = 0.5, length = 5),
  lat = seq(-55, by = 1, length = 5),
  lonerr = rexp(5, 1 / 0.5),
  laterr = rexp(5, 1 / 1.5)
  )

## ----data 5, echo = FALSE, message=FALSE--------------------------------------
data(ellie, package = "aniMotum")
foo <- sf::st_as_sf(ellie, coords=c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +no_defs") 
foo <- sf::st_transform(foo, crs = "+proj=stere +lat_0=-60 +lon_0=85 +ellps=WGS84 +units=km +no_defs")
head(foo)

## ----data 6, echo = FALSE-----------------------------------------------------
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

## ----fit_ssm, eval=FALSE, message=FALSE---------------------------------------
#  ## format, prefilter and fit Random Walk SSM using a 24 h time step
#  fit <-
#    fit_ssm(
#      x = ellie,
#      model = "rw",
#      time.step = 24
#    )

## ----fit_ssm2, message=FALSE--------------------------------------------------
data(sese2_n)
sese2_n

## ----format_data1, message=FALSE----------------------------------------------
d <- format_data(sese2_n, date = "time", coord = c("longitude", "latitude"))
d

## ----fit_ssm3, eval=FALSE, message=FALSE--------------------------------------
#  fit <- fit_ssm(d,
#                 model = "crw",
#                 time.step = 24)

## ----fit_ssm4, eval=FALSE, message=FALSE--------------------------------------
#  fit <- fit_ssm(sese2_n,
#                 date = "time",
#                 coord = c("longitude","latitude"),
#                 model = "crw",
#                 time.step = 24)

## ----multi-fits, message=FALSE------------------------------------------------
## fit to data with two individuals
fit <- fit_ssm(sese2,
               model = "crw",
               time.step=24,
               control = ssm_control(verbose = 0))

## list fit outcomes for both seals
fit

## ----fit summary fn, message=FALSE--------------------------------------------
summary(fit)

## ----grab estimated locations, message=FALSE----------------------------------
## grab fitted locations
floc <- grab(fit, what = "fitted")
floc[1:5,]

## grab predicted locations in projected form
ploc <- grab(fit, what = "predicted", as_sf = TRUE)
ploc[1:5,]

## ----grab data, message=FALSE-------------------------------------------------
fp.data <- grab(fit, what = "data")
fp.data[1:5,]

## ----fit plot1, eval=FALSE, fig.width=7, fig.height=5-------------------------
#  # plot time-series of the fitted values
#  plot(fit, what = "fitted", type = 1, pages = 1)

## ----fit plot2, eval=FALSE, fig.width=7, fig.height=5-------------------------
#  # plot fitted values as a 2-d track
#  plot(fit, what = "predicted", type = 2, pages = 1)

