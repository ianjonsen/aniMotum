## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(foieGras)
library(dplyr)
library(ggplot2)
library(sf)

## ----data 1, echo = FALSE------------------------------------------------
data(ellie, package = "foieGras")
head(ellie)

## ----data 2, echo = FALSE------------------------------------------------
data(ellies, package = "foieGras")
head(ellies)

## ----data 3, echo = FALSE------------------------------------------------
data(ellie, package = "foieGras")
ellie[3:5, c("smaj","smin","eor")] <- NA
head(ellie)

## ----data 4, echo = FALSE, message=FALSE---------------------------------
data(ellie, package = "foieGras")
foo <- sf::st_as_sf(ellie, coords=c("lon","lat"), crs = 4326) 
foo <- sf::st_transform(foo, crs = "+init=epsg:3031 +units=km")
head(foo)

## ----data 5, echo = FALSE------------------------------------------------

  tibble(
  id = rep(54632, 5),
  date = seq(Sys.time(), by = "12 hours", length.out = 5),
  lc = rep("G", 5),
  lon = seq(100, by = 0.5, length = 5),
  lat = seq(-55, by = 1, length = 5),
  lonerr = rexp(5, 1 / 0.5),
  laterr = rexp(5, 1 / 1.5)
  )

## ----fit_ssm, message=FALSE----------------------------------------------
## load foieGras example data
data(ellie)
## prefilter and fit Random Walk SSM using a 24 h time step
fit <- fit_ssm(ellie, model = "rw", time.step = 24, verbose = 0)

## ----fit summary---------------------------------------------------------
fit$ssm[[1]]

## ----fit plot, fig.width=7,fig.height=5----------------------------------
# plot time-series of the predicted values
plot(fit, what = "predicted", type = 1)
plot(fit, what = "fitted", type = 2)

## ----osar comp, fig.width=7, fig.height=5, message=FALSE-----------------

## fit crw SSM
fitc <- fit_ssm(ellie, model = "crw", time.step = 24, verbose = 0)

## calculate OSA resids for both models
fit_res <- osar(fit)
fitc_res <- osar(fitc)

## plot residuals
plot(fit_res)
plot(fitc_res)

## ----ggplot map, fig.width=5, fig.height=5, message=FALSE----------------
## map ssm-predicted values without observations
fmap(fitc, what = "predicted", obs = FALSE)

## change projection to Antarctic Polar Stereographic centred on 
##  the approximate mid-point of the track
fmap(fitc, what = "predicted", crs = "+init=epsg:3031 +lon_0=85")

## ----grab----------------------------------------------------------------
## grab fitted locations from fit object as a projected sf object 
plocs_sf <- grab(fitc, what = "f")

## grab predicted locations in unprojected form, returning as a tibble
plocs <- grab(fitc, "p", as_sf = FALSE)

## unprojected form looks like this
plocs

## ----multi-fits----------------------------------------------------------
# load 2 southern elephant seal example data
data(ellies)

fit2 <- fit_ssm(ellies, vmax = 10, model = "crw", time.step = 48, verbose = 0)

# list fit outcomes for both seals
fit2

## ----ggplot map 2, fig.width=5, fig.height=5, message=FALSE--------------
## map predicted values and observations
fmap(fit2, "p", obs = TRUE)

## ----fit mpm, message=FALSE----------------------------------------------
## fit mpm separately to each individual track
fmp <- fit2 %>% 
  grab(., "p", as_sf = FALSE) %>%
  select(id, date, lon, lat) %>%
  fit_mpm(., model = "mpm")

fmp

## ----plot mpm ts 1, fig.width=7, fig.height=5, message=FALSE-------------
## plot mpm estimates by individual seal
grab(fmp, "fitted") %>% 
  ggplot() +
  geom_point(aes(date, g, colour = g)) +
  scale_colour_viridis_c(limits = c(0,1)) +
  ylim(0,1) +
  facet_wrap(~ id, scales = "free_x", ncol = 1)


## ----plot mpm track, fig.width=7, fig.height=5, message=FALSE------------

## join ssm predicted locations and move persistence values together
fmp_locs <- join(fit2, fmp, as_sf = FALSE)

ggplot(fmp_locs) +
  geom_point(aes(lon, lat, colour = g)) +
  scale_colour_viridis_c(limits = c(0,1))

