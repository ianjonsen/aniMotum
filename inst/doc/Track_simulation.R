## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
require(aniMotum)
require(ggplot2)
require(patchwork)

## ----sim----------------------------------------------------------------------
set.seed(pi) 
sim.rw <- sim(N = 200, model = "rw")
sim.crw <- sim(N = 200, model = "crw", D = 0.5)
sim.mp <- sim(N = 200, model = "mp", sigma_g = 0.3)

## ----plot.sim, eval=FALSE, fig.width=7, fig.height=6--------------------------
#  (plot(sim.rw, type=2) + labs(title="'rw'") | plot(sim.crw, type=2) + labs(title="'crw'")) /
#    (plot(sim.mp, type = 2) + labs(title="'mp'") | plot(sim.mp, type = 1)) +
#    plot_layout(guides = 'collect') &
#    theme(legend.position = 'bottom')

## ----SSM, warning=FALSE, message=FALSE, fig.width=7, fig.height=6-------------
# coerce simulated RW data to format expected by fit_ssm
d <- with(sim.rw, data.frame(id = 1, date, lc, lon, lat))

# fit SSM `rw` model without any speed filtering
fit.rw <- fit_ssm(d, 
                  spdf = FALSE, 
                  model = "rw", 
                  time.step = 12, 
                  control = ssm_control(verbose = 0))

# fit SSM `crw` model
fit.crw <- fit_ssm(d, 
                   spdf = FALSE, 
                   model = "crw", 
                   time.step = 12, 
                   control = ssm_control(verbose = 0))

# extract SSM fitted locations
loc.rw <- grab(fit.rw, "fitted")
loc.crw <- grab(fit.crw, "fitted")

## ----plots, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=5----
#  # compare estimated to true values in y-direction
#  ggplot() +
#    geom_point(data = sim.rw, aes(date, I(y+y.err)), col = "grey60") +       # y-values observed with Argos error
#    geom_point(data = sim.rw, aes(date, y), col = "dodgerblue") +            # true y-values
#    geom_point(data = loc.rw, aes(date, y), cex = 0.7, col = "firebrick") +  # RW SSM fitted y
#    geom_point(data = loc.crw, aes(date, y), cex = 0.4, col = "orange")      # CRW SSM fitted y

## ----sim_fit 1b, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  set.seed(pi)
#  fit <- fit_ssm(sese2,
#                 model="crw",
#                 time.step=24,
#                 control=ssm_control(verbose=0))
#  
#  st <- sim_fit(fit[2,], what="predicted", reps=5)
#  
#  plot(st)

## ----sim_fit potential fn, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  load(system.file("extdata/grad.rda", package = "aniMotum"))
#  grad <- terra::unwrap(grad)
#  
#  set.seed(pi)
#  st.pf <- sim_fit(fit[2, ], what = "predicted", reps=5, grad=grad, beta=c(-300,-300))
#  
#  plot(st.pf)

## ----sim_fit 4, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  st.cpf <- sim_fit(fit[2, ], what = "predicted", reps=5, cpf = TRUE)
#  
#  plot(st.cpf)

## ----sim_filter 1, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  # simulate 50 tracks
#  st <- sim_fit(fit[1,], what = "predicted", reps = 50)
#  
#  # filter, keep only top 20 %
#  st_f <- sim_filter(st, keep = 0.2)
#  
#  # compare unfiltered vs. filtered tracks
#  plot(st) | plot(st_f)

## ----sim_filter 2, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  # simulate 50 cpf tracks
#  st.cpf <- sim_fit(fit[2,], what = "predicted", reps = 50, cpf = TRUE)
#  
#  # filter, keep only top 20 %
#  st.cpf_f <- sim_filter(st.cpf, keep = 0.2)
#  
#  # compare unfiltered vs. filtered tracks
#  plot(st.cpf) | plot(st.cpf_f)

## ----sim_filter 3, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  
#  st.cpf_f1 <- sim_filter(st.cpf, keep = 0.2, var = c("lon","lat"), FUN = "mean")
#  
#  # compare tracks filtered by similarity flag vs mean lon,lat
#  plot(st.cpf_f) | plot(st.cpf_f1)

## ----sim_filter 4, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  chl <- terra::rast("../data-raw/chl.grd")
#  
#  st.cpf.df <- tidyr::unnest(st.cpf, cols = c(sims))
#  
#  ## extract chl values at track locations
#  st.cpf.df <- st.cpf.df |>
#    dplyr::mutate(terra::extract(chl, cbind(lon, lat))) |>
#    dplyr::rename(chl = chl_summer_climatology)
#  
#  ## convert back to nested tibble
#  st.cpf.n <- tidyr::nest(st.cpf.df, sims = c(rep, date, lon, lat, x, y, chl))
#  
#  ## append new nested tibble with correct aniMotum classes so sim_filter works
#  class(st.cpf.n) <- append(class(st.cpf)[1:2], class(st.cpf.n))
#  
#  ## filter based on mean chl values
#  st.cpf.chl <- sim_filter(st.cpf.n, keep = 0.2, var = "chl", FUN = "mean", na.rm = TRUE)
#  
#  ## plot filtered tracks
#  plot(st.cpf.chl)

## ----route_path b, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  # reroute simulated tracks
#  st.cpf_f1rr <- route_path(st.cpf_f1, centroids = TRUE)
#  
#  # compare
#  plot(st.cpf_f1) | plot(st.cpf_f1rr)

## ----route_path a, eval=FALSE, echo=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  if(requireNamespace("pathroutr", quietly = TRUE)) {
#    # reroute simulated tracks
#    st.cpf_f1rr <- route_path(st.cpf_f1, centroids = TRUE)
#  
#    # compare
#    plot(st.cpf_f1) | plot(st.cpf_f1rr)
#  } else {
#    cat("\n the 'pathroutr' pkg is not installed, use remotes::install_github(\"jmlondon/pathroutr\")
#            or install.packages(\"pathroutr\", repos = \"https://jmlondon.r-universe.dev\")to use this function\n")
#  }

## ----sim_post, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  fit <- fit_ssm(sese2, model = "rw", time.step=6, control = ssm_control(verbose = 0))
#  psim <- sim_post(fit[2,], what = "predicted", reps = 100)
#  
#  plot(psim, type = "lines", alpha = 0.05) | plot(psim, type = "both", alpha = 0.05)

