## ---- include = FALSE---------------------------------------------------------
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
#  plot(st, zoom=TRUE)

## ----sim_fit potential fn, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  load(system.file("extdata/grad.rda", package = "aniMotum"))
#  set.seed(pi)
#  st.pf <- sim_fit(fit[2, ], what = "predicted", reps=5, grad=grad, beta=c(-350,-350))
#  
#  plot(st.pf, zoom=TRUE)

## ----sim_fit 4, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  st.cpf <- sim_fit(fit[2, ], what = "predicted", reps=5, cpf = TRUE)
#  
#  plot(st.cpf, zoom=TRUE)

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

## ----route_path b, eval=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  # reroute simulated tracks
#  st.cpf_frr <- route_path(st.cpf_f, centroids = TRUE)
#  
#  # compare
#  plot(st.cpf_f, zoom=TRUE) | plot(st.cpf_frr, zoom=TRUE)

## ----route_path a, eval=FALSE, echo=FALSE, warning=FALSE, message=FALSE, fig.width=7, fig.height=6----
#  if(requireNamespace("pathroutr", quietly = TRUE)) {
#    # reroute simulated tracks
#    st.cpf_frr <- route_path(st.cpf_f, centroids = TRUE)
#  
#    # compare
#    plot(st.cpf_f, zoom=TRUE) | plot(st.cpf_frr, zoom=TRUE)
#  } else {
#    cat("\n the 'pathroutr' pkg is not installed, use remotes::install_github(\"jmlondon/pathroutr\")
#            or install.packages(\"pathroutr\", repos = \"https://jmlondon.r-universe.dev\")to use this function\n")
#  }

