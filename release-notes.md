
* adds a joint (hierarchical) move persistence SSM via `fit_ssm(model = "jmp")`, fitting the time-varying move persistence model to all tracks at once (Jonsen et al. 2019). By default, `sigma_g` and the measurement error parameters are pooled, the process variance is hierarchical, and `rho_p` is estimated separately for each individual
* adds a rotation-invariant joint correlated random walk SSM via `fit_ssm(model = "jcrw")`. Movement scale and anisotropy are shared among individuals, while each individual keeps its own orientation. Anisotropy is set via `share_control(aniso = )`
* adds `share_control()` to specify which parameters are pooled, hierarchical or individual-specific in joint fits. Combinations that are not identifiable are refused
* adds `sim_jcrw()` to simulate tracks from the `jcrw` process model with known parameters
* joint fits have class `jssm_df`. `summary()` and `print()` report convergence and AICc once for the joint fit, and separate parameter estimates shared by all individuals from those estimated per individual
* adds a `projection` argument to `fit_ssm()`. The default `"mercator"` is unchanged, `"auto"` chooses a conformal projection suited to the data extent via the new `auto_crs()`, or a proj4 string can be supplied. The projection is chosen once for all individuals
* `fit_ssm()` now always respects the projection of an input `sf` object and never re-projects it
* adds a `Projections` vignette, describing how the choice of projection affects parameter estimates
* adds `smru_haulout()` to flag haulout periods (`ho = 1`) in tracking data, using an SMRU haulout data file
* adds `haulout`, `ho.ref` and `ho.id_fun` arguments to `fit_ssm()`, calling `smru_haulout()` before fitting. An existing `ho` column in the input data is also used
* the `rw`, `crw` and `mp` models now keep locations nearly stationary during haulouts, by scaling the process variance with `ssm_control(ho_scale = )` (default 0.01)
* adds `ssm_control(gap.thresh = )` to flag data gaps longer than `gap.thresh` hours. The `crw` and `mp` models break directional persistence across flagged gaps, which stops implausible looping by `crw` tracks across large gaps. No gaps are flagged by default
* `route_path()` no longer moves locations known to be on land: haulout locations (`ho = 1`), locations within intervals supplied via the new `onland` argument, and on-land locations at track ends. All locations are returned, with `on_land = TRUE` where kept on land
* `sim_fit()` now simulates tracks in the projection used to fit the SSM, rather than assuming Mercator. This fixes simulations from fits to projected `sf` data and from tracks that cross -180,180 (#76)
* `sim_post()` now samples more efficiently, using far less memory for long tracks (Jan-Ole Koslik, #85), supports joint fits, and no longer assumes a Mercator projection
* fixes issue with `osar()` where residuals were assigned to the wrong individuals when they could not be calculated for some individuals (#91)
* fixes issue with `fit_mpm(model = "jmpm")` where tracks with numeric ids and unequal lengths resulted in an error. Also fixes `fit_mpm()` dropping `id` and `date` when coordinates are named `x`, `y`, and `grab()` failing on crashed `jmpm` fits (#90)
* fixes issue with `fit_ssm(model = "mp")` where `tau` was estimated for "GL" locations (#82)
* fixes issue with `fit_ssm()` where all columns but geometry were dropped from `sf` input data when `spdf = FALSE`
* fixes issue with `ellp.par()` where simulated location errors did not match their error ellipses
* fixes issue with `sim_fit()` where simulations used fitted locations when `what = "predicted"` was specified, and `rw` tracks did not return to their start when `cpf = TRUE`
* removes `ggplot2` warnings about empty axis labels in plot functions


