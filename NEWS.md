# aniMotum (development version)

## projection

* new `fit_ssm(projection = )`. `"mercator"` (the default) is unchanged.
`"auto"` chooses a projection from the geographic extent of the data with the
new `auto_crs()`; a proj4 string may also be supplied directly.

* `auto_crs()` chooses a conformal projection sized to the data: a Lambert
conformal conic with standard parallels one sixth and five sixths through the
latitude range for one hemisphere below 80 degrees, polar stereographic beyond
80 degrees or for circumpolar data, oblique stereographic across the equator,
and the existing Mercator grid where the stretch varies by less than 20 percent
and the extremes are below 60 degrees. Conformal projections are chosen because
these models are about movement steps, which have a direction as well as a
length: a projection that distorts shape makes the x and y process variances
differ for reasons unrelated to the animal, and changes move persistence.

* the choice is made once from all individuals together, never per individual.
Fitting each animal in its own projection would leave their parameter estimates
in differently distorted frames and so not comparable across a deployment, and
for `jmp` would make pooling meaningless.

* **an `sf` object is always respected.** aniMotum chooses a projection for
itself only when the data arrive as plain lon,lat. Data prepared and projected
upstream - by `ArgosQC`, for example - reach the model exactly as prepared,
`projection` is ignored with a message, and nothing is ever projected twice.

* new vignette, `Projections`, on what the projection does to parameter
estimates and how to tell whether it matters for your data.

## joint (hierarchical) move persistence model

* new `model = "jmp"` in `fit_ssm()` fits the time-varying move persistence
model to all individual tracks at once, sharing parameters among individuals.
`sigma_g` is pooled by default, which is the point of the model: with an
individual-specific `sigma_g` each animal's `g_t` is smoothed on its own scale
and the series are not comparable between animals. This follows Jonsen et al.
(2019) and the existing `jmpm` model in `fit_mpm()`. The measurement model
(`tau`, `psi`, `rho_o`) is pooled, the process innovation scale is
hierarchical, and `rho_p` is estimated per individual. Written in R using RTMB
rather than as a C++ template.

* `rho_p` is individual by default, deliberately. It sets the tilt of the
process covariance ellipse relative to the projected coordinate axes, so it
records the animal's direction of travel rather than a property of its
movement. Animals heading different ways are not exchangeable in it, and
pooling averages correlations that may differ in sign.

* a joint fit no longer presents itself as several separate fits. `jmp` fits
now carry class `jssm_df`, ahead of `ssm_df` so that `grab()`, `plot()` and
`map()` are unaffected, with each individual's component classed `jmp_ssm`.
`summary()` and `print()` report the objective, convergence and AICc once for
the single fit rather than repeating them against every animal - the previous
behaviour put the joint AICc in each row of the summary table, which invited it
to be summed or compared animal by animal, neither of which is valid. The
parameter tables now separate estimates shared by all individuals from those
estimated separately, so it is clear that `sigma_g` and `tau` are the
population's while `sigma_x` and `rho_p` are that animal's. `grab()` keys on
the first class, so it gained a fall-through for `jssm_df`.

* new `share_control()` specifies which parameters are pooled, hierarchical or
estimated separately, and refuses combinations that are not identifiable. It
will not allow `sigma` and the measurement parameters to be hierarchical at
once, since random effects on both sides of the process/measurement variance
partition trade off against each other. `share_control(group = )` pools the
measurement parameters within strata (tag type, species, deployment).

* `share_control(ho_scale = "pooled")` estimates the haulout process variance
scale factor instead of asserting it. This is only possible in a joint fit.

* `fit_ssm(model = "jmp")` warns when individuals span a wide latitude range on
a Mercator grid, where the `sec(latitude)` scale factor inflates apparent
movement scale and is absorbed into the among-individual variance of `sigma`.
`g_t` and `sigma_g` are unaffected, being scale-free.

* the joint fit uses tightened optimiser tolerances by default. `ssm_control()`'s
defaults (`rel.tol = 1e-3`, `x.tol = 1.5e-2`) are tuned for fast per-individual
quality control and are too loose for a variance parameter in a low-curvature
direction, where they can produce a spurious among-individual variance while
reporting `convergence = 0`.

* new internal `ssm_prep()` factors out the prediction grid, time differences,
gap and haulout flags and state initial values shared by all of the filters.
It is used by the joint filter only for now; `sfilter()` and `mpfilter()`
currently duplicate this logic between them and are intended to migrate onto
`ssm_prep()` once it has been validated against their existing behaviour.

* a joint correlated random walk (`jcrw`) was prototyped and set aside; see
commit "joint (hierarchical) CRW prototype in RTMB - set aside". The diffusion
tensor is frame-dependent, so pooling its components across animals travelling
on different bearings averages incommensurable quantities. A rotation-invariant
reparameterisation is the way back to it.


# aniMotum 1.2-06 (05/06/2024)

* various minor issues fixed including: build against TMB 1.9.11, fixes error when grad specified in `sim_fit()`.


# aniMotum 1.2 (03/11/2023)

* addition of `sim_post()`, a function for posterior simulations from SSM fits, conditional on data and movement parameters
* addition of plot method for `sim_post` objects
* `sim_filter()` can now use arbitrary variables, including user-appended environmental variables, for filtering tracks simulated with `sim_fit()`
* `sim_fit()` now simulates tracks from user-specified `start` and `end` locations that differ from the estimated track start and end.
* `fit_ssm()` now handles "generic location" data provided the locations have `x` and `y` standard errors. These data can be light-level geolocations, acoustic telemetry positions, or other location data. Input data should have `lc = "GL"` for all generic locations.
* a change to the `min.dt` argument in `fit_ssm()`, the default is now `min.dt = 0` (no minimum time interval between observations, but any subsequent observations that occur at the same time are ignored when fitting an SSM).
* fixed an issue with high temporal resolution data, where prediction times exactly match observation times when observations occur 1 s apart. This caused an error when fitting SSM's.


# aniMotum 1.1-06 (13/07/2023)

* fixes issue with `route_path()` where simulated tracks that are entirely on land resulted in an error


# aniMotum 1.1-04 (01/03/2023)

* fixes issue with `grab()` where multiple data sets with `lon` modulo 0,360 resulted in an error
* fixes issue with `route_path()` where rerouting tracks that have no locations on land resulted in an error. In these cases, `route_path()` now returns a tibble identical to that supplied (fitted or predicted locations) and issues a message on the console.


# aniMotum 1.1-02 (13/02/2023)

* update citations to include new aniMotum R package paper in Methods in Ecology and Evolution as the primary reference.


# aniMotum 1.1-01 (01/02/2023)

* fixes issue with map() where function could become paused one some calls.


# aniMotum 1.1 (10/12/2022)

* package name change to coincide with Methods in Ecology and Evolution manuscript: Jonsen et al. `aniMotum`, an R package for animal movement data: rapid quality control, behavioural estimation and simulation. Accepted 06/12/2022.


# foieGras 1.1 (01/12/2022)

* adds `format_data()` to pre-process non-default data formats into that expected by `fit_ssm`
* adds greater flexibility for input data formats (via new arguments to `fit_ssm`), related to `format_data`
* fixes an issue with input data as an `sf-tibble` or `sf-data.frame` that caused an error when fitting SSM's via `fit_ssm`
* when installing package from source, C++ code is automatically compiled against the existing TMB package version. This should eliminate the warning message on installation and package load that some users may have experienced previously
* updated `Overview` vignette, describing input data structures in greater detail


# foieGras 1.0-5 (15/05/2022)

* adds `route_path()` a wrapper function calling [`pathroutr`](https://github.com/jmlondon/pathroutr) to re-route `fit_ssm` estimated or `simfit` simulated tracks around land barriers
* adds a move persistence model via `fit_ssm(model = "mp")` to allow simultaneous estimation of locations and move persistence. This approach may be preferable to using `fit_mpm()` on a `fit_ssm` model object. `fit_mpm()` is retained for less error-prone (GPS) location data
* adds option for move persistence estimates to be normalised to 0,1 in `grab()`, either on tracks separately or as a group for a relative measure that spans 0 - 1. 
* adds `sim_filter()` to calculate similarity between simulated and ssm-estimated tracks, and returns the most similar simulated tracks based on a user-specified quantile
* adds mapping function `map()` to replace `fmap()` for faster, more flexible estimated track maps & fixes to coastline and other mapping issues for tracks that cross -180,180
* adds rosm map tile layers to `map()` via `ggspatial::annotation_map_tile` for more detailed coastlines on large-scale maps
* adds faster `crw` model fitting via `fit_ssm()` by turning off travel rate standard error (s.se) estimation in `ssm_control()` as the default. SE estimation can be turned on via `control = ssm_control(se = TRUE)`.
* adds a `summary` function for displaying information about SSM fits. 
* replaces hcl.colors("Zissou1") palette for most plots - Zissou1 was fun but not colour blind-friendly; in most cases "Cividis" is now the default but users can specify any `hcl.pals()` palette, using the `pal` argument in many of the plot functions.
* fit object s3 classes `fG_ssm`, `fG_mpm` migrated to `ssm_df`, `mpm_df`
* generic plot method s3 classes migrated to `plot.ssm_df`, `plot.mpm_df`, `plot.osar`, `plot.sim`, `plot.simfit`
* adds new vignettes for more comprehensive documentation of package features


# foieGras 0.7-6 (26/04/2021)

* adds fixes to example data for testing/examples so pkg builds on solaris and various linux boxes with older GDAL/PROJ libraries


# foieGras 0.7-5 (23/04/2021)

* adds `sim()` to simulate animal tracks using the `rw`, `crw` or `mpm` process models. The `rw` and `crw` models can also be specified with state-switching between multiple behavioural states. Tracks can be simulated with or without Argos (LS or KF) errors, as time-regular or time-irregular
* adds `simfit()` to simulate animal tracks from `fit_ssm` fit objects
* adds generic `plot()` methods for `sim` and `simfit` objects
* adds `fit_mpm` can take a `fit_ssm` object directly as input, removing need for user to manipulate data prior to calling `fit_mpm`
* adds `fit_mpm` can fit to SSM-`predicted` (time regular) or SSM-`fitted` (time irregular) locations, via `what` argument
* adds `fit_mpm` can fit to lon,lat or x,y coordinates, via `coords` argument
* adds `ssm_control()` for centralized control over optimizer and optimization method choices, optimizer parameters, and `foieGras` model parameter bounds
* adds faster and more stable optimization
* adds time-series and `acf()` plots as option when visualising prediction residuals calculated from `osar()`
* removes `hist` plots as option when visualising prediction residuals calculated from `osar()`
* removes (with deprecation errors) `verbose`, `optim`, `optMeth`, and `lpsi` arguments to `fit_ssm`
* replaces wesanderson::wes_palette("Zissou1") with hcl.colors("Zissou1") and provides arguments to change this default palette


# foieGras 0.6-9

* adds `wesanderson::wes_palette("Zissou1")` as default palette for plots/maps
* generic plot method for `fG_ssm` objects can now plot individuals all on 1 page (`pages = 1`) or on separate pages (`pages = 0`)
* 2-d (track) plots now include confidence ellipses on estimated locations
* generic plot method for `fG_mpm` objects now available
* mapping function (`fmap`) can optionally take an `fG_mpm` object to colour locations by behavioural index (`gamma_t`)


# foieGras 0.6-7

* adds ability to fit move persistence models to temporally regular OR irregular location data
* adds ability to turn off predicted locations by setting `time.step=NA` causing locations to be estimated only at observation times
* adds ability to fit to Argos - GPS or Argos - GLS locations simultaneously when combined in a single input data.frame
* adds ability to specify an alternate emf (Error Multiplication Factor) `data.frame` for Least-Squares and/or GPS locations
* adds human-readable/understandable errors and warnings with (occasional) advice
* turns off estimation of psi parameter (ellipse semi-minor axis re-scaling factor) when `rw` process model is fit to Argos KF/KS data
* removes reliance on `dplyr::do`, which is superseded as of `dplyr 1.0.0`
* replaces dplyr progressbar with parameter trace when fitting SSM to single or multiple data sets in default verbose mode 1
* replaces `argosfilter::sdafilter` in favour of `trip::sda` (which is a faster, vectorized version of the former) to prefilter outlier locations


# foieGras 0.4.0

* adds move persistence models for behavioural inference along animal tracks, via `fit_mpm()`
* adds ability to fit SSM's to processed light-level geolocation or GPS data by adding `lonerr, laterr` variables to input data
* adds One-Step-Ahead prediction residuals for evaluating SSM fits, via `osar()` and generic `plot()` method for `osar` output
* adds diagnostic plot functions that handle multi-individual fits, via generic `plot()` method for `fG_ssm` fit objects
* adds `sf`-enabled mapping function, via `fmap()`
* fixes CRAN check errors from last version, where proj4string syntax `+init:epsg=` was not supported in non-PROJ4 emulation mode on some linux platforms


# foieGras 0.2.2

* patch for compatibility with latest sf version


# foieGras 0.2.1

* patch to improve C++ portability


# foieGras 0.2.0

* first release, updates will follow regularly and be documented here





