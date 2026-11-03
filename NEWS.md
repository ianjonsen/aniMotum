# aniMotum 1.3-0 (03/11/2026)

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





